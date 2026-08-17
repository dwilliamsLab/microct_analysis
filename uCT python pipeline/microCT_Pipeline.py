# environment and configuration
import os
import numpy as np
import tifffile
import SimpleITK as sitk
import matplotlib.pyplot as plt
import argparse
from scipy import ndimage
from skimage.filters import threshold_otsu

# checkpoint visualization helper
def get_checkpoint_slice_indices(n_z, n_slices=10):
    '''evenly spaced interior slice indices through a stack of depth n_z'''
    if n_slices <= 0:
        raise ValueError('n_slices must be >= 1')
    fractions = np.linspace(0, 1, n_slices + 2)[1:-1]
    z_idx = np.round(fractions * (n_z - 1)).astype(int)
    z_idx = np.clip(z_idx, 0, n_z - 1)
    # de-dupe (thin stacks) while preserving order
    # don't keep the values, keep the indices so we know where they were
    _, unique_order = np.unique(z_idx, return_index=True)
    keep = np.sort(unique_order)
    return z_idx[keep], fractions[keep]

# panels is a dictionary
def show_checkpoint(panels, title='', n_slices=10, figsize_per_slice=4.0):
    '''standard checkpoint visualization used throughout the pipeline'''
    n_z = panels[0]['volume'].shape[0]
    z_indices, fractions = get_checkpoint_slice_indices(n_z, n_slices)
    n_rows = len(panels)
    n_cols = len(z_indices)
    fig, axes = plt.subplots(
        n_rows, n_cols,
        figsize=(figsize_per_slice * n_cols, figsize_per_slice * n_rows),
        squeeze=False,
    )
    for row, panel in enumerate(panels):
        vol = panel['volume']
        overlays = panel.get('overlays', [])
        panel_title = panel.get('panel_title', '')
        for col, z in enumerate(z_indices):
            ax = axes[row, col]
            ax.imshow(vol[z], cmap='gray')
            #overlays is an array
            for ov in overlays:
                m = ov['mask']
                if z < m.shape[0]:
                    ax.imshow(m[z], cmap=ov.get('cmap', 'Reds'), alpha=ov.get('alpha', 0.4))
            frac_pct = int(round(fractions[col] * 100))
            subtitle = f'z={z} (~{frac_pct}%)'
            if panel_title:
                subtitle = f'{panel_title}\n{subtitle}'
            ax.set_title(subtitle, fontsize=9)
            ax.axis('off')
    fig.suptitle(title, fontsize=13)
    plt.tight_layout()
    plt.show()
    return z_indices

def _otsu_threshold(values, n_bins=256):
    '''otsu threshold: bin edge maximizing between-class variance'''
    values = np.asarray(values, dtype=np.float64)
    vmin, vmax = float(values.min()), float(values.max())
    if vmax <= vmin:
        return vmin
    hist, edges = np.histogram(values, bins=n_bins, range=(vmin, vmax))
    hist = hist.astype(np.float64)
    bin_centers = 0.5 * (edges[:-1] + edges[1:])
    total = hist.sum()
    cum_weight = np.cumsum(hist)
    cum_mean = np.cumsum(hist * bin_centers)
    # between-class variance for split after bin i
    with np.errstate(divide='ignore', invalid='ignore'):
        w1 = cum_weight / total
        w2 = 1.0 - w1
        mean1 = np.where(cum_weight > 0, cum_mean / np.maximum(cum_weight, 1), 0.0)
        mean2_num = (cum_mean[-1] - cum_mean)
        mean2_den = np.maximum(total - cum_weight, 1)
        mean2 = np.where((total - cum_weight) > 0, mean2_num / mean2_den, 0.0)
        between_var = w1 * w2 * (mean1 - mean2) ** 2
    between_var = np.nan_to_num(between_var, nan=0.0)
    best_i = int(np.argmax(between_var))
    return float(bin_centers[best_i])

def remove_speckles(mask_arr_in, min_voxels=200, keep_largest_n=None):
    labeled, n = ndimage.label(mask_arr_in.astype(bool), structure=np.ones((3, 3, 3)))
    if n == 0:
        return mask_arr_in
    sizes = ndimage.sum(mask_arr_in, labeled, range(1, n + 1))
    if keep_largest_n is not None:
        keep_labels = np.argsort(sizes)[::-1][:keep_largest_n] + 1
    else:
        keep_labels = np.where(sizes >= min_voxels)[0] + 1
    cleaned = np.isin(labeled, keep_labels)
    print(f"Speckle removal: {n} components found, kept {len(keep_labels)}, "
          f"removed {int(mask_arr_in.sum() - cleaned.sum())} voxels")
    return cleaned.astype(np.uint8)

def crop_to_bbox(img, bbox):
    '''crop image to (x0,y0,z0,sx,sy,sz), keeping origin/direction aligned'''
    bx0, by0, bz0, bsx, bsy, bsz = bbox
    return img[bx0:bx0 + bsx, by0:by0 + bsy, bz0:bz0 + bsz]

##############
# Start main #
##############

def main(tiff_path, output_dir):
    #set up the environment and configuration
    #TIFF_PATH = '/projects/standard/drakew/shared/drake/uct-testing/tiffs/Tiff3'  # TODO: update per sample
    #TIFF_PATH = './tiffs/Tiff2'
    TIFF_PATH = tiff_path
    print("Entered tiff path: " + TIFF_PATH)
    IS_DIRECTORY = True  # True = directory of slices, False = single multipage TIFF
    # this is for doing multiple samples (TRUE) or just a single sample (FALSE)
    # voxel spacing (mm), update to match your scanner
    # Change 1: add voxel_space variable 
    voxel_space = 0.00554999
    spacing_full = (voxel_space, voxel_space, voxel_space)  # 40 um isotropic example
    # dilation margin (mm); used by 5.6 ROI, Section 8 volume, and NRRD export
    dilation_mm = 0.25
    # --- ring artifact removal (air/water boundary method) ---
    # finds true air from the image border, treats anything within a fixed
    # physical distance of air as ring (no brightness test)
    AIR_BORDER_WIDTH_VOX = 3             # border frame width (vox) for air level estimate
    RING_PROFILE_MAX_SLICES = 200        # slices sampled for air/water levels
    WATER_UPPER_EXCLUDE_PERCENTILE = 80  # excludes brightest tail from water baseline
    AIR_THRESHOLD_FRACTION = 0.01        # cutoff between air-class/non-air-class means; lower if air mask picks up water, raise if it misses air
    AIR_OPENING_VOX = 5                  # opening to break noise-driven bridges before labeling
    RING_REMOVAL_DILATION_MM = 0.1       # margin (mm) removed inward from air; raise if ring remains, lower if real tissue is clipped
    # ring excluded from growth by location masking only -- pixels never modified
    # --- bone thresholding ---
    # band derived from the tooth mask's own intensity range (self-calibrating)
    BONE_LOWER_PERCENTILE = 10   # 0/100 = literal min/max of tooth-mask intensities
    BONE_UPPER_PERCENTILE = 90   # narrow (e.g. 1/99) if outliers skew the band
    # --- checkpoint visualization ---
    DEFAULT_N_CHECKPOINT_SLICES = 10  # e.g. 3 -> slices at ~25%, 50%, 75%
    # --- NRRD export ---
    #SAMPLE_NAME = os.path.basename(os.path.normpath(TIFF_PATH))
    SAMPLE_NAME = os.path.basename(
        os.path.dirname(
            os.path.dirname(
                os.path.normpath(TIFF_PATH)
            )
        )
    ).split(" [")[0]
    print("Sample_NAME: " + SAMPLE_NAME)
    # NRRD_OUTPUT_DIR = './nrrd_exports'
    NRRD_OUTPUT_DIR = output_dir
    print("Output directory: " + output_dir)
    # TODO: output path, done as input argument 2
    # gaussian sigma (mm) to smooth mask distance map before re-threshold
    MASK_SMOOTHING_SIGMA_MM = 0.04
    # erosion (vox) for 5.7 refinement seed
    REFINE_TOOTH_SEED_ERODE_VOX = 2

    # 1. Load full-resolution stack
    if IS_DIRECTORY:
        slice_files = sorted([
            os.path.join(TIFF_PATH, f)
            for f in os.listdir(TIFF_PATH) if f.lower().endswith('.tif')
        ])
        stack_full = tifffile.imread(slice_files)  # (Z, Y, X)
    else:
        stack_full = tifffile.imread(TIFF_PATH)
    print("Full-res stack shape:", stack_full.shape)
    img_full = sitk.GetImageFromArray(stack_full)
    img_full.SetSpacing(spacing_full)
    # Checkpoint 1: after loading
    _ = show_checkpoint(
        panels=[{"volume": stack_full, "panel_title": "Raw loaded stack"}],
        title="Checkpoint 1 / 7 -- after loading",
    )

    # 2. normalize intensities (zero mean, unit variance) for cross-scan consistency
    normalize_filter = sitk.NormalizeImageFilter()
    img_normalized = normalize_filter.Execute(img_full)

    norm_arr = sitk.GetArrayFromImage(img_normalized).astype(np.float32)  # (Z, Y, X)
    sample_vals = norm_arr[::4, ::4, ::4].ravel()

    plt.figure(figsize=(6, 4))
    plt.hist(sample_vals, bins=100)
    plt.title('Normalized intensity histogram (subsampled)')
    plt.xlabel('Normalized intensity (z-score)')
    plt.ylabel('Voxel count')
    plt.show()

    # Checkpoint 2: after normalization
    _ = show_checkpoint(
        panels=[{"volume": norm_arr, "panel_title": "Normalized stack"}],
        title="Checkpoint 2 / 7 -- after normalization",
        )    
    # Change 2: check that the normalization worked
    # Original image as NumPy array
    original_arr = stack_full.astype(np.float32)

    # Normalized image
    normalized_arr = norm_arr

    # Compare statistics of before and after the normalization
    # normalized mean should be ~0 and standard deviation should be 1

    print("========== ORIGINAL ==========")
    print("Shape:", original_arr.shape)
    print("Mean:", original_arr.mean())
    print("Std:", original_arr.std())
    print("Min:", original_arr.min())
    print("Max:", original_arr.max())

    print("\n======= NORMALIZED =========")
    print("Shape:", normalized_arr.shape)
    print("Mean:", normalized_arr.mean())
    print("Std:", normalized_arr.std())
    print("Min:", normalized_arr.min())
    print("Max:", normalized_arr.max())

    # 3. Ring artifact removal
    # 3a. estimate air/water reference intensity levels
    nz, ny, nx = norm_arr.shape

    # subsample slices to estimate global air/water levels
    n_profile_slices = min(RING_PROFILE_MAX_SLICES, nz)
    profile_z = np.unique(np.linspace(0, nz - 1, n_profile_slices).round().astype(int))

    w = AIR_BORDER_WIDTH_VOX
    border_mask_2d = np.zeros((ny, nx), dtype=bool)
    border_mask_2d[:w, :] = True
    border_mask_2d[-w:, :] = True
    border_mask_2d[:, :w] = True
    border_mask_2d[:, -w:] = True

    border_vals_pooled = np.concatenate([norm_arr[z][border_mask_2d] for z in profile_z])
    air_threshold_otsu = _otsu_threshold(border_vals_pooled)

    # refine otsu split using AIR_THRESHOLD_FRACTION between class means
    _air_class_vals = border_vals_pooled[border_vals_pooled <= air_threshold_otsu]
    _nonair_class_vals = border_vals_pooled[border_vals_pooled > air_threshold_otsu]
    _air_class_mean = float(_air_class_vals.mean()) if _air_class_vals.size else float(border_vals_pooled.min())
    _nonair_class_mean = float(_nonair_class_vals.mean()) if _nonair_class_vals.size else float(border_vals_pooled.max())
    air_threshold = _air_class_mean + AIR_THRESHOLD_FRACTION * (_nonair_class_mean - _air_class_mean)
    air_frac_at_border = float((border_vals_pooled <= air_threshold).mean())

    # water baseline: median of non-air voxels, excluding brightest tail
    interior_sample_vals = np.concatenate([norm_arr[z].ravel() for z in profile_z[::max(1, len(profile_z) // 40)]])
    non_air_vals = interior_sample_vals[interior_sample_vals > air_threshold]
    upper_cut = np.percentile(non_air_vals, WATER_UPPER_EXCLUDE_PERCENTILE)
    water_vals = non_air_vals[non_air_vals <= upper_cut]
    water_level = float(np.median(water_vals))

    print("=== Air/water reference-level diagnostics ===")
    print(f"Sampled {len(profile_z)} of {nz} slices ({w}-vox border frame) to estimate air/water levels")
    print(f"Otsu split on border-pooled values (initial estimate, NOT used directly): {air_threshold_otsu:.4f}")
    print(f"Air-class mean: {_air_class_mean:.4f}  |  Non-air-class mean: {_nonair_class_mean:.4f}")
    print(f"Final air threshold (AIR_THRESHOLD_FRACTION={AIR_THRESHOLD_FRACTION} of the way from air-class mean to "
          f"non-air-class mean): {air_threshold:.4f}")
    print(f"Fraction of border voxels classified as air: {air_frac_at_border * 100:.1f}%")
    print("If the air mask in Checkpoint 3 is still picking up water, LOWER AIR_THRESHOLD_FRACTION (pulls the cutoff closer to the "
          "air-class mean); if it's missing real air near the border, RAISE it.")
    if air_frac_at_border < 0.01:
        print("WARNING: near-zero air detected at the border across sampled slices. This likely means the "
              "crop cut off before reaching true air everywhere in this sample -- in which case there is no "
              "ring artifact to remove (nothing will be masked in 3b), which is the correct behavior.")
    print(f"Water/ethanol level (median, excluding top {100 - WATER_UPPER_EXCLUDE_PERCENTILE}% brightest): {water_level:.4f} "
          "(voxels removed as ring in 3b are replaced with this value)")

    # 3b. identify ring from one representative 2D slice, reuse for whole stack 
    struct_2d = ndimage.generate_binary_structure(2, 2)  # 8-connectivity
    spacing_xy = img_normalized.GetSpacing()[0]
    ring_removal_dilation_vox = max(1, int(np.ceil(RING_REMOVAL_DILATION_MM / spacing_xy)))

    # representative slice: per-pixel median across sampled slices
    reference_2d = np.median(norm_arr[profile_z], axis=0)

    # air: low-intensity pixels connected to border, opened to break noise bridges
    low_2d_raw = reference_2d <= air_threshold
    low_2d = ndimage.binary_opening(low_2d_raw, structure=struct_2d, iterations=AIR_OPENING_VOX)
    labeled_low, n_low = ndimage.label(low_2d, structure=struct_2d)
    if n_low > 0:
        border_labels = np.unique(np.concatenate([
            labeled_low[0, :], labeled_low[-1, :], labeled_low[:, 0], labeled_low[:, -1],
        ]))
        border_labels = border_labels[border_labels > 0]
    else:
        border_labels = np.array([], dtype=int)
    air_2d = np.isin(labeled_low, border_labels) if border_labels.size else np.zeros_like(low_2d)

    # diagnostic only: air region size before opening
    _labeled_low_raw, _n_low_raw = ndimage.label(low_2d_raw, structure=struct_2d)
    if _n_low_raw > 0:
        _raw_border_labels = np.unique(np.concatenate([
            _labeled_low_raw[0, :], _labeled_low_raw[-1, :], _labeled_low_raw[:, 0], _labeled_low_raw[:, -1],
        ]))
        _raw_border_labels = _raw_border_labels[_raw_border_labels > 0]
        air_2d_unopened_px = int(np.isin(_labeled_low_raw, _raw_border_labels).sum()) if _raw_border_labels.size else 0
    else:
        air_2d_unopened_px = 0

    # ring: voxels within fixed distance of air, minus air itself
    if air_2d.any():
        dilated_air_2d = ndimage.binary_dilation(air_2d, structure=struct_2d, iterations=ring_removal_dilation_vox)
        ring_2d = dilated_air_2d & ~air_2d
    else:
        ring_2d = np.zeros_like(air_2d)

    # broadcast 2D mask to every slice (tube position constant along z)
    air_mask_arr = np.broadcast_to(air_2d, (nz, ny, nx)).copy()
    ring_mask_arr = np.broadcast_to(ring_2d, (nz, ny, nx)).copy()

    # location-only mask; downstream steps exclude by location, pixels unmodified
    ring_mask_img = sitk.GetImageFromArray(ring_mask_arr.astype(np.uint8))
    ring_mask_img.CopyInformation(img_normalized)

    print("=== Ring removal diagnostics: fixed-distance dilation from air (v8) ===")
    print(f"Ring removal margin: {RING_REMOVAL_DILATION_MM} mm = {ring_removal_dilation_vox} vox (spacing: {spacing_xy:.4f} mm/vox)")
    print(f"Representative slice: per-pixel median across the {len(profile_z)} slices sampled for the reference levels above")
    print(f"Air detected in representative slice: {'yes' if air_2d.any() else 'no'} ({int(air_2d.sum())} vox after opening; "
          f"{air_2d_unopened_px} vox before opening)")
    if air_2d_unopened_px > int(air_2d.sum()) * 2 and air_2d.sum() > 0:
        print(f"NOTE: the un-opened border-connected region was {air_2d_unopened_px / max(int(air_2d.sum()), 1):.1f}x larger than the "
              "final air mask -- this is the signature of noise-driven percolation through the water/ethanol fill being caught and "
              "corrected by AIR_OPENING_VOX. If the final air mask still looks too large/small on Checkpoint 3 below, adjust "
              "AIR_OPENING_VOX (larger = more aggressive at breaking noise bridges, but can also erase a genuinely thin air gap).")
    if air_2d.any():
        print(f"Ring voxels removed (fixed {RING_REMOVAL_DILATION_MM}mm band around air, one 2D mask reused for all {nz} slices): "
              f"{int(ring_2d.sum())}")
        print("This removal is now purely geometric (distance from air), not brightness-based -- it cannot be fooled by real bone/tooth "
              "happening to be bright, and it cannot run away indefinitely into real tissue. If Checkpoint 3 below still shows a visible "
              "ring remnant, raise RING_REMOVAL_DILATION_MM; if it shows real tissue being clipped near the FOV edge, lower it.")
    else:
        print("No air detected touching the border in the representative slice -- either this sample's crop never reaches true "
              "air (nothing to remove), or the reference slice is unusually noisy; if a ring is visible in Checkpoint 3's 'before' "
              "panel but nothing was found here, consider lowering AIR_BORDER_WIDTH_VOX, adjusting RING_PROFILE_MAX_SLICES to sample "
              "different slices, or checking a single raw slice manually.")
    print(f"Total ring voxels masked across the full stack (same mask broadcast to every slice): {int(ring_mask_arr.sum())} "
          f"({int(ring_2d.sum())} per slice x {nz} slices)")
    print(f"Total air voxels found (same mask broadcast to every slice): {int(air_mask_arr.sum())} (not suppressed, used only for ring adjacency)")

    # Checkpoint 3: air/ring location masks
    _ = show_checkpoint(
        panels=[
            {
                "volume": norm_arr,
                "overlays": [
                    {"mask": air_mask_arr, "cmap": "Greens", "alpha": 0.35, "label": "air mask"},
                    {"mask": ring_mask_arr, "cmap": "Blues", "alpha": 0.5, "label": "ring mask"},
                ],
                "panel_title": "Normalized + detected \n air/ring  masks (location\n only, no pixels modified)",
            },
        ],
        title="Checkpoint 3 / 7 -- ring \n location identified (no suppression)",
    )

    # 4. detect enamel seeds from brightest non-ring voxels
    seed_percentile = 99.7  # TODO: tune; higher = fewer, brighter seed voxels
    seed_cut = np.percentile(norm_arr[~ring_mask_arr], seed_percentile)

    # seeds must be bright and outside ring location
    enamel_seed_arr = (norm_arr > seed_cut) & ~ring_mask_arr

    enamel_seed_img = sitk.GetImageFromArray(enamel_seed_arr.astype(np.uint8))
    enamel_seed_img.CopyInformation(img_normalized)

    # Clean seed regions
    seed_cc = sitk.ConnectedComponent(enamel_seed_img)
    seed_stats = sitk.LabelShapeStatisticsImageFilter()
    seed_stats.Execute(seed_cc)

    min_seed_voxels = 50  # TODO: tune based on voxel size and enamel size
    seed_labels = [l for l in seed_stats.GetLabels() if seed_stats.GetNumberOfPixels(l) >= min_seed_voxels]

    # handle no-seeds case explicitly (ChangeLabel no-ops on empty map)
    if seed_labels:
        enamel_seeds_img = sitk.ChangeLabel(seed_cc, {l: 1 for l in seed_labels})
        enamel_seeds_img = sitk.BinaryThreshold(enamel_seeds_img, 1, 1, 1, 0)
    else:
        enamel_seeds_img = sitk.Image(seed_cc.GetSize(), sitk.sitkUInt8)
        enamel_seeds_img.CopyInformation(seed_cc)
    enamel_seeds_arr = sitk.GetArrayFromImage(enamel_seeds_img)

    print("Enamel seed cutoff (normalized intensity, computed over non-ring voxels only):", float(seed_cut))
    print("Number of enamel seed regions kept:", len(seed_labels))
    print("Seed voxel count:", int(enamel_seeds_arr.sum()))
    
    # Checkpoint 4: after enamel seed detection
    _ = show_checkpoint(
        panels=[{
            "volume": norm_arr,
            "overlays": [{"mask": enamel_seeds_arr, "cmap": "Reds", "alpha": 0.5, "label": "enamel seeds"}],
            "panel_title": "Enamel seeds (ring \n locations excluded, \npixels unmodified)",
        }],
        title="Checkpoint 4 / 7 -- \n after enamel seed detection",
    )

    # 5. grow tooth via watershed (crop -> downsample -> watershed -> upsample -> clean)
    # --- CROP AROUND SEEDS ---
    seed_idx_all = np.argwhere(enamel_seeds_arr > 0)
    if seed_idx_all.size == 0:
        raise RuntimeError("No enamel seeds found in enamel_seeds_arr.")

    margin_mm = 1  # crop margin (mm)
    margin_vox = int(round(margin_mm / spacing_full[0]))  # spacing_full ≈ 0.00555 mm/voxel

    z0 = max(0, seed_idx_all[:, 0].min() - margin_vox)
    z1 = min(norm_arr.shape[0], seed_idx_all[:, 0].max() + margin_vox)
    y0 = max(0, seed_idx_all[:, 1].min() - margin_vox)
    y1 = min(norm_arr.shape[1], seed_idx_all[:, 1].max() + margin_vox)
    x0 = max(0, seed_idx_all[:, 2].min() - margin_vox)
    x1 = min(norm_arr.shape[2], seed_idx_all[:, 2].max() + margin_vox)

    norm_crop = norm_arr[z0:z1, y0:y1, x0:x1]
    ring_crop = ring_mask_arr[z0:z1, y0:y1, x0:x1]
    air_crop = air_mask_arr[z0:z1, y0:y1, x0:x1]
    seeds_crop = enamel_seeds_arr[z0:z1, y0:y1, x0:x1]

    print("Watershed crop shape:", (z1 - z0, y1 - y0, x1 - x0), "of full", norm_arr.shape)

    # --- DOWNSAMPLE FOR WATERSHED ---
    WATERSHED_STRIDE = 4  # larger = faster, coarser

    norm_ds = norm_crop[::WATERSHED_STRIDE, ::WATERSHED_STRIDE, ::WATERSHED_STRIDE]
    ring_ds = ring_crop[::WATERSHED_STRIDE, ::WATERSHED_STRIDE, ::WATERSHED_STRIDE]
    air_ds = air_crop[::WATERSHED_STRIDE, ::WATERSHED_STRIDE, ::WATERSHED_STRIDE]
    seeds_ds = seeds_crop[::WATERSHED_STRIDE, ::WATERSHED_STRIDE, ::WATERSHED_STRIDE]
    print("Watershed downsampled shape:", norm_ds.shape, "with stride", WATERSHED_STRIDE)

    # --- SEEDS & DIAGNOSTICS (on full-res crop) ---
    seed_indices_zyx_full = np.argwhere(seeds_crop > 0)
    max_seed_points = 200
    if len(seed_indices_zyx_full) > max_seed_points:
        step = max(1, len(seed_indices_zyx_full) // max_seed_points)
        seed_indices_zyx_full = seed_indices_zyx_full[::step]
    seed_list = [(int(x), int(y), int(z)) for z, y, x in seed_indices_zyx_full]

    seed_vals = norm_crop[seeds_crop > 0]
    seed_mean = float(seed_vals.mean())
    seed_std = float(seed_vals.std())

    # diagnostic only, not used to constrain growth
    lower = float(np.percentile(norm_crop[~ring_crop], 1))
    upper = float(np.percentile(norm_crop[~ring_crop], 99))

    # --- SEEDS FOR WATERSHED (on downsampled grid) ---
    seed_indices_zyx_ds = np.argwhere(seeds_ds > 0)
    if len(seed_indices_zyx_ds) > max_seed_points:
        step_ds = max(1, len(seed_indices_zyx_ds) // max_seed_points)
        seed_indices_zyx_ds = seed_indices_zyx_ds[::step_ds]
    seed_list_ds = [(int(x), int(y), int(z)) for z, y, x in seed_indices_zyx_ds]

    # --- BUILD IMAGES FOR GROWTH (DS + FULL) ---
    img_grow_full = sitk.GetImageFromArray(norm_crop.astype(np.float32))
    img_grow_full.SetSpacing(img_normalized.GetSpacing())
    img_grow_full.SetOrigin(img_normalized.GetOrigin())
    img_grow_full.SetDirection(img_normalized.GetDirection())

    img_grow_ds = sitk.GetImageFromArray(norm_ds.astype(np.float32))
    spacing_ds = tuple(s * WATERSHED_STRIDE for s in img_normalized.GetSpacing())
    img_grow_ds.SetSpacing(spacing_ds)
    img_grow_ds.SetOrigin(img_normalized.GetOrigin())
    img_grow_ds.SetDirection(img_normalized.GetDirection())

    # --- WATERSHED GROWTH ON DOWNSAMPLED CROP ---
    seed_labels_ds = np.zeros(norm_ds.shape, dtype=np.uint32)
    if seed_indices_zyx_ds.size == 0:
        raise RuntimeError("No seeds found in downsampled seeds_ds.")
    seed_labels_ds[tuple(seed_indices_zyx_ds.T)] = 1
    background_seed_mask_ds = ring_ds | air_ds
    seed_labels_ds[background_seed_mask_ds] = 2

    seed_img_ds = sitk.GetImageFromArray(seed_labels_ds)
    seed_img_ds.CopyInformation(img_grow_ds)

    WATERSHED_GRADIENT_SIGMA_MM = 0.02  # raise if bone leaks in, lower if under-growing
    gradient_img_ds = sitk.GradientMagnitudeRecursiveGaussian(img_grow_ds, sigma=WATERSHED_GRADIENT_SIGMA_MM)

    ws_ds = sitk.MorphologicalWatershedFromMarkers(
        gradient_img_ds, seed_img_ds, markWatershedLine=False, fullyConnected=True
    )
    ws_ds_arr = sitk.GetArrayFromImage(ws_ds)

    tooth_mask_grown_ds = (ws_ds_arr == 1).astype(np.uint8)

    # --- UPSAMPLE WATERSHED MASK BACK TO FULL CROP RESOLUTION ---
    mask_crop_arr = np.repeat(
        np.repeat(
            np.repeat(tooth_mask_grown_ds, WATERSHED_STRIDE, axis=0),
            WATERSHED_STRIDE, axis=1
        ),
        WATERSHED_STRIDE, axis=2
    )
    mask_crop_arr = mask_crop_arr[:norm_crop.shape[0], :norm_crop.shape[1], :norm_crop.shape[2]]

    n_grown_before_ring_strip = int(mask_crop_arr.sum())
    mask_crop_arr[ring_crop] = 0
    n_grown_after_ring_strip = int(mask_crop_arr.sum())

    tooth_mask_grown_img = sitk.GetImageFromArray(mask_crop_arr.astype(np.uint8))
    tooth_mask_grown_img.CopyInformation(img_grow_full)

    # --- CLOSING + FILLHOLE ON FULL-RES CROP ---
    tooth_mask_clean_crop = sitk.BinaryMorphologicalClosing(tooth_mask_grown_img, [2, 2, 2])
    tooth_mask_clean_crop = sitk.BinaryFillhole(tooth_mask_clean_crop, fullyConnected=True)

    cc_crop = sitk.ConnectedComponent(tooth_mask_clean_crop)
    cc_crop_arr = sitk.GetArrayFromImage(cc_crop)

    min_voxels = 500
    seeded_labels = np.unique(cc_crop_arr[seeds_crop > 0])
    seeded_labels = seeded_labels[seeded_labels > 0]

    stats = sitk.LabelShapeStatisticsImageFilter()
    stats.Execute(cc_crop)
    tooth_labels = [int(l) for l in seeded_labels if stats.GetNumberOfPixels(int(l)) >= min_voxels]

    if tooth_labels:
        tooth_mask_final_crop = sitk.ChangeLabel(cc_crop, {l: 1 for l in tooth_labels})
        tooth_mask_final_crop = sitk.BinaryThreshold(tooth_mask_final_crop, 1, 1, 1, 0)
    else:
        tooth_mask_final_crop = sitk.Image(cc_crop.GetSize(), sitk.sitkUInt8)
        tooth_mask_final_crop.CopyInformation(cc_crop)

    mask_crop_arr = sitk.GetArrayFromImage(tooth_mask_final_crop)

    # --- PASTE CROP BACK INTO FULL-SIZE MASK ---
    mask_arr = np.zeros(norm_arr.shape, dtype=np.uint8)
    mask_arr[z0:z1, y0:y1, x0:x1] = mask_crop_arr.astype(np.uint8)

    # --- SURFACE SMOOTHING (bias-corrected to preserve thin edges) ---
    SMOOTHING_THRESHOLD_BIAS_MM = -0.003  # negative = grow slightly to counteract erosion

    n_before_smoothing = int(mask_arr.sum())
    if mask_arr.any() and MASK_SMOOTHING_SIGMA_MM > 0:
        _s5_mask_img = sitk.GetImageFromArray(mask_arr.astype(np.uint8))
        _s5_mask_img.SetSpacing(img_normalized.GetSpacing())
        _s5_dist_map = sitk.SignedMaurerDistanceMap(
            _s5_mask_img, insideIsPositive=False, useImageSpacing=True
        )
        _s5_dist_smooth = sitk.SmoothingRecursiveGaussian(
            _s5_dist_map, sigma=MASK_SMOOTHING_SIGMA_MM
        )
        mask_arr = (sitk.GetArrayFromImage(_s5_dist_smooth) < -SMOOTHING_THRESHOLD_BIAS_MM)
    n_after_smoothing = int(mask_arr.sum())

    # --- FOV-EDGE CLEANUP ---
    nz_, ny_, nx_ = mask_arr.shape
    yy_, xx_ = np.meshgrid(np.arange(ny_), np.arange(nx_), indexing='ij')
    cy_, cx_ = ny_ / 2.0, nx_ / 2.0
    rr_ = np.sqrt((yy_ - cy_) ** 2 + (xx_ - cx_) ** 2)
    max_r_2d = rr_.max()
    ring_exclude_inner_frac = 0.90
    fov_edge_mask_2d = rr_ >= ring_exclude_inner_frac * max_r_2d

    n_masked_before = int(mask_arr.sum())
    mask_arr[:, fov_edge_mask_2d] = 0
    n_masked_after = int(mask_arr.sum())

    # --- 2D slice-wise hole fill (pulp / canal) ---
    for z in range(nz_):
        slice_mask = mask_arr[z].astype(bool)
        filled_slice = ndimage.binary_fill_holes(slice_mask)
        mask_arr[z] = filled_slice.astype(np.uint8)

    print("Slice-wise 2D hole fill applied across", nz_, "slices.")

    # Checkpoint 5a: before speckle removal
    _ = show_checkpoint(
        panels=[{
            "volume": stack_full,
            "overlays": [{"mask": mask_arr, "cmap": "Reds", "alpha": 0.4, "label": "tooth mask"}],
            "panel_title": "Tooth mask (post refinement, \npre speckle removal)",
        }],
        title="Checkpoint 5a -- after gaussian-smoothing resegmentation",
    )

    # --- SPECKLE REMOVAL (opening + component size filter) ---
    n_before_speckle = int(mask_arr.sum())
    mask_arr = ndimage.binary_opening(mask_arr.astype(bool), iterations=1).astype(np.uint8)
    mask_arr = remove_speckles(mask_arr, min_voxels=200)
    n_after_speckle = int(mask_arr.sum())
    print(f"Tooth mask voxels before speckle removal: {n_before_speckle}, after: {n_after_speckle} "
          f"({n_before_speckle - n_after_speckle} voxels removed)")

    tooth_mask_final = sitk.GetImageFromArray(mask_arr.astype(np.uint8))
    tooth_mask_final.CopyInformation(img_normalized)

    print("Watershed crop shape:", (z1 - z0, y1 - y0, x1 - x0), "of full", norm_arr.shape)
    print("Watershed downsampled shape:", norm_ds.shape, "with stride", WATERSHED_STRIDE)
    print("Seed mean:", seed_mean)
    print("Seed std:", seed_std)
    print("Seeds used (full-res crop):", len(seed_list))
    print(f"Grown mask voxels before ring-bridge strip (upsampled crop): {n_grown_before_ring_strip}")
    print(f"Grown mask voxels after ring-bridge strip (upsampled crop): {n_grown_after_ring_strip} "
          f"({n_grown_before_ring_strip - n_grown_after_ring_strip} voxels removed)")
    print(f"Tooth mask voxels before surface smoothing (MASK_SMOOTHING_SIGMA_MM={MASK_SMOOTHING_SIGMA_MM}, "
          f"bias={SMOOTHING_THRESHOLD_BIAS_MM}): {n_before_smoothing}")
    print(f"Tooth mask voxels after surface smoothing: {n_after_smoothing} "
          f"({n_before_smoothing - n_after_smoothing} voxels removed)")
    print("Tooth mask voxel count (before FOV-edge cleanup):", n_masked_before)
    print("Tooth mask voxel count (after FOV-edge cleanup):", n_masked_after)
    print(f"FOV-edge radius-fraction cleanup (ring_exclude_inner_frac={ring_exclude_inner_frac}): removed "
          f"{n_masked_before - n_masked_after} voxels beyond {ring_exclude_inner_frac:.0%} of the max in-plane radius.")
    print("Number of tooth components kept:", len(tooth_labels))

    # 5.6. dilate mask and compute ROI bounding box
    ref_img = img_full
    spacing = ref_img.GetSpacing()
    print("Voxel spacing (mm):", spacing)
    print("Dilation margin (mm):", dilation_mm)

    # Ensure binary uint8 mask
    mask_bin = sitk.Cast(tooth_mask_final > 0, sitk.sitkUInt8)

    # signed distance map (mm); dilated region is distance <= dilation_mm
    dist_map = sitk.SignedMaurerDistanceMap(
        mask_bin,
        squaredDistance=False,
        useImageSpacing=True
    )

    dilated_mask = sitk.Cast(dist_map <= dilation_mm, sitk.sitkUInt8)

    stats_roi = sitk.LabelShapeStatisticsImageFilter()
    stats_roi.Execute(dilated_mask)

    if not stats_roi.HasLabel(1):
        raise RuntimeError("Dilated mask does not contain label 1.")

    bbox = stats_roi.GetBoundingBox(1)   # (x0, y0, z0, sx, sy, sz)
    x0, y0, z0, sx, sy, sz = bbox
    print("ROI bounding box (index, size):", bbox)

    # 5.7a. crop full-res arrays to the 5.6 dilation ROI
    dilated_arr = sitk.GetArrayFromImage(dilated_mask).astype(bool)

    crop_idx = np.argwhere(dilated_arr)
    if crop_idx.size == 0:
        raise RuntimeError("Dilated mask is empty -- check that Section 5 produced a non-empty tooth mask.")

    pad_vox = 2  # pad so a background seed ring always exists in 5.7b
    rz0, ry0, rx0 = (crop_idx.min(axis=0) - pad_vox).clip(min=0)
    rz1, ry1, rx1 = np.minimum(crop_idx.max(axis=0) + 1 + pad_vox, np.array(dilated_arr.shape))

    n_full_vox = dilated_arr.size
    n_crop_vox = (rz1 - rz0) * (ry1 - ry0) * (rx1 - rx0)
    print("Refinement crop shape:", (int(rz1 - rz0), int(ry1 - ry0), int(rx1 - rx0)), "of full", dilated_arr.shape)
    print(f"Refinement crop voxels: {n_crop_vox:,} ({100 * n_crop_vox / n_full_vox:.3f}% of the full {n_full_vox:,}-voxel stack)")

    norm_refine_crop = norm_arr[rz0:rz1, ry0:ry1, rx0:rx1]
    ring_refine_crop = ring_mask_arr[rz0:rz1, ry0:ry1, rx0:rx1]
    air_refine_crop = air_mask_arr[rz0:rz1, ry0:ry1, rx0:rx1]
    tooth_refine_crop = mask_arr[rz0:rz1, ry0:ry1, rx0:rx1].astype(bool)
    dilated_refine_crop = dilated_arr[rz0:rz1, ry0:ry1, rx0:rx1]

    # Checkpoint 5.7a: refinement crop, masked to dilation shell
    _display_crop = np.where(dilated_refine_crop, norm_refine_crop, np.nan)

    _ = show_checkpoint(
        panels=[{
            "volume": _display_crop,
            "overlays": [
                {"mask": tooth_refine_crop, "cmap": "Reds", "alpha": 0.4, "label": "current tooth (pre-refinement)"},
            ],
            "panel_title": "Refinement crop (masked \nto dilation shell only)",
        }],
        title="Checkpoint 5.7a -- refinement crop before redo-watershed",
    )

    # tooth seed: interior erosion, used to validate connectivity only
    tooth_seed_refine = ndimage.binary_erosion(
        tooth_refine_crop, iterations=REFINE_TOOTH_SEED_ERODE_VOX
    )

    CANDIDATE_MARGIN_VOX = 4
    candidate_region = ndimage.binary_dilation(
        tooth_refine_crop, iterations=CANDIDATE_MARGIN_VOX
    )

    cutoff = threshold_otsu(norm_refine_crop[candidate_region])

    thresholded = candidate_region & (norm_refine_crop >= cutoff)

    CLOSING_ITER = 1
    thresholded = ndimage.binary_closing(thresholded, iterations=CLOSING_ITER)

    cc = ndimage.label(thresholded)[0]
    seeded_labels = np.unique(cc[tooth_seed_refine])
    seeded_labels = seeded_labels[seeded_labels > 0]
    tooth_refined_crop = np.isin(cc, seeded_labels)

    print("Otsu cutoff (candidate region only):", cutoff)
    print("Refined crop voxels:", int(tooth_refined_crop.sum()))

    # 5.7c. clean up refined crop, paste back into full-res mask

    crop_clean = tooth_refined_crop.copy()

    crop_clean = ndimage.binary_fill_holes(crop_clean)  # full 3D, not per-slice

    crop_clean = ndimage.binary_opening(crop_clean, iterations=1)

    mask_arr[rz0:rz1, ry0:ry1, rx0:rx1] = crop_clean.astype(np.uint8)

    tooth_mask_final = sitk.GetImageFromArray(mask_arr.astype(np.uint8))
    tooth_mask_final.CopyInformation(img_normalized)

    print("Tooth mask voxels after ROI refinement + cleanup:", int(mask_arr.sum()))

    # Checkpoint 5: after tooth growth & cleanup (preview-stride proxy for display)
    _ck5_stride = _s5_stride if '_s5_stride' in dir() else 1
    _ck5_stack_display = (
        stack_full[::_ck5_stride, ::_ck5_stride, ::_ck5_stride] if _ck5_stride > 1 else stack_full
    )
    _ = show_checkpoint(
        panels=[{
            "volume": _ck5_stack_display,
            "overlays": [{"mask": mask_arr, "cmap": "Reds", "alpha": 0.4, "label": "tooth mask"}],
            "panel_title": "Tooth mask (post \ngrowth + cleanup)"
            + (f" [PREVIEW stride={_ck5_stride}]" if _ck5_stride > 1 else ""),
        }],
        title="Checkpoint 5 / 7 -- after tooth growth & cleanup",
    )

    # 6. threshold bone from raw stack, using band derived from tooth mask
    # guard against a stale reduced-resolution mask
    assert mask_arr.shape == stack_full.shape, (
        f"tooth_mask_final/mask_arr shape {mask_arr.shape} does not match the full-resolution stack "
        f"{stack_full.shape} -- re-run Section 5 (the growth cell) at full resolution before continuing -- "
        f"Sections 6 onward require the full-resolution mask for correct bone thresholding, dilation, and "
        f"volume measurements."
    )

    # computed on raw stack, matching mask_arr's intensity scale
    tooth_raw_vals = stack_full[mask_arr > 0].astype(np.float64)

    bone_lower = float(np.percentile(tooth_raw_vals, BONE_LOWER_PERCENTILE))
    bone_upper = float(np.percentile(tooth_raw_vals, BONE_UPPER_PERCENTILE))

    bone_arr = (stack_full >= bone_lower) & (stack_full <= bone_upper)  # whole-volume bone-intensity mask

    print("=== Bone intensity band diagnostics ===")
    print(f"Tooth mask raw intensity stats: min={tooth_raw_vals.min():.1f}, max={tooth_raw_vals.max():.1f}, "
          f"mean={tooth_raw_vals.mean():.1f}, std={tooth_raw_vals.std():.1f}, n={tooth_raw_vals.size}")
    print(f"Derived bone intensity band ({BONE_LOWER_PERCENTILE}th-{BONE_UPPER_PERCENTILE}th percentile of tooth-mask voxels): "
          f"[{bone_lower:.1f}, {bone_upper:.1f}]")
    print("Bone voxels (whole volume, before restricting to the dilation shell):", int(bone_arr.sum()))
    print("Note: this band includes the tooth's own voxels by construction (it was derived from them); "
          "the tooth is excluded from the BONE VOLUME count below via the dilation-shell restriction.")

    # speckle removal (reuses remove_speckles from Section 5)
    n_bone_before_speckle = int(bone_arr.sum())
    bone_arr = ndimage.binary_opening(bone_arr, iterations=1)
    bone_arr = remove_speckles(bone_arr.astype(np.uint8), min_voxels=200).astype(bool)
    n_bone_after_speckle = int(bone_arr.sum())
    print(f"Bone mask voxels before speckle removal: {n_bone_before_speckle}, after: {n_bone_after_speckle} "
          f"({n_bone_before_speckle - n_bone_after_speckle} voxels removed)")

    # Checkpoint 6: bone (orange) and tooth (red), bands overlap by design
    _ = show_checkpoint(
        panels=[{
            "volume": stack_full,
            "overlays": [
                {"mask": bone_arr, "cmap": "Oranges", "alpha": 0.45, "label": "bone (tooth-intensity band)"},
                {"mask": mask_arr.astype(bool), "cmap": "Reds", "alpha": 0.45, "label": "tooth mask"},
            ],
            "panel_title": "Bone band (orange) \n+ tooth mask (red)",
        }],
        title="Checkpoint 6 / 7 -- after bone thresholding",
    )

    # 8. Tooth volume + bone volume restricted to the dilation shell
    spacing = img_full.GetSpacing()  # (x, y, z) mm
    voxel_vol_mm3 = spacing[0] * spacing[1] * spacing[2]

    tooth_arr = sitk.GetArrayFromImage(tooth_mask_final).astype(bool)
    dilated_arr = sitk.GetArrayFromImage(dilated_mask).astype(bool)

    # shell = dilated region minus tooth itself
    shell_arr = dilated_arr & (~tooth_arr)

    bone_shell_arr = shell_arr & bone_arr  # bone_arr from Section 6

    n_tooth_voxels = int(tooth_arr.sum())
    n_shell_voxels = int(shell_arr.sum())
    n_bone_shell_voxels = int(bone_shell_arr.sum())

    tooth_vol_mm3 = n_tooth_voxels * voxel_vol_mm3
    bone_vol_mm3 = n_bone_shell_voxels * voxel_vol_mm3

    print("=== Volume summary ===")
    print(f"Voxel spacing (mm): {spacing}")
    print(f"Voxel volume: {voxel_vol_mm3:.6f} mm^3")
    print()
    print(f"Tooth voxels: {n_tooth_voxels}")
    print(f"Tooth volume: {tooth_vol_mm3:.4f} mm^3  ({tooth_vol_mm3 / 1000.0:.4f} cm^3)")
    print()
    print(f"Dilation shell voxels ({dilation_mm} mm margin, tooth interior excluded): {n_shell_voxels}")
    print(f"Bone voxels within the {dilation_mm} mm dilation shell: {n_bone_shell_voxels}")
    print(f"Bone volume within the {dilation_mm} mm dilation shell: {bone_vol_mm3:.4f} mm^3  "
          f"({bone_vol_mm3 / 1000.0:.4f} cm^3)")

    # Checkpoint 7: final ROI (display only; NRRD export re-derives its own crop)
    full_crop = stack_full[z0:z0 + sz, y0:y0 + sy, x0:x0 + sx]
    tooth_crop = tooth_arr[z0:z0 + sz, y0:y0 + sy, x0:x0 + sx]
    shell_crop = shell_arr[z0:z0 + sz, y0:y0 + sy, x0:x0 + sx]
    bone_shell_crop = bone_shell_arr[z0:z0 + sz, y0:y0 + sy, x0:x0 + sx]

    _ = show_checkpoint(
        panels=[{
            "volume": full_crop,
            "overlays": [
                {"mask": shell_crop, "cmap": "Blues", "alpha": 0.25, "label": "dilation shell"},
                {"mask": bone_shell_crop, "cmap": "Oranges", "alpha": 0.40, "label": "bone (in shell)"},
                {"mask": tooth_crop, "cmap": "Reds", "alpha": 0.40, "label": "tooth"},
            ],
            "panel_title": "Final cropped ROI",
        }],
        title="Checkpoint 7 / 7 -- after final dilation & crop",
    )

    # 9. NRRD exports: combined label map + tooth-only + bone-only masks, all cropped to ROI
    os.makedirs(NRRD_OUTPUT_DIR, exist_ok=True)
    # build full-res label volumes aligned to img_full geometry
    tooth_label_img = sitk.Cast(tooth_mask_final, sitk.sitkUInt8)
    tooth_label_img.CopyInformation(img_full)

    bone_shell_img = sitk.GetImageFromArray(bone_shell_arr.astype(np.uint8))
    bone_shell_img.CopyInformation(img_full)

    # combined label map: 0=background, 1=tooth, 2=bone
    combined_arr = np.zeros_like(tooth_arr, dtype=np.uint8)
    combined_arr[bone_shell_arr] = 2
    combined_arr[tooth_arr] = 1
    combined_label_img = sitk.GetImageFromArray(combined_arr)
    combined_label_img.CopyInformation(img_full)

    # crop all three to the dilation-step ROI bbox
    combined_crop_img = crop_to_bbox(combined_label_img, bbox)
    tooth_crop_img = crop_to_bbox(tooth_label_img, bbox)
    bone_crop_img = crop_to_bbox(bone_shell_img, bbox)

    combined_path = os.path.join(NRRD_OUTPUT_DIR, f"{SAMPLE_NAME}_tooth_bone_labels.nrrd")
    tooth_path = os.path.join(NRRD_OUTPUT_DIR, f"{SAMPLE_NAME}_tooth_mask.nrrd")
    bone_path = os.path.join(NRRD_OUTPUT_DIR, f"{SAMPLE_NAME}_bone_mask.nrrd")

    sitk.WriteImage(combined_crop_img, combined_path, useCompression=True)
    sitk.WriteImage(tooth_crop_img, tooth_path, useCompression=True)
    sitk.WriteImage(bone_crop_img, bone_path, useCompression=True)

    print("=== NRRD export ===")
    print("Combined tooth+bone label NRRD:", combined_path)
    print("Labels: 0=background, 1=tooth, 2=bone")
    print("Tooth-only NRRD:", tooth_path)
    print("Bone-only NRRD (bone within dilation shell):", bone_path)
    print("Cropped size (voxels):", combined_crop_img.GetSize())
    print("Cropped spacing (mm):", combined_crop_img.GetSpacing())
    print("Cropped origin (mm):", combined_crop_img.GetOrigin())
    print("Cropped direction:", combined_crop_img.GetDirection())


    # Save volume measurements as CSV
    volume_file = os.path.join(
        NRRD_OUTPUT_DIR,
        f"{SAMPLE_NAME}_volumes.csv"
    )

    with open(volume_file, "w") as f:
        f.write("Sample,Tooth_Volume_mm3,Tooth_Volume_cm3,Bone_Volume_mm3,Bone_Volume_cm3\n")
        f.write(
            f"{SAMPLE_NAME},"
            f"{tooth_vol_mm3:.4f},"
            f"{tooth_vol_mm3 / 1000.0:.6f},"
            f"{bone_vol_mm3:.4f},"
            f"{bone_vol_mm3 / 1000.0:.6f}\n"
        )

    print("=== Volume measurements saved ===")
    print("Volume CSV:", volume_file)


    # Last line to run
    print("End of program.")
        

# command line variables and main
if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="uCT scan pipeline"
    )
    parser.add_argument("tiff_path")
    parser.add_argument("output_dir")
    args = parser.parse_args()
    main(
        args.tiff_path,
        args.output_dir
    )
