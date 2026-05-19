import argparse
import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from scipy.ndimage import gaussian_filter
from skimage import io
from skimage.filters import (
    threshold_otsu,
    threshold_yen,
    threshold_isodata,
    threshold_li,
    threshold_mean,
    threshold_minimum,
    threshold_triangle
)

# Background correction
def background_correct(image, sigma=50):
    # reduce noise by identifying uneven lighting
    # and tissue autofluorescence as a gradient
    background = gaussian_filter(image, sigma=sigma)
    corrected = image - background
    # remove negative values
    corrected[corrected < 0] = 0
    return corrected

# Noise estimation (robust)
def estimate_noise(image):
    # the bottom 30% of pixels in
    # terms of brightness are considered
    # noise, bg holds the values as a matrix
    cutoff = np.percentile(image, 30)
    bg = image[image <= cutoff]
    # prevent division by zero
    return np.std(bg) + 1e-8

# Save overlay
def save_overlay(image, mask, title, path):
    # plot the orignal grey image with the
    # mask in red and add the method name
    plt.figure(figsize=(6, 6))
    plt.imshow(image, cmap="gray")
    plt.imshow(mask, cmap="Reds", alpha=0.35)
    plt.title(title)
    plt.axis("off")
    plt.savefig(
            path,
            dpi=150,
            bbox_inches="tight"
            )
    plt.close()

# Combined figure
def save_all_methods_figure(image, results, output_dir):
    n = len(results)
    fig, axes = plt.subplots(
        1,
        n,
        figsize=(4 * n, 4)
    )
    if n == 1:
        axes = [axes]
    for ax, r in zip(axes, results):
        mask = r.get("mask", None)
        ax.imshow(image, cmap="gray")
        if mask is not None:
            ax.imshow(
                mask,
                cmap="Reds",
                alpha=0.35
            )
        ax.set_title(
            f"{r['method']}\n{r['status']}"
        )
        ax.axis("off")
    plt.tight_layout()
    out_path = os.path.join(
        output_dir,
        "all_methods.png"
    )
    plt.savefig(
        out_path,
        dpi=150,
        bbox_inches="tight"
    )
    plt.close()
    print(f"Saved: {out_path}")

# Main pipeline
def main(image_path, output_dir):

    os.makedirs(output_dir, exist_ok=True)
    # load OME-TIFF (CYX)
    img = io.imread(image_path)
    # use channel 1
    image = img[1].astype(np.float32)
    print("Image shape:", image.shape)
    # save raw image matrix in numpy binary
    raw_image_path = os.path.join(output_dir,"raw_image.npy")
    np.save(raw_image_path, image)
    print(f"Saved: {raw_image_path}")
    # save as csv format
    raw_csv_path = os.path.join(output_dir,"raw_image.csv")
    pd.DataFrame(image).to_csv(raw_csv_path,index=False)
    print(f"Saved: {raw_csv_path}")
    
    # background correction
    image = background_correct(image)
    # normalize image to 0-1
    image = (
            image - image.min()
            ) / (
            image.max() - image.min() + 1e-8
            )
    # save normalized image matrix in numpy binary
    normalized_image_path = os.path.join(output_dir,"normalized_image.npy")
    np.save(normalized_image_path, image)
    print(f"Saved: {normalized_image_path}")
    # csv format
    norm_csv_path = os.path.join(output_dir,"normalized_image.csv")
    pd.DataFrame(image).to_csv(norm_csv_path,index=False)
    print(f"Saved: {norm_csv_path}")

    # Global noise estimate
    noise_level = estimate_noise(image)
    print("Estimated noise:", noise_level)
    # Threshold methods
    methods = {
        "otsu": threshold_otsu,
        "yen": threshold_yen,
        "isodata": threshold_isodata,
        "li": threshold_li,
        "mean": threshold_mean,
        "minimum": threshold_minimum,
        "triangle": threshold_triangle
    }
    results = []
    # Evaluate each method
    for name, func in methods.items():
        try:
            # compute threshold
            thresh = func(image)
            # binary segmentation
            mask = image > thresh
            # split signal/background
            fg = image[mask]
            bg = image[~mask]
            fg_frac = mask.mean()
            # Handle invalid masks
            if len(fg) == 0 or len(bg) == 0:
                snr = np.nan
                cnr = np.nan
                score = np.nan
                status = "INVALID_MASK"
            else:
                fg_mean = fg.mean()
                bg_mean = bg.mean()
                bg_std = bg.std() + 1e-8
                # Signal-to-noise ratio
                # SNR = (foreground - background) / background std
                snr = (
                    (fg_mean - bg_mean)
                    / bg_std
                )
                # Contrast-to-noise ratio
                cnr = (
                    (fg_mean - bg_mean)
                    / bg_std
                )
                # Combined QC score
                score = (
                    snr
                    + cnr
                    - abs(fg_frac - 0.1)
                )
                status = "OK"
            # Save overlay image
            save_overlay(
                image,
                mask,
                (
                    f"{name} | "
                    f"{status} | "
                    f"score={score:.3f}"
                ),
                os.path.join(
                    output_dir,
                    f"{name}_overlay.png"
                )
            )
            # Store results
            results.append({
                "method": name,
                "threshold": float(thresh),
                "snr": snr,
                "cnr": cnr,
                # foreground statistics
                "fg_mean": fg.mean(),
                "fg_std": fg.std(),
                # background statistics
                "bg_mean": bg.mean(),
                "bg_std": bg.std(),
                "foreground_fraction": fg_frac,
                "score": score,
                "status": status,
                "mask": mask
            })
            print(
                f"{name}: "
                f"{status} | "
                f"SNR={snr:.3f} | "
                f"CNR={cnr:.3f} | "
                f"score={score:.3f}"
            )

        except Exception as e:
            print(f"{name} crashed: {e}")
            results.append({
                "method": name,
                "threshold": np.nan,
                "snr": np.nan,
                "cnr": np.nan,
                "foreground_fraction": np.nan,
                "score": np.nan,
                "status": "CRASH",
                "mask": None
            })
    # Save CSV summary
    # results is a list of dictionaries
    # if the current item is not the mask
    # array, then keep the value for the
    # results table
    df = pd.DataFrame([
        {
            k: v
            for k, v in r.items()
            if k != "mask"
        }
        for r in results
    ])
    df = df.sort_values(
        "score",
        ascending=True,
        na_position="last"
    )
    csv_path = os.path.join(
        output_dir,
        "threshold_qc_summary.csv"
    )
    df.to_csv(csv_path, index=False)
    print("\nSaved CSV:", csv_path)

    # Save combined methods figure
    save_all_methods_figure(
        image,
        results,
        output_dir
    )


# command line variables
if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Imaging-grade threshold QC pipeline"
    )
    parser.add_argument("input_image")
    parser.add_argument("output_dir")
    args = parser.parse_args()
    main(
        args.input_image,
        args.output_dir
    )
