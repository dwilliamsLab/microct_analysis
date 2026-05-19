import os
import argparse
import numpy as np 
import xarray as xr
from pathlib import Path
from liffile import LifFile
from bioio.writers import OmeTiffWriter
from bioio_base.types import PhysicalPixelSizes


# This function is used to format the Lif image dimensions.
# T: time, Z: depth, C: channel, Y: image height, X: image width
def load_image(lif_img):
    xdata = lif_img.asxarray()
    ome_arr = ['T', 'Z', 'C', 'Y', 'X']
    for dim in ome_arr:
        # If the image lacks a dimension, add it as size 1?
        if dim not in xdata.dims:
            xdata = xdata.expand_dims({dim: 1})
    # ensures dimensions are always ordered identically
    return xdata.transpose(*ome_arr)

 
# Uses the LifFile function to load a .lif
# "project" file and returns "lif" which
# contains every image and all metadata in the project.
def load_project(input_file):
    input_path = Path(input_file)
    print(f"processing file {input_path}")
    lif = LifFile(str(input_path))
    return lif


# function to get scale of image 
def get_res(xdata, axis):
    coords = xdata.coords[axis].values
    scale = (
        round((coords[1] - coords[0]) * 1e6, 4)
        if len(coords) > 1
        else 1.0
    )
    return scale


#
def save_images(in_path, antigen, out_path):
    # setting output to cwd/user inputted directory/images
    full_out = Path(out_path) / antigen / 'images'
    # load specified .lif file
    project = load_project(in_path)
    # looping through each image in the .lif file
    for image in project.images:  
        try:
            # load image
            xdata = load_image(image)  
            # storing image values 
            data = xdata.values
            # using PhysicalPixelSizes function from
            # bioio to be used for saving ometiff
            px_sizes = PhysicalPixelSizes(
                        get_res(xdata, 'Z'),
                        get_res(xdata, 'Y'),
                        get_res(xdata, 'X')
                        )
            # getting channel names from current image
            n_channels = xdata.sizes['C']
            channels = [f"Ch_{i}" for i in range(n_channels)] 
            dir_path = full_out / image.name
            # make a directory for each individual images to
            # handle saving multiple copies easier later
            dir_path.mkdir(parents=True, exist_ok=True)
            # creating name of new ome.tif
            tif_name = f"{xdata.name.replace(' ', '_')}.ome.tif"
            # getting whole path for saving new ome.tif
            tif_path = dir_path / tif_name 

            # OmeTiffWriter from bioio which makes
            # xml for you - way easier than tiffile 
            OmeTiffWriter.save(
                data,
                tif_path,
                dim_order = "TZCYX",
                channel_names = channels,
                physical_pixel_sizes = px_sizes
            )
            print(f"saved: {tif_name} | shape: {xdata.shape}")
        
        except Exception as e:
            # returns error if new tif not saved for some reason
            print(f"failed to process {image.name}: {e}")  


# 
def main(input_folder="input", output_folder="outputs", antigen="antigen"):
    save_images(input_folder, antigen, output_folder)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Convert LIF to OME-TIFF")
    parser.add_argument("--input", default="input", help="Folder with LIF files")
    parser.add_argument("--output", default="outputs", help="Folder for TIFF outputs")
    parser.add_argument("--antigen", default="antigen", help="Antigen name")
    args = parser.parse_args()
    main(args.input, args.output, args.antigen)
