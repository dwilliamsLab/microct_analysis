# %%
import numpy as np 
from pathlib import Path
from liffile import LifFile
import xarray as xr
import os
from bioio.writers import OmeTiffWriter
from bioio_base.types import PhysicalPixelSizes
import argparse

# %%
def load_image(lif_img):
    xdata = lif_img.asxarray() # convert to xarray
    curr_dims = list(lif_img.dims) # get the images current dimensions 
    ome_arr = ['T', 'Z', 'C', 'Y', 'X'] # specify the ome-dimensions 

    missing_ele = (set(ome_arr)-set(curr_dims)) # get the missing elements 
    
    for dim in ome_arr:
        if dim not in xdata.dims:
            xdata = xdata.expand_dims({dim: 1})

    xdata = xdata.transpose(*ome_arr)

    return xdata 
# %%
# uses LifFile to load .lif "project" file and returns "lif" which contains every image and all metadata in the project
def load_project(input_file):
    
    input = Path(input_file)
    print(f"processing file {input}")
    lif = LifFile(str(input))

    return lif
# %%
def save_images(in_path, antigen, out_path):

    cwd = os.getcwd() 
    full_out = Path(cwd) / out_path / antigen / 'images' # setting output to cwd/user inputted directory/images

    project = load_project(in_path) # load specified .lif file 

    for image in project.images: # looping through each image in the .lif file 
        try:
            xdata = load_image(image) # load image 
            data = xdata.values # storing image values 

            # function to get scale of image 
            def get_res(axis):
                
                coords = xdata.coords[axis].values # given [X,Y, or Z] axis get the value from the coords tag 
                scale = round((coords[1]-coords[0]) * 1e6, 4) if len(coords) > 1 else 1.0 # get the image scale by subtracting coords[1] form coords[0] on the axis and converting to µm from m (default leica unit)
                return scale

            px_sizes = PhysicalPixelSizes(get_res('Z'), get_res('Y'), get_res('X')) # using PhysicalPixelSizes funciton from bioio to be used for saving ometiff
            channels = [f"Ch_{i}" for i in range(xdata.shape[xdata.dims.index('C')])] # getting channel names from current image

            dir_path = Path(full_out) / image.name 
            Path.mkdir(dir_path, parents = True, exist_ok = True) # make a directory for each individual images to handle saving multiple copies easier later 
            
            tif_name = f"{xdata.name.replace(' ', '_')}.ome.tif" # creating name of new ome.tif 
            tif_path = dir_path / tif_name # getting whole path for saving new ome.tif

            # OmeTiffWriter from bioio which makes xml for you - way easier than tiffile 
            OmeTiffWriter.save(
                data,
                tif_path,
                dim_order = "TZCYX",
                channel_names = channels,
                physical_pixel_sizes = px_sizes
            )
            print(f"saved: {tif_name} | shape: {xdata.shape}")
        
        except Exception as e:
            print(f"failed to process {image.name}: {e}") # returns error if new tif not saved for some reason 
# %%
def main(input_folder="input", output_folder="outputs", antigen="antigen"):
    save_images(input_folder, antigen, output_folder)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Convert LIF to OME-TIFF")
    parser.add_argument("--input", default="input", help="Folder with LIF files")
    parser.add_argument("--output", default="outputs", help="Folder for TIFF outputs")
    parser.add_argument("--antigen", default="antigen", help="Antigen name")
    args = parser.parse_args()

    main(args.input, args.output, args.antigen)
