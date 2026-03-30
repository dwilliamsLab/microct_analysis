import pandas as pd
import numpy as np

def main():
    # list the sample names
    samples = ["DW_1_normalized","DW_2_normalized"]

    # define column headers (13 columns total)
    header = ["Sample_ID",
            "Bone Volume Density_R","Trabecular Thickness_R","Trabecular Separation_R", "Trabecular Number_R","Bone Surface Density_R",
            "Bone Volume Density_L","Trabecular Thickness_L","Trabecular Separation_L", "Trabecular Number_L","Bone Surface Density_L"]

    # create empty dataframe with correct shape
    df = pd.DataFrame(columns=header)

    # loop with index
    for i, sample_name in enumerate(samples):

        # RIGHT side
        file_name1 = sample_name + "/" + sample_name + "_R_bone_morphology.txt"
        with open(file_name1, "r") as f:
            lines1 = [line.strip() for line in f if line.strip()]
        
        values_R = lines1[-1].split(",")  # convert string → list

        # LEFT side
        file_name2 = sample_name + "/" + sample_name + "_L_bone_morphology.txt"
        with open(file_name2, "r") as f:
            lines2 = [line.strip() for line in f if line.strip()]
        
        values_L = lines2[-1].split(",")  # convert string → list

        # combine R + L values
        row_values = [sample_name] + values_R + values_L

        # assign to dataframe row
        df.loc[i] = row_values

    # Outside of for loop
    # Total Trabecular Thickness
    df["Trabecular Thickness_Total"] = pd.to_numeric(df["Trabecular Thickness_R"]) + pd.to_numeric(df["Trabecular Thickness_L"])

    # Total Trabecular Number
    df["Trabecular Number_Total"] = pd.to_numeric(df["Trabecular Number_R"]) + pd.to_numeric(df["Trabecular Number_L"])

    # Save the DataFrame to a CSV file
    df.to_csv("bone_metrics_combined.csv", index=False)

main()
