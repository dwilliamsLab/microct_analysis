# First, set the directory.
# Step-1: click 'Session' 
#Step-2: Click 'Set working directory'
#Step-3: Click 'To source file location'


library("ggplot2")
library("dplyr")
library("ggpubr")



#define the table names
sample_names = c("Segment","Voxel_count","Volume_mm3","volume_cm3",
                "Minimum","Maximum","Mean","Standard_deviation","Percentile_5",
                "Percentaile_95","Median")

#sample names, use these to get all results
#Change the sample names everytime.
sample_name = c("NME_F_1_normalized","NME_F_2_normalized","NME_F_3_normalized","NME_M_4_normalized","NME_M_5_normalized",
                "NME_M_6_normalized","NME_M_7_normalized","NME_M_8_normalized")
num_samples = length(sample_name)

table_names = c("SampleID","Orientation side","Segment","Voxel count","Volume_mm3",
                "Orientation side","Segment","Voxel count","Volume_mm3",
                "Difference_in_volume","Total_volume","Average_volume")
#make a dataframe to hold the results
all_sample_results_df = data.frame(matrix(data = 0, nrow = num_samples,ncol = 12))
colnames(all_sample_results_df) = table_names

#now, read in each table, and enter the information into the dataframe
for(i in 1:num_samples){
  #make the file name for the left side sample
  current_sample = sample_name[i]
  left_table_file = paste0(current_sample, "/")
  left_table_file = paste0(left_table_file,current_sample)
  left_table_file = paste0(left_table_file,"_L.tsv")
  #print(left_table_file)
  
  #read the table for the left side sample in
  current_left_sample = read.table(file = left_table_file, header = TRUE, fill = TRUE)
  colnames(current_left_sample) = sample_names
  current_left_sample = current_left_sample[1:2,1:11]
  #print(current_left_sample)
  
  #save the segment, voxel_count, and volume_mm3
  all_sample_results_df[i, 1] = current_sample
  all_sample_results_df[i, 2] = "Left"
  all_sample_results_df[i,3:5] = current_left_sample[2, 1:3]
  #print(all_sample_results_df)
  
  #enter the right side bone volume
  right_table_file = paste0(current_sample,"/")
  right_table_file = paste0(right_table_file, current_sample)
  right_table_file = paste0(right_table_file,"_R.tsv")
  #print(right_table_file)
  
  #read the right side table in
  current_right_sample = read.table(file = right_table_file, header = TRUE, fill = TRUE)
  colnames(current_right_sample) = sample_names
  current_right_sample = current_right_sample[1:2, 1:11]
  #print(current_right_sample)
  
  #save the right hand results in our data frame
  all_sample_results_df[i, 6] = "Right"
  all_sample_results_df[i,7:9] = current_right_sample[2,1:3]
  #print(head(all_sample_results_df))
  #all_sample_results_df[i, ]
  
  #calculate the difference, sum, and average
  all_sample_results_df[i, 10] = all_sample_results_df[i, 5] - all_sample_results_df[i, 9]
  all_sample_results_df[i, 11] = all_sample_results_df[i, 5] + all_sample_results_df[i, 9]
  all_sample_results_df[i, 12] = (all_sample_results_df[i, 5] + all_sample_results_df[i, 9])/2
  
  #print(head(all_sample_results_df))
}
View(all_sample_results_df)
#save the results
write.csv(file = "NME_pilot_bone volume.csv", x = all_sample_results_df, row.names = FALSE)





