library("ggplot2")
library("dplyr")
library("scales")

#defined samples to include
sample_names_df = c("DW01","DW02","DW03","DW04","DW05","DW06","DW07","DW08","DW09","DW10",
                    "DW11","DW12","DW13","DW14","DW15","DW16","DW17","DW18","DW19","DW20",
                    "DW21","DW22","DW23","DW24","DW25","DW26","DW27","DW28")
# sample_names_df = c(       "DW02","DW03","DW04","DW05","DW06","DW07","DW08","DW09","DW10",
#                     "DW11","DW12","DW13","DW14","DW15","DW16","DW17","DW18","DW19","DW20",
#                     "DW21","DW22","DW23","DW24","DW25","DW26","DW27","DW28","DW29","DW30",
#                            "DW32","DW33","DW34","DW35","DW36")

default_table_names = c("Segment","Voxel count (LM)", "Volume mm3 (LM)", "Volume cm3 (LM)", "Voxel count (SV)", "Volume mm3 (SV)","Volume cm3 (SV)","Minimum", "Maximum", "Mean", "Standard deviation",
                        "Percentile 5", "Percentile 95", "Median", "Surface mm2", "Volume mm3 (CS)", "Volume cm3 (CS)")

#read in the samples details
sample_info_df = read.csv("uCT scan records.csv")
#sample_info_df
sampleID_df = data.frame(sample_info_df$Sample.name..scanco.)
colnames(sampleID_df) = "SampleID"
sampleID_duration_df = data.frame(factor(sample_info_df$exp_endpoint..days.))
colnames(sampleID_duration_df) = "Endpoint_days"
sampleID_and_duration_df = data.frame(cbind(sampleID_df, sampleID_duration_df))
sampleID_and_duration_df

#try reading in a sample
current_sample_Left = read.table(file = "DW02/DW02_L.tsv", header = TRUE, fill = TRUE)
current_sample_Left
dim(current_sample_Left)
colnames(current_sample_Left) = default_table_names
current_sample_Left
current_sample_Left = current_sample_Left[1:2,1:17]
current_sample_Left

#make a dataframe for all tooth statistics
num_samples = length(sample_names_df)
all_tooth_samples_df = data.frame(matrix(data = 0, nrow = num_samples, ncol = 19))
data_names = c("SampleID","Orientation side","Segment", "Voxel count (LM)","Volume mm3 (LM)" , "Volume cm3 (LM)", "Voxel count (SV)","Volume mm3 (SV)","Volume cm3 (SV)",
                          "Orientation side","Segment", "Voxel count (LM)","Volume mm3 (LM)" , "Volume cm3 (LM)", "Voxel count (SV)","Volume mm3 (SV)","Volume cm3 (SV)", 
               "Difference_in_volume", "Total_volume")
#data_names
colnames(all_tooth_samples_df) = data_names
all_tooth_samples_df

for(i in 1:num_samples){
  #read in the left side table
  current_sample_name = sample_names_df[i]
  left_table_file = current_sample_name
  left_table_file = paste0(left_table_file, "/")
  left_table_file = paste0(left_table_file, current_sample_name)
  left_table_file = paste0(left_table_file,"_L.tsv")
  #print(left_table_file)
  current_sample_Left_df = read.table(file = left_table_file, header = TRUE, fill = TRUE)
  colnames(current_sample_Left_df) = default_table_names
  current_sample_Left_df = current_sample_Left_df[1:2,1:11]
  current_sample_Left_df
  
  # #read in the right side table
  right_table_file = current_sample_name
  right_table_file = paste0(right_table_file, "/")
  right_table_file = paste0(right_table_file, current_sample_name)
  right_table_file = paste0(right_table_file,"_R.tsv")
  #print(right_table_file)
  current_sample_Right_df = read.table(file = right_table_file, header = TRUE, fill = TRUE)

  # #gather the results for the left side
  all_tooth_samples_df[i, 1] = sample_names_df[i]
  all_tooth_samples_df[i, 2] = "Left"
  all_tooth_samples_df[i, 3:9] = current_sample_Left_df[1, 1:7]

  all_tooth_samples_df[i, 10] = "Right"
  all_tooth_samples_df[i, 11:17] = current_sample_Right_df[1, 1:7]
  all_tooth_samples_df[i, 18] = all_tooth_samples_df[i, 6] - all_tooth_samples_df[i, 14]
  all_tooth_samples_df[i, 19] = all_tooth_samples_df[i, 6] + all_tooth_samples_df[i, 14]
  
}
all_tooth_samples_df

#gather only the sampleID and the differences
tooth_sample_IDS = all_tooth_samples_df$SampleID
#tooth_sample_IDS
tooth_differences = as.numeric(all_tooth_samples_df$Difference_in_volume)
#tooth_differences
sampleID_ligand_duration = data.frame(sample_info_df$Sample.name..scanco.)
tooth_data_to_plot = data.frame(cbind(tooth_sample_IDS,tooth_differences))
colnames(tooth_data_to_plot) = cbind("SampleID","Difference_in_volume")
#tooth_data_to_plot
#add the sampleID ligand duration
tooth_data_to_plot2 = merge(tooth_data_to_plot, sampleID_and_duration_df, by = "SampleID", all.x = TRUE)
tooth_data_to_plot2$Endpoint_days = as.character(tooth_data_to_plot2$Endpoint_days)
#tooth_data_to_plot2

#add the value 0
zero_value = data.frame(matrix(data = 0, nrow = 1, ncol = 3))
colnames(zero_value) = cbind("SampleID","Difference_in_volume", "Endpoint_days")
zero_value[1,1] = "Empty value"
tooth_data_to_plot2 = rbind(tooth_data_to_plot2,zero_value)
#move the scale from cm to microns, multiply by 1,000,000
tooth_data_to_plot3 = tooth_data_to_plot2
tooth_data_to_plot3$Difference_in_volume = as.numeric(tooth_data_to_plot3$Difference_in_volume) * 1000000
tooth_data_to_plot3$Endpoint_days = factor(tooth_data_to_plot3$Endpoint_days, 
                                             levels = c("0", "1", "2", "4", "8", "14", "28"))

scatter_of_tooth_differences = ggplot(data = tooth_data_to_plot3, aes(x = SampleID, y = Difference_in_volume, color = Endpoint_days)) + 
                               geom_point() + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
                               ylab("Difference in teeth Volume (Left - Right) microns") + 
                               scale_y_continuous(breaks = seq(-300, 300, by = 25))
print(scatter_of_tooth_differences)

summary(as.numeric(tooth_data_to_plot3[1:num_samples, 2]))
sd(tooth_data_to_plot3[1:num_samples, 2])
#plot the total volume of the teeth
tooth_volume = as.numeric(all_tooth_samples_df$Total_volume)
tooth_volume_to_plot = data.frame(cbind(tooth_sample_IDS, tooth_volume))
colnames(tooth_volume_to_plot) = cbind("SampleID","Total_volume")
#tooth_volume_to_plot
tooth_volume_to_plot2 = merge(tooth_volume_to_plot, sampleID_and_duration_df, by = "SampleID", all.x = TRUE)
tooth_volume_to_plot2$Endpoint_days = as.character(tooth_volume_to_plot2$Endpoint_days)
#tooth_volume_to_plot2
#add the value 0
# zero_sample_val = data.frame(matrix(data = 0, nrow = 1, ncol = 3))
# colnames(zero_sample_val) = c("SampleID", "Total_volume", "Endpoint_days")
# zero_sample_val[1,1] = "Empty_value"
# tooth_volume_to_plot2 = rbind(tooth_volume_to_plot2, zero_sample_val)
tooth_volume_to_plot2$Total_volume = as.numeric(tooth_volume_to_plot2$Total_volume)
#order the endpoint days variable
tooth_volume_to_plot2$Endpoint_days = factor(tooth_volume_to_plot2$Endpoint_days, 
                                             levels = c("0", "1", "2", "4", "8", "14", "28"))
tooth_volume_to_plot3 = tooth_volume_to_plot2
tooth_volume_to_plot3$Total_volume = tooth_volume_to_plot3$Total_volume*1000000

bargraph_of_teeth_volume = ggplot(data = tooth_volume_to_plot3, aes(x = SampleID, y = Total_volume, fill = Endpoint_days)) + 
                           geom_bar(stat = "identity", color = "black") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
                           scale_y_continuous(breaks = seq(0, 2000, by = 125)) +
                           ylab("Total Volume (microns cubed)") + ggtitle("Total Tooth Volume") + theme(axis.text.x = element_text(color = "black")) +
                           labs(fill = "Endpoint (days)")
print(bargraph_of_teeth_volume)  

summary(as.numeric(tooth_volume_to_plot3$Total_volume))
sd(tooth_volume_to_plot3$Total_volume)
