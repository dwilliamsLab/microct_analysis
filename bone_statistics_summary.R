library("ggplot2")

#defined samples to include
sample_names_df = c(       "DW02","DW03","DW04","DW05","DW06","DW07","DW08","DW09","DW10",
                    "DW11","DW12","DW13","DW14","DW15","DW16","DW17","DW18","DW19","DW20",
                    "DW21","DW22","DW23","DW24","DW25","DW26","DW27","DW28","DW29","DW30",
                           "DW32","DW33","DW34","DW35","DW36")

default_table_names = c("Segment","Voxel count", "Volume mm3", "Volume cm3","Minimum", "Maximum", "Mean", "Standard deviation",
                        "Percentile 5", "Percentile 95", "Median")
#read in the samples details
sample_info_df = read.csv("Desktop/Drake Williams lab/uCT_data_v2/uCT scan records.csv")
#sample_info_df
sampleID_df = data.frame(sample_info_df$Sample.name..scanco.)
colnames(sampleID_df) = "SampleID"
sampleID_duration_df = data.frame(factor(sample_info_df$exp_endpoint..days.))
colnames(sampleID_duration_df) = "Endpoint_days"
sampleID_and_duration_df = data.frame(cbind(sampleID_df, sampleID_duration_df))
sampleID_and_duration_df

#make a dataframe for all bone statistics
num_samples = length(sample_names_df)
all_bone_samples_df = data.frame(matrix(data = 0, nrow = num_samples, ncol = 19))
data_names = colnames(current_sample_Left[,1:7])
data_names = c("SampleID", "Orientation side", data_names, "Orientation side", data_names, "Difference_in_volume", "Total_volume")
#data_names
colnames(all_bone_samples_df) = data_names
all_bone_samples_df

for(i in 1:num_samples){
  #read in the left side table
  current_sample_name = sample_names_df[i]
  left_table_file = paste0("Desktop/Drake Williams lab/uCT_data_v2/", current_sample_name)
  left_table_file = paste0(left_table_file, "/")
  left_table_file = paste0(left_table_file, current_sample_name)
  left_table_file = paste0(left_table_file,"_L.tsv")
  #print(left_table_file)
  current_sample_Left_df = read.table(file = left_table_file, header = TRUE, fill = TRUE)
  colnames(current_sample_Left_df) = default_table_names
  current_sample_Left_df = current_sample_Left_df[1:2,1:11]
  current_sample_Left_df
  
  # #read in the right side table
  right_table_file = paste0("Desktop/Drake Williams lab/uCT_data_v2/", current_sample_name)
  right_table_file = paste0(right_table_file, "/")
  right_table_file = paste0(right_table_file, current_sample_name)
  right_table_file = paste0(right_table_file,"_R.tsv")
  #print(right_table_file)
  current_sample_Right_df = read.table(file = right_table_file, header = TRUE, fill = TRUE)

  # #gather the results for the left side
  all_bone_samples_df[i, 1] = sample_names_df[i]
  all_bone_samples_df[i, 2] = "Left"
  all_bone_samples_df[i, 3:9] = current_sample_Left_df[2, 1:7]

  all_bone_samples_df[i, 10] = "Right"
  all_bone_samples_df[i, 11:17] = current_sample_Right_df[2, 1:7]
  all_bone_samples_df[i, 18] = all_bone_samples_df[i, 6] - all_bone_samples_df[i, 14]
  all_bone_samples_df[i, 19] = all_bone_samples_df[i, 6] + all_bone_samples_df[i, 14]

}
all_bone_samples_df

#gather only the sampleID and the differences
bone_sample_IDS = all_bone_samples_df$SampleID
bone_sample_IDS
bone_differences = as.numeric(all_bone_samples_df$Difference_in_volume)
bone_differences
bone_data_to_plot = data.frame(cbind(bone_sample_IDS,bone_differences))
colnames(bone_data_to_plot) = cbind("SampleID","Difference_in_volume")
#bone_data_to_plot
bone_data_to_plot2 = merge(bone_data_to_plot, sampleID_and_duration_df, by = "SampleID", all.x = TRUE)
bone_data_to_plot2$Endpoint_days = as.character(bone_data_to_plot2$Endpoint_days)

#add the value 0
zero_value = data.frame(matrix(data = 0, nrow = 1, ncol = 3))
colnames(zero_value) = cbind("SampleID","Difference_in_volume","Endpoint_days")
zero_value[1,1] = "Empty value"
bone_data_to_plot2 = rbind(bone_data_to_plot2,zero_value)
summary(as.numeric(bone_data_to_plot2[1:num_samples, 2]))
#move the scale from cm to microns, multiply by 1,000,000
bone_data_to_plot3 = bone_data_to_plot2
bone_data_to_plot3$Difference_in_volume = as.numeric(bone_data_to_plot3$Difference_in_volume) * 1000000
bone_data_to_plot3$Endpoint_days = factor(bone_data_to_plot3$Endpoint_days,
                                             levels = c("0", "1", "2", "4", "8", "14", "28"))

scatter_of_bone_differences = ggplot(data = bone_data_to_plot3, aes(x = SampleID, y = Difference_in_volume, color = Endpoint_days)) + 
                              geom_point() + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
                              scale_y_continuous(breaks = seq(-250, 250, by = 25)) + ylab("Difference in bone Volume (Left - Right) microns")
print(scatter_of_bone_differences)
print(summary(bone_data_to_plot3$Difference_in_volume))
sd(bone_data_to_plot3$Difference_in_volume)


#plot the total volume for the left and right side
bone_total_volume = as.numeric(all_bone_samples_df$Total_volume)
bone_volume_to_plot = data.frame(cbind(bone_sample_IDS,bone_total_volume))
colnames(bone_volume_to_plot) = cbind("SampleID","Total_volume")
summary(as.numeric(bone_volume_to_plot$Total_volume))

#add the ligand duration data
bone_volume_to_plot2 = merge(bone_volume_to_plot, sampleID_and_duration_df, by = "SampleID", all.x = TRUE)
bone_volume_to_plot2$Endpoint_days = as.character((bone_volume_to_plot2$Endpoint_days))
#add the value 0
# zero_sample_val = data.frame(matrix(data = 0, nrow = 1, ncol = 3))
# colnames(zero_sample_val) = c("SampleID", "Total_volume", "Endpoint_days")
# zero_sample_val[1,1] = "Empty_value"
# bone_volume_to_plot2 = rbind(bone_volume_to_plot2,zero_sample_val)

bone_volume_to_plot2$Total_volume = as.numeric(bone_volume_to_plot2$Total_volume)
bone_volume_to_plot2$Endpoint_days = factor(bone_volume_to_plot2$Endpoint_days, 
                                              levels = c("1", "2", "4", "8", "14", "28"))
#increase the units from cm to microns, multiple by 1,000,000
bone_volume_to_plot3 = bone_volume_to_plot2
bone_volume_to_plot3$Total_volume = bone_volume_to_plot3$Total_volume*1000000

bargraph_of_bone_volume = ggplot(data = bone_volume_to_plot3, aes(x = SampleID, y = Total_volume, fill = Endpoint_days)) + 
                          geom_bar(stat = "identity") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
                          ylab("Total Volume (microns)") + scale_y_continuous(breaks = seq(0, 1375, by = 125)) +
                          ggtitle("Total Volume of Bones")
print(bargraph_of_bone_volume)

summary(bone_volume_to_plot3$Total_volume)
sd(bone_volume_to_plot3$Total_volume)        # Standard deviation
