library("jsonlite")

# samples included
sample_list = c("DW_1","DW_2","DW_3","DW_4","DW_5","DW_6","DW_7","DW_8","DW_9","DW_10","DW_11","DW_12","DW_13")

num_samples = length(sample_list)

# make a dataframe to hold the results
results_df = data.frame(matrix(data = 0, nrow = num_samples, ncol = 2))
colnames(results_df) = c("SampleID", "Distance")
results_df$SampleID = sample_list

#loop through each JSON object
for(i in 1:num_samples){
  current_sample = sample_list[i]
  json_file = paste0(current_sample, "/L.mrk.json")
  json_object = fromJSON(json_file)
  measurement_value = json_object$markups$measurements[[1]]$value
  measurement_value2 = as.numeric(measurement_value)
  results_df[i, 2] = measurement_value2
}
results_df

# save the results
write.csv(results_df,"Bone distances.csv",row.names = FALSE)




