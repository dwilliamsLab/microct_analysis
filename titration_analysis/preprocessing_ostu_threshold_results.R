library("ggplot2")
library("dplyr")

#setwd("Desktop/Drake Williams lab/titration_src/output/CD31_LN/images/")
directory_files = list.files()
print(directory_files)
num_tiffs = length(directory_files)
# hold the results
all_results_df = data.frame(matrix(data = 0, nrow = num_tiffs, ncol = 12))
colnames(all_results_df) = c("sample", "method","threshold","snr","cnr","fg_mean",
                             "fg_std","bg_mean","bg_std","foreground_fraction","score","status")
print(all_results_df)
for(i in 1:num_tiffs){
  # get the directory of the tiff file
  current_directory = paste0(getwd(), "/", directory_files[i])
  # open the results file for this tiff
  current_results_df = read.csv(paste0(current_directory,"/threshold_qc_summary.csv"))
  current_results_df = filter(current_results_df,current_results_df$method == "otsu")
  all_results_df[i, 1] = directory_files[i]
  all_results_df[i,2:12] = current_results_df[1, ]
}

# save results
save_directory = paste0(getwd(),"/","all_otsu_thresholding_results.csv")
print(save_directory)
write.csv(x = all_results_df, file = save_directory, row.names = FALSE)



