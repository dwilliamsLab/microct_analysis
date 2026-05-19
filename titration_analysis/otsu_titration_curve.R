library("ggplot2")
library("dplyr")
# read in the otsu thresholding results
#setwd("Desktop/Drake Williams lab/titration_src/output/CD11B_LN/images/")
otsu_results_df = read.csv("all_otsu_thresholding_results.csv")

# add the dilution details using the sample string
otsu_results_df$dilution_level <- as.numeric(
  sub("_.*", "", otsu_results_df$sample)
)

# collect the mean for each dilution level
dilution_means_df <- aggregate(
  snr ~ dilution_level,
  data = otsu_results_df,
  FUN = mean
)

# plot
titration_plot1 = ggplot(data = otsu_results_df, aes(x = dilution_level, y = snr)) + 
                  geom_point() + theme_bw(base_size = 16) +
                  labs(x = "Dilution level", y = "Signal to Noise ratio") +
                  scale_x_continuous(breaks = c(100, 200, 400, 800)) +
                  scale_y_continuous(breaks = c(seq(from = 0, to = 14, by = 2)), limits = c(0, 14)) +
                  theme(axis.ticks = element_line(color = "black"), axis.text = element_text(color = "black")) +
                  geom_point(data = dilution_means_df, aes(x = dilution_level, y = snr), color = "red") +
                  geom_line(data = dilution_means_df, aes(x = dilution_level, y = snr), color = "red")
print(titration_plot1)

#save the plot
ggsave(
  filename = "CD4_lig_otsu_titration_plot.pdf",
  plot = titration_plot1,
  width = 8,
  height = 6
)













