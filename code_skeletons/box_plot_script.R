# Libraries that are needed
library("ggplot2")
library("dplyr")
library("ggpubr")
library("cowplot")
library("viridis")

# read in data
input_data = read.csv("ABSL_3_pilot_bone_volume.csv")

#these are the comparisons for the p values
my_comparisons <- list(
  c("F_CTRL", "F_LIP"),
  c("M_CTRL", "M_LIP"),
  c("F_LIP", "M_LIP"),
  c("F_CTRL", "M_CTRL")
)

# factor the group variable for use
# this is the order that they appear
# in the figure
input_data$Class <- factor(
  input_data$Class,
  levels = c("F_CTRL", "F_LIP", "M_CTRL", "M_LIP")
)

# Define colorblind-friendly darker colors (Okabe-Ito)
# Set the values on the left equal to the different
# classes from above
okabe_ito <- c(
  "F_CTRL" = "#FFFFFF",  # White
  "F_LIP"  = "#999999",  # Grey
  "M_CTRL" = "#ff7400",  # Orange
  "M_LIP"  = "#7ED348"   # Green
)

# This is where we do the comparisons and save them for the plot
pvals <- c(
  t.test(Total_volume ~ Class,
         data = subset(input_data, Class %in% c("F_CTRL", "F_LIP")))$p.value,
  
  t.test(Total_volume ~ Class,
         data = subset(input_data, Class %in% c("M_CTRL", "M_LIP")))$p.value,
  
  t.test(Total_volume ~ Class,
         data = subset(input_data, Class %in% c("F_LIP", "M_LIP")))$p.value,
  
  t.test(Total_volume ~ Class,
         data = subset(input_data, Class %in% c("F_CTRL", "M_CTRL")))$p.value   
)
annotation_text <- sprintf("p = %.4f", pvals)

# put the data together to make the plot
total_bone_volume_plot <- ggplot(
  data = input_data,
  aes(x = Class, y = Total_volume, fill = Class)
) +
  # Show all points
  geom_jitter(width = 0.15, size = 6, alpha = 0.8,
              shape = 21, color = "black") +
  
  # Show group mean
  stat_summary(fun = mean, geom = "crossbar",
               width = 0.5, color = "black", fatten = 2) +
  
  # Add significance bars with your specific comparisons and p-values
  geom_signif(
    comparisons = my_comparisons,
    annotations = annotation_text,
    # to edit the position of the p values,
    # adjust the y_position values. They correspond
    # to the y axis
    y_position = c(1.3, 1.1, 1.0, 1.25),   
    tip_length = 0.03,
    textsize = 12,
    size = 1,
    color = "black",
    fontface = "bold"
  ) +
  
  ylab("Total bone volume (mm³)") +
  xlab(NULL) +
  # Use your custom colors
  scale_fill_manual(values = okabe_ito) +
  theme_bw(base_size = 48) +
  theme(
    axis.text.x = element_text(color = "black", face = "bold", angle = 45, hjust = 1),
    axis.text.y = element_text(color = "black", face = "bold"),
    axis.title = element_text(size = 42, , face = "bold"),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  # This is where you set the limits of the y axis
  scale_y_continuous(limits = c(0, 1.4),
                     breaks = seq(0, 1.4, by = 0.2))

print(total_bone_volume_plot)

pdf(file = "Total_bone_volume.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 12) # The height of the plot in inches
# Step 2: Create the plot with R code
print(total_bone_volume_plot)
# Step 3: Run dev.off() to create the file!
dev.off()
