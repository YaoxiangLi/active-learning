# Only install packages which do not exist
list_of_packages <- c("tidyverse", "MASS", "ggplot2", "magrittr", "UsingR")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load the packages
library(tidyverse)
library(ggplot2)
library(magrittr)
library(MASS)
library(UsingR)

# Load the raw questionnaire data
data = readr::read_csv("https://raw.githubusercontent.com/lzyacht/active/master/data/Data0115.csv")

# Create group infomation columns
data$usefulness_group = rep(NA, nrow(data))
data$usefulness_group[data$usefulness > mean(as.numeric(data$usefulness))] = "Useless"
data$usefulness_group[data$usefulness <= mean(as.numeric(data$usefulness))] = "Useful"
data$convience_group = rep(NA, nrow(data))
data$convience_group[data$convience > mean(as.numeric(data$convience))] = "Inconvenience"
data$convience_group[data$convience <= mean(as.numeric(data$convience))] = "Convience"

# Define group name from: "Useless" "Useful" "Inconvenience" "Convience"
group_name = "Useless"  # 1. Change group name!!!
# Define column name from the columns:"seat_pos"         "usefulness"       "convience"       
# "sex"              "temp"             "light"            "lesson_care"      "room_size"       
# "room_crowd"       "chair_easy"       "teacher_voice"    "teacher_dist"     "legibility"      
# "attention_easy"   "tired_easy"       "cool_al"          "atmosphere"       "student_dist"    
# "express"          "communication"    "ownership"        "contribution"     "motivation"      
# "ict_plenty"
column_name = "student_dist" # 2. Change column name with quotation!!!

# Get the data need to be ploted
plot_data = data %>% dplyr::filter(usefulness_group == group_name)
# Save the current plotting column data as plot_data_c
plot_data_c = plot_data %>% dplyr::pull(column_name)
p = ggplot(plot_data) +
    geom_histogram(
        aes(x = student_dist, # 3. Change column name no quotation!!!
            y = ..density..),
        binwidth = 1,
        fill = "white",
        color = "black"
    )

x <- seq(min(plot_data_c), max(plot_data_c), length.out = 100)
df <-
    with(plot_data, data.frame(x = x, y = dnorm(x, mean(plot_data_c), sd(plot_data_c))))
p + geom_line(data = df,
              aes(x = x, y = y),
              color = "red",
              size = 1) + theme_bw() +
    labs(
        title = "Histogram and Density Plot for Student Distance\n", # 4. Change Title name!!!
        x = "Student Distance", #5. Change Title name!!!
        y = "Density",
        color = "Legend Title"
    ) +
    scale_x_discrete(limits = c(1, 2, 3, 4, 5, 6, 7)) +
    scale_y_continuous(limits = c(0, .6)) + # Control the Y axis range
    theme(
        text = element_text(size = 14, face = "bold"), # Control the all x y label texts
        axis.text.x = element_text(size = 14, face = "bold"), # x axis number
        axis.text.y = element_text(size = 14, face = "bold"), # y axis number
        plot.title = element_text(size = 16, face = "bold") # title
    )

