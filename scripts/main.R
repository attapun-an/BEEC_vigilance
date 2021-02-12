# BEEC 
# Writen by attapun anivat | attapun-an | attapun.an@gmail.com

# Import Libraries ----
library(ggplot2)
library(dplyr)
library(tidyr)

# Import Data ----
Vigil_data <- read.csv("data/Vigilance.csv")  #vigilance data for geese 

Vigilance_data
str(Vigil_data)
# Clean up colums
Vigil_data <- Vigil_data %>% 
  select(Group_size, Frequency, Duration, Freq_Dur = Freq_divBy_Duration)

