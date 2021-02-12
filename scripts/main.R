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

# Clean up collums
Vigil_data <- Vigil_data %>% 
  select(Group_size, Frequency, Duration, Freq_divBy_Duration)

# Models ----
  # Run everything against Group size
  # dependant ~ independant
Model_vs_Freq <- lm(Frequency ~ Group_size, data = Vigil_data)
Model_vs_Dur <- lm(Duration ~ Group_size, data = Vigil_data)
Model_vs_Freq_divBy_Duration <- lm(Freq_divBy_Duration ~ Group_size,
                                   data = Vigil_data)

summary(Model_vs_Freq)
summary(Model_vs_Dur)
summary(Model_vs_Freq_divBy_Duration)

# Visualizing Models ----
(Graph_vs_Freq <- ggplot(Vigil_data, aes(x = Group_size, y = Frequency))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  xlab("\n Group size")+
  ylab("Vigilance Frequency\n")+
  theme_bw()
)

(Graph_vs_Freq <- ggplot(Vigil_data, aes(x = Group_size, y = Duration))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  xlab("\n Group size")+
  ylab("Time spent being vigilant (s)\n")+
theme_bw()
)

(Graph_vs_Freq <- ggplot(Vigil_data, aes(x = Group_size,
                                         y = Freq_divBy_Duration))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  xlab("\n Group size")+
  ylab("Frequency of vigilance / Duration of vigilance\n")+
  theme_bw()
)

 