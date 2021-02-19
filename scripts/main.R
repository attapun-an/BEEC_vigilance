# BEEC 
# Writen by attapun anivat | attapun-an | attapun.an@gmail.com

# Import Libraries ----
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(grid)

# Import Data ----
Vigil_data <- read.csv("data/Vigilance.csv")  #vigilance data for geese 

Vigilance_data
str(Vigil_data)

# Clean up collums
Vigil_data <- Vigil_data %>%
  mutate(Dur_divBy_Freq = Duration/Frequency) %>%                               # Fix data (supposed to be dur/freq, not freq/dur)
  select(Group_size, Frequency, Duration, Dur_divBy_Freq) 
  

# Models ----
  # Run everything against Group size
  # dependant ~ independant
Model_vs_Freq <- lm(Frequency ~ Group_size, data = Vigil_data)
Model_vs_Dur <- lm(Duration ~ Group_size, data = Vigil_data)
Model_vs_Dur_divBy_Freq <- lm(Dur_divBy_Freq ~ Group_size,
                                   data = Vigil_data)


# Summary and saving model ouputs
sink(file = "output/vsFreq.txt")
print(summary(Model_vs_Freq))
sink()
sink(file = "output/vsDur.txt")
summary(Model_vs_Dur)
sink()
sink(file = "output/vsDur_divBy_Freq.txt")
summary(Model_vs_Dur_divBy_Freq)
sink()

# Visualizing Models ----
(Graph_vs_Freq <- ggplot(Vigil_data, aes(x = Group_size, y = Frequency))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  xlab("\n Group size")+
  ylab("Number of times spent vigilant\n")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

(Graph_vs_Dur<- ggplot(Vigil_data, aes(x = Group_size, y = Duration))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  xlab("\n Group size")+
  ylab("Time spent being vigilant (s)\n")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

(Graph_vs_Dur_divBy_Freq <- ggplot(Vigil_data, aes(x = Group_size,
                                         y = Dur_divBy_Freq))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  xlab("\n Group size")+
  ylab("Frequency of vigilance / Duration of vigilance\n")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

# Combined Graph Freq Dur
Annotation_1 = grobTree(textGrob("(a)", x=0.01,  y=0.95, hjust=0,
                          gp=gpar(fontsize=13)))
Annotation_2 = grobTree(textGrob("(b)", x=0.01,  y=0.95, hjust=0,
                                 gp=gpar(fontsize=13)))

(Graph_combined <- Graph_vs_Freq+
                  annotation_custom(Annotation_1)+
                  Graph_vs_Dur+
                  annotation_custom(Annotation_2))


# save data ---
ggsave(filename = 'output/Graph_vs_Freq.png', plot = Graph_vs_Freq, 
       device = "png", width = 10, height = 6, units = "in")
ggsave(filename = 'output/Graph_vs_Dur.png', plot = Graph_vs_Dur, 
       device = "png", width = 10, height = 6, units = "in")
ggsave(filename = 'output/Graph_vs_Dur_divBy_Freq.png', plot = Graph_vs_Dur_divBy_Freq, 
       device = "png", width = 10, height = 6, units = "in")
ggsave(filename = 'output/Graph_Combined.png', plot = Graph_combined, 
       device = "png", width = 10, height = 6, units = "in")

