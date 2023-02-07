# INSTAL AND LOAD R PACKAGES ---------------------------------------------------
if (!require(svDialogs)) install.packages("svDialogs")
if (!require(readr)) install.packages("readr")
if (!require(writexl)) install.packages("writexl")
if (!require(readxl)) install.packages("readxl")
if (!require(tidyr)) install.packages("tidyr")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(ggpubr)) install.packages("ggpubr")
if (!require(scales)) install.packages("scales")
if (!require(dplyr)) install.packages("dplyr")
if (!require(broom)) install.packages("broom")
if (!require(matrixStats)) install.packages("matrixStats")
if (!require(purrr)) install.packages("purrr")
if (!require(stringr)) install.packages("stringr")

library(svDialogs)
library(readr)
library(writexl)
library(readxl)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(scales)
library(datasets)
library(dplyr)
library(broom)
library(magrittr)
library(purrr)

#SET WORKING DIRECTORY
getwd()
path_with_excel_files <- setwd(dlg_dir(default = getwd())$res)
path_with_excel_files
getwd()

# READ AND JOIN MULTIPLE EXCEL FILES -------------------------------------------

# get a list of files

dir(path_with_excel_files, full.names = T, pattern = ".xls")
filenames <- list.files(pattern = '\\.xls', full.names = TRUE)
filenames

#DELAYED FLUORESCENCE EXPERIMENT PROPERTIES
#Sample properties

sampleName <- dlg_input(message = "Inform the name of chemical solution", gui = .GUI)$res
numberSamples <- as.integer(dlg_input(message = "Inform the number of samples", gui = .GUI)$res)
numberReplicates <- as.integer(dlg_input(message = "Inform the number of replicates", gui = .GUI)$res)
led_setup <- dlg_input(message = "Type the LED setup for delayed fluorescence in PMX, e.g. L01S03R01", gui = .GUI)$res
cat(paste('Chemical tested:' , sampleName, '\n', 
          'Number of samples:', numberSamples, '\n',
          'Number of replicates:', numberReplicates, '\n',
          'PMX setup:', led_setup))

# Importing all excel files into one data frame

all_excel_files <- setwd(dlg_dir(default = getwd())$res) %>%
  list.files(pattern = ".xls", full.names = TRUE) %>%
  set_names(filenames) %>% 
  map_dfr(read_excel, sheet = 2, .id = "filename") %>% 
  mutate(filename = basename(filename)) %>% 
  pivot_wider(names_from = filename, values_from = L01S03R01)
all_excel_files

#Cutting the noise background

delayed_lum_data <- filter(all_excel_files, Time > 0.9)
delayed_lum_data

#DELAYED FLUORESCENCE PLOT DEFAULT
#graph[1]
plot(delayed_lum_data$Time, delayed_lum_data$`0.0(1).xls`,
     type = "l", col="dark green", 
     ylim = c(1e2,1e6), xlab = "Time [s]", ylab = "photon-counts")
lines(delayed_lum_data$Time, delayed_lum_data$`0.5(1).xls`, 
      type = "l", col="blue", 
      ylim = c(1e2,1e6), xlab = "Time [s]", ylab = "photon-counts")
lines(delayed_lum_data$Time, delayed_lum_data$`1.5(1).xls`, 
      type = "l", col="orange", 
      ylim = c(1e2,1e6), xlab = "Time [s]", ylab = "photon-counts")
lines(delayed_lum_data$Time, delayed_lum_data$`4.5(1).xls`, 
      type = "l", col="cyan", 
      ylim = c(1e2,1e6), xlab = "Time [s]", ylab = "photon-counts")
lines(delayed_lum_data$Time, delayed_lum_data$`13.5(1).xls`, 
      type = "l", col="red", 
      ylim = c(1e2,1e6), xlab = "Time [s]", ylab = "photon-counts")
lines(delayed_lum_data$Time, delayed_lum_data$`40.5(1).xls`, 
      type = "l", col="black", 
      ylim = c(1e2,1e6), xlab = "Time [s]", ylab = "photon-counts")

#graph[2]
plot(delayed_lum_data$Time, delayed_lum_data$`0.0(2).xls`,
     type = "l", col="dark green", 
     ylim = c(1e2,1e6), xlab = "Time [s]", ylab = "photon-counts")
lines(delayed_lum_data$Time, delayed_lum_data$`0.5(2).xls`, 
      type = "l", col="blue", 
      ylim = c(1e2,1e6), xlab = "Time [s]", ylab = "photon-counts")
lines(delayed_lum_data$Time, delayed_lum_data$`1.5(2).xls`, 
      type = "l", col="orange", 
      ylim = c(1e2,1e6), xlab = "Time [s]", ylab = "photon-counts")
lines(delayed_lum_data$Time, delayed_lum_data$`4.5(2).xls`, 
      type = "l", col="cyan", 
      ylim = c(1e2,1e6), xlab = "Time [s]", ylab = "photon-counts")
lines(delayed_lum_data$Time, delayed_lum_data$`13.5(2).xls`, 
      type = "l", col="red", 
      ylim = c(1e2,1e6), xlab = "Time [s]", ylab = "photon-counts")
lines(delayed_lum_data$Time, delayed_lum_data$`40.5(2).xls`, 
      type = "l", col="black", 
      ylim = c(1e2,1e6), xlab = "Time [s]", ylab = "photon-counts")

#graph[3]
plot(delayed_lum_data$Time, delayed_lum_data$`0.0(3).xls`,
     type = "l", col="dark green", 
     ylim = c(1e2,1e6), xlab = "Time [s]", ylab = "photon-counts")
lines(delayed_lum_data$Time, delayed_lum_data$`0.5(3).xls`, 
      type = "l", col="blue", 
      ylim = c(1e2,1e6), xlab = "Time [s]", ylab = "photon-counts")
lines(delayed_lum_data$Time, delayed_lum_data$`1.5(3).xls`, 
      type = "l", col="orange", 
      ylim = c(1e2,1e6), xlab = "Time [s]", ylab = "photon-counts")
lines(delayed_lum_data$Time, delayed_lum_data$`4.5(3).xls`, 
      type = "l", col="cyan", 
      ylim = c(1e2,1e6), xlab = "Time [s]", ylab = "photon-counts")
lines(delayed_lum_data$Time, delayed_lum_data$`13.5(3).xls`, 
      type = "l", col="red", 
      ylim = c(1e2,1e6), xlab = "Time [s]", ylab = "photon-counts")
lines(delayed_lum_data$Time, delayed_lum_data$`40.5(3).xls`, 
      type = "l", col="black", 
      ylim = c(1e2,1e6), xlab = "Time [s]", ylab = "photon-counts")

#DELAYED FLUORESCENCE GGPLOT---------------------------------------------------

df_plot <- setwd(dlg_dir(default = getwd())$res)%>%
  list.files(pattern = ".xls", full.names = TRUE) %>%
  set_names(filenames) %>% 
  map_dfr(read_excel, sheet = 2, .id = "filename") %>% 
  mutate(filename      = basename(filename)) %>% 
  mutate(concentration = str_extract(filename, pattern = "[^(]+")) %>% 
  mutate(replicate     = str_extract(filename, pattern = "(?<=\\().+?(?=\\))")) %>%
  rename(value = L01S03R01) %>% 
  select(filename, concentration, replicate, Time, value) %>% 
  arrange(filename, concentration, replicate, Time)
df_plot

sapply(df_plot, class)
df_plot$concentration <- as.numeric(df_plot$concentration)
df_plot$replicate <- as.numeric(df_plot$replicate)
sapply(df_plot, class)

#Plot individual data
df_plot%>%
  subset(Time > 1.0) %>%
  ggplot(aes(x = Time, y = value, color = concentration, group = replicate)) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_line() +
  ylim(1e2,1e6)+
  facet_grid(replicate ~ concentration)+
  labs(title = "Delayed Fluorescence",
       x = "Time [s]",
       y = "photon-counts")

#Plot average
df_plot %>%
  subset(Time > 1.0) %>%
  ggplot(aes(x = Time, y = value, color = concentration))+
  theme_bw()+
  ylim(1e2,1e6)+
  geom_hline(yintercept = 0) +
  stat_summary(fun.data = mean_se, geom = "line") +
  facet_grid(. ~ concentration)+
  labs(title = "Mean of Delayed Fluorescence",
       x = "Time [s]",
       y = "photon-counts")

#Sum of photon-counts
df_plot %>%
  subset(Time > 1.0) %>%
  ggplot(aes(x = concentration, y = value, color = concentration)) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  stat_summary(fun = sum, geom = "bar") +
  facet_grid(. ~ replicate)+
  labs(title = "Total counts of delayed fluorescence",
       x = "concentration",
       y = "total counts")

#Concentration vs total counts
df_plot %>%
  subset(Time > 0.9) %>%
  mutate(concentration = as.numeric(as.character(concentration))) %>% 
  group_by(concentration) %>% 
  summarise(total_counts = sum(value)) %>% 
  ggplot(aes(x = concentration, y = total_counts)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method = lm) +
  stat_cor(method="pearson", label.x = 25)+
  labs(x = "concentration",
       y = "total counts")

#Polynomial fit -------------------------------------------------------------
  
df_plot %>%
  filter(Time > 4.9) %>% 
  filter(Time < 39.9) %>% 
  ggplot(aes(x = Time, y = value, color = concentration))+
  theme_bw()+
  ylim(1e3,7.5e5)+
  geom_hline(yintercept = 0) +
  stat_summary(fun.data = mean_se, geom = "line") +
  stat_smooth(method = "lm", color = "red",
              formula = y ~ poly(x, 5))+
  facet_grid(. ~ concentration)+
  labs(title = "Polynomial fitting",
       x = "Time [s]",
       y = "photon-counts")

#How to extract the polynomial coef of each plot?
#How to find the instant Time for derivative  dy/dx = 0 ?

#STATISTICS ANALYSIS -----------------------------------------------------------

only_delayed_data <- select(delayed_lum_data, - Time)
only_delayed_data
summary(only_delayed_data)
only_delayed_data 



