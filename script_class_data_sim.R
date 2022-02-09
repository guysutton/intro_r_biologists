# Class exercise for General Linear Modelling component of the RU Hons 2021 
# Statistics Course in Zoology and Entomology. 
# - Author: Guy F. Sutton
# - If you have any questions or issues, please email me at g.sutton@ru.ac.za

# Please work through this script and submit an .R script file (this file) with your 
# code, workings and answers to me via email by Thursday 22nd July 2021 at 2pm.
# - Remember that there will be a 2 hour video conference directly after your 
#   hand in where you can ask any questions you have about the course materials.
# - You will then be allowed to re-submit this class exercise by Monday 
#   26th July 2021 by 1pm (again, submit it to me via email). 

# Your first submission will be marked primarily in terms of your effort 
# towards answering the questions, not on accuracy of your answers.
# - The second submission will be marked on the basis of how you revised 
#   your answers/code and the accuracy of the final answers. 

# For this exercise, you are going to be analysing a dataset concerning the 
# abundance of a tick on different grass species and across seasons.
# - You have collected 30 stems from each of 3 different grass species, namely: 
#   grass_sp1, grass_sp2 and grass_sp3 in both summer and winter.
# - You want to know whether:
#   (1) Tick abundance differs between different grass species?
#   (2) Tick abundance differs between seasons?, and 
#   (3) Tick abundance on the different grasses changes between seasons? 

###########################################################################
# Setup: Load required packages -------------------------------------------
###########################################################################

# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               here,
               DHARMa,
               mvabund,
               boral) 

# Change ggplot theme
# - This makes the graphs we will eventually make look pretty. 
# - Don't worry about this for now. 
theme_set(theme_classic() +
            theme(panel.border = element_rect(colour = "black", 
                                              fill = NA),
                  axis.text = element_text(colour = "black"),
                  axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), 
                                                            "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), 
                                                            "mm")),
                  legend.position = "none"))

###########################################################################
###########################################################################
###########################################################################

###########################################################################
# Load data to be analysed ------------------------------------------------
###########################################################################








# Simulate data 
set.seed(2000)             
data1a <- data.frame(y = rnbinom(n = 30, mu = 4, size = 30),
                    grass_sp = rep(LETTERS[1], each = 30),
                    season = rep(LETTERS[1], each = 30))
data1b <- data.frame(y = rnbinom(n = 30, mu = 10, size = 30),
                     grass_sp = rep(LETTERS[2], each = 30),
                     season = rep(LETTERS[1], each = 30))
data1c <- data.frame(y = rnbinom(n = 30, mu = 15, size = 30),
                     grass_sp = rep(LETTERS[3], each = 30),
                     season = rep(LETTERS[1], each = 30))

data2a <- data.frame(y = rnbinom(n = 30, mu = 8, size = 30),
                     grass_sp = rep(LETTERS[1], each = 30),
                     season = rep(LETTERS[2], each = 30))
data2b <- data.frame(y = rnbinom(n = 30, mu = 25, size = 30),
                     grass_sp = rep(LETTERS[2], each = 30),
                     season = rep(LETTERS[2], each = 30))
data2c <- data.frame(y = rnbinom(n = 30, mu = 50, size = 30),
                     grass_sp = rep(LETTERS[3], each = 30),
                     season = rep(LETTERS[2], each = 30))

data <- dplyr::bind_rows(data1a, data1b, data1c, data2a, data2b, data2c) %>%
  dplyr::rename(tick_abundance = y) %>%
  dplyr::mutate(season = dplyr::case_when(
    season == "A" ~ "Summer",
    season == "B" ~ "Winter")) %>%
  dplyr::mutate(grass_sp = dplyr::case_when(
    grass_sp == "A" ~ "grass_sp1",
    grass_sp == "B" ~ "grass_sp2",
    grass_sp == "C" ~ "grass_sp3")) %>%
  dplyr::select(season, grass_sp, tick_abundance)
head(data)

write_csv(x = data,
          "./class_exercise_data.csv")
mod1 <- glm(tick_abundance ~ season * grass_sp,
            data = data,
            family = poisson(link = "log"))

DHARMa::simulateResiduals(mod1, plot = TRUE)

mod2 <- glmmTMB::glmmTMB(tick_abundance ~ season * grass_sp,
                         data = data,
                         family = "nbinom2")

DHARMa::simulateResiduals(mod2, plot = TRUE)

car::Anova(mod2, 
           type = "III",
           test = "Chisq")

data %>%
  ggplot(data = ., aes(x = grass_sp,
                       y = tick_abundance,
                       fill = season)) +
  geom_boxplot() +
  labs(fill = "Season") +
  theme(legend.position = "right")
