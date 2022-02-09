# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, 
               emmeans, 
               glmmTMB) 
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
data2b <- data.frame(y = rnbinom(n = 30, mu = 2, size = 30),
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

#write_csv(x = data,
#          "./class_exercise_data.csv")

mod1 <- glm(tick_abundance ~ season * grass_sp,
            data = data,
            family = poisson(link = "log"))

DHARMa::simulateResiduals(mod1, plot = TRUE)
DHARMa::testZeroInflation(mod1)

mod2 <- glmmTMB::glmmTMB(tick_abundance ~ season * grass_sp,
                         data = data,
                         family = "nbinom2")

DHARMa::simulateResiduals(mod2, plot = TRUE)
DHARMa::testZeroInflation(mod2)

mod3 <- glm(tick_abundance ~ season * grass_sp,
            data = data)

DHARMa::simulateResiduals(mod3, plot = TRUE)

car::Anova(mod2, 
           test = "Chisq")

car::Anova(mod2, 
           type = "II",
           test = "Chisq")

car::Anova(mod2, 
           type = "III",
           test = "Chisq")

#####
#####
data1 <- data %>%
  dplyr::filter(grass_sp %in% c("grass_sp2", "grass_sp3")) %>%
  dplyr::select(insect_abundance = tick_abundance,
                trap_colour = grass_sp,
                trap_fruit = season) %>%
  dplyr::mutate(trap_colour = dplyr::case_when(
    trap_colour == "grass_sp2" ~ "Blue",
    TRUE ~ "Yellow")) %>%
  dplyr::mutate(trap_fruit = dplyr::case_when(
    trap_fruit == "Summer" ~ "Banana",
    TRUE ~ "Citrus"))


data1 %>%
  ggplot(data = ., aes(x = trap_colour,
                       y = insect_abundance,
                       fill = trap_fruit)) +
  geom_boxplot() +
  labs(fill = "Trap Fruit") +
  theme(legend.position = "right")

mod1 <- glm(insect_abundance ~ trap_colour * trap_fruit, 
            data = data1,
            family = poisson(link = "log"))

car::Anova(mod1, 
           test = "LR",
           type = "III")
