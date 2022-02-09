# Class exercise - answers 

# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, 
               emmeans, 
               glmmTMB) 

# Load data 
data <- readr::read_csv("./tick_abundance_data.csv")
head(data)

# Plot the data 
data %>%
  ggplot(data = ., aes(x = grass_sp,
                       y = tick_abundance,
                       fill = season)) +
  geom_boxplot()

###########################################################################
# Fit candidate models ----------------------------------------------------
###########################################################################

##################
# 1. Gaussian GLM
##################

# While this dataset contains count data (which usually need a Poisson / Negative 
# binomial GLM), you could and should definitely look at the simplest model first 
# (i.e. a Gaussian GLM or ANOVA). 
# - Just to highlight that a standard ANOVA would be completely unappropriate 
#   for this analysis. 

# Fit model
mod_gaussian <- glm(tick_abundance ~ season * grass_sp,
                    data = data)

# Check residuals 
DHARMa::simulateResiduals(mod_gaussian, plot = TRUE)

# - Not good.
#   - LHS: QQplot shows significant departures from expectations of normality.
#          - KS test is significant and points are far awar from the red 1:1 line. 
#   - RHS: Evidence for unequal variances. Some boxplots have a much greater range than others. 

##################
# 2. Poisson GLM
##################

# As above, because we are analysing counts, the Poisson is a natural starting point. 

# Fit model
mod_poisson <- glm(tick_abundance ~ season * grass_sp,
                   data = data,
                   family = poisson(link = "log"))

# Check residuals 
DHARMa::simulateResiduals(mod_poisson, plot = TRUE)
DHARMa::testZeroInflation(mod_poisson)

# - Not good either, but better than the Gaussian GLM.
#   - LHS: QQplot shows significant departures from expectations of normality.
#          - KS test is significant and points are far awar from the red 1:1 line. 
#   - RHS: No evidence for unequal variances. 

##################
# 3. Negative binomial GLM
##################

# The Poisson GLM above improved the model fit significantly versus the Gaussian GLM/ANOVA
# we initially ran. This shows us that fitting a statistical distribution designed for 
# counts/abundance data was a good option. 
# - Because the poisson GLM wasn't a great fit, our next stop is the Negative Binomial GLM.
# - Negative Binomial (variance > mean) expects more variance than the Poisson (variance = mean). 

# Fit model
mod_nb <- glmmTMB::glmmTMB(tick_abundance ~ season * grass_sp,
                           data = data,
                           family = "nbinom2")

# Check residuals 
DHARMa::simulateResiduals(mod_nb, plot = TRUE)
DHARMa::testZeroInflation(mod_nb)





###########################################################################
# Inference options -------------------------------------------------------
###########################################################################

# Poisson GLM - type II sum-of-squares
car::Anova(mod_poisson, 
           type = "II",
           test = "LR")

# Poisson GLM - type III sum-of-squares
car::Anova(mod_poisson, 
           type = "III",
           test = "LR")

# NB GLM - type II sum-of-squares
car::Anova(mod_nb, 
           type = "II",
           test = "Chisq")

# NB GLM - type III sum-of-squares
car::Anova(mod_nb, 
           type = "III",
           test = "Chisq")
