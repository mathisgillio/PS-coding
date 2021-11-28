################################################################################
##  Data Science in Ecological and Environmental Sciences - Data Journalism   ##
##                            Professional Skills                             ##
##                      Date: 21-November-2021                                ##
##                            Written by:                                     ##
##              Mathis Gillio (s1843841@ed.ac.uk, @mathisgillio)              ##
################################################################################

## This project aims at answering questions for the Professional Skills coding assignment

### 1. Load the librairies ----

library(tidyverse)  # includes dplyr (for data manipulation) and ggplot2 
library(lme4) # allows to generate a model
library(ggeffects) # allows to plot model predictions 
install.packages("tinytex")
tinytex::install_tinytex()

### 2. Load the data ---- 

inga_traits <- read.csv("Data/Inga_traits.csv")

### 3. Inspect the data ---- 

head(inga_traits)
str(inga_traits)

### Exercise 1: Histograms and normality ---- 

## Question a: ---- 
# make and present a histogram for leaf area of these species 

# Create theme to suit all of our work 

theme_ps <- function(){            # creating a new theme function
  theme(axis.title = element_text(size = 15,
                                  face = "bold"),
        axis.text.x = element_text(size = 12,
                                   vjust = 1,
                                   face = "bold"), 
        axis.text.y = element_text(size = 12, face = "bold"),  # define font,
        # font sizes, alignment
        #legend.position = "none",  # remove legend
        plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"),  # create plot
        # margins
        panel.grid = element_blank())
}

(hist_leafarea<- ggplot(inga_traits, aes(x = Leaf_Area)) + # create an histogram of population 
    # count 
    geom_histogram(aes(y = ..count..), binwidth = 7,
                   colour = "#242424", fill = "#7CCD7C") +
    theme_ps() +
    labs(x = "\nLeaf area (in cm2)", 
         y = "Count\n") + # edit axis labels 
    scale_y_continuous(expand = expand_scale(mult = c(0,0.1))) + # remove gap between y axis
  # and legend
    scale_x_continuous(breaks = c(0,20,40,60,80,100,120,140)) + 
    geom_vline(xintercept = mean(inga_traits$Leaf_Area), linetype = "dotted",
               colour = "#363636", size = 1) +
    # Adding in a text allocation - the coordinates are based on the x and y axes
    annotate("text", x = 28, y = 4.6, label = "The mean leaf area\n is 47 cm2.") +
    geom_curve(aes(x = 28, y = 5, xend = mean(inga_traits$Leaf_Area) - 3, yend = 5),
               arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
               color = "grey20", curvature = -0.3)
    # "\n" creates a line break
)

# The data appears to show a Poisson type distribution. That is the data is centered
# on the left and skewed towards the right. 

## Question b: ---- 
# log transform the data and create new histogram 

inga_traits <- inga_traits %>% 
  mutate(log.leafarea = log(Leaf_Area))

(hist_leafarea2<- ggplot(inga_traits, aes(x = log.leafarea)) + # create an histogram of population 
    # count 
    geom_histogram(aes(y = ..count..), binwidth = 0.25,
                   colour = "#242424", fill = "#548B54") +
    theme_ps() +
    labs(x = "\nLogged leaf area (in cm2)", 
         y = "Count\n") + # edit axis labels 
    scale_y_continuous(expand = expand_scale(mult = c(0,0.1))) # remove gap between y axis
  # and legend
)

# The data now seems to be normally distributed 

## Question c: ---- 
# describe the distribution of leaf sizes across trees in this region to a non-scientist?

mean(inga_traits$Leaf_Area)

# In the Amazon of Southeast Peru, it seems like leaves or leaflets have a mean area of 
# 47 cm2. This can be explained by most of the trees in the Amazon forest having leaves 
# which sizes are between 10 and 70 cm2. The proportion of leaves greater than this decreases 
# as the potential size of the leaf increases 


### Exercise 2: Box plots and Analysis of Variance (ANOVA) ---- 

## Question a: ---- 
# Make and present a boxplot of leaf phosphorous concentration versus habitat

levels(inga_traits$Habitat) <- c("Foodplain",  # Relevel factor labels
                                  "Generalist",
                                  "Upland")

(phosph_boxplot <- ggplot(inga_traits, aes(Habitat, P_Leaf)) + 
   geom_boxplot(aes(fill = Habitat)) +
   theme_ps() +
   scale_fill_manual(values = c("#43CD80", "#2E8B57", "#9AFF9A")) +               # Adding custom colours
   scale_colour_manual(values = c("#43CD80", "#2E8B57", "#9AFF9A")) +             # Adding custom colours
   ylab("Leaf phosphorous concentration (in mg/g)\n") +                             
   xlab("\nHabitat type") +
   theme(legend.position = "none")
 )
   
# The concentration of phosphorous seems to be higher in Foodplain habitat 
# and lowest in Upland habitats. This could be due to the fact that food plain habitats 
# get drained with rain that has ran through more land and thus that is richer in nutrients 
# concentrations 

## Question b: ---- 
# statistically test if species found in different habitats have significantly 
# different phosphorous concentrations in their leaves

# Check the distribution of the data 

(hist_phosph<- ggplot(inga_traits, aes(x = P_Leaf)) + # create an histogram of population 
    # count 
    geom_histogram(aes(y = ..count..), binwidth = 0.01,
                   colour = "#242424", fill = "#548B54") +
    theme_ps() +
    labs(x = "\nPhosphourous concentration (in gm/g)", 
         y = "Count\n") + # edit axis labels 
    scale_y_continuous(expand = expand_scale(mult = c(0,0.1))) # remove gap between y axis
  # and legend
)

# Construct the model and ANOVA
phosph_lm <- lm(P_Leaf ~ Habitat, data = inga_traits)
anova(phosph_lm)
summary(phosph_lm)

# The anova() function gives you an ANOVA table, with all its constituent components, 
# including the results of an F test
# The summary function gives you all of the above and more

# The F - value from the ANOVA is 8.598 with 2 and 27 DF
# The p - value is 0.001

# You can use the F statistic when deciding to support or reject the null hypothesis. 

# P value: If you have a significant result, it doesn’t mean that all your variables are significant. 
# The statistic is just comparing the joint effect of all the variables together.

# If the p value is less than the alpha level, go to Step 2 (otherwise your results 
# are not significant and you cannot reject the null hypothesis). A common alpha level 
# for tests is 0.05.

# “Is the variance between the means of two populations significantly different?” 
# The F value in the ANOVA test also determines the P value; The P value is the probability
# of getting a result at least as extreme as the one that was actually observed, 
# given that the null hypothesis is true.

# The p value is a probability, while the f ratio is a test statistic, calculated as:
# F value = variance of the group means (Mean Square Between) / mean of the within group variances (Mean Squared Error)

## Question c: ---- 
# Evaluation of the model: 

phosph_resids <- resid(phosph_lm) # obtain the residuals 

shapiro.test(phosph_resids) # test the normality of the residuals

bartlett.test(P_Leaf ~ Habitat, data = inga_traits) # check equality of variances
# Not met p value is under 0.05

plot(phosph_lm)
# first plot you see is the residual versus fitted plot. This lets you readily assess 
# if you have constant variances
# next plot is the normal Q-Q plot. This allows you to assess if your residuals 
# are normally distributed. 
# third plot is also primarily intended to identify non-constant variance, or 
# heteroscedasticity
# The last plot, to compare leverage versus residual values is not very relevant in 
# this context because we do not have continuous predictor variables

# Assumptions of ANOVA: 
# -	Assumption: equal amount of variance 
# -	Assumption: all observations are independent 
# -	The variances of the residuals in different groups are the same 
# -	The residuals are normally distributed 

## Question d: ---- 
# Improve the model and report the revised F Statistic and p-value

# Log-transform the phoshpourous data: 

inga_traits <- inga_traits %>% 
  mutate(log.phosph = log(P_Leaf))

(hist_phosph<- ggplot(inga_traits, aes(x = log.phosph)) + # create an histogram of population 
    # count 
    geom_histogram(aes(y = ..count..), binwidth = 0.06,
                   colour = "#242424", fill = "#548B54") +
    theme_ps() +
    labs(x = "\nLogged phosphourous concentration (in gm/g)", 
         y = "Count\n") + # edit axis labels 
    scale_y_continuous(expand = expand_scale(mult = c(0,0.1))) # remove gap between y axis
  # and legend
)

phosph_lm2 <- lm(log.phosph ~ Habitat, data = inga_traits)
anova(phosph_lm2)
summary(phosph_lm2)

phosph_resids <- resid(phosph_lm2)# obtain the residuals 

shapiro.test(phosph_resids) # test the normality of the residuals

bartlett.test(log.phosph ~ Habitat, data = inga_traits) # check equality of variances

plot(phosph_lm2)

# It seems that the assumptions are now met 

# The F - value from the ANOVA is 10.12 with 2 and 27 DF
# The p - value is 0.0005

## Question e: ---- 
# provide an explanation of your analysis, the results and what they mean, in 
# non-technical terms that would be accessible to a relative or someone you meet in a pub 

### Exercice 3: Multiple explanatory variables ---- 

## Question a: ---- 
# Make a plot of leaf phosphorous concentrations versus leaf carbon concentrations

(P_C_scatter <- ggplot(inga_traits, aes (x = C_Leaf, y = P_Leaf, colour = Habitat)) +
   geom_point(size = 2) +                                               # Changing point size
   geom_smooth(method = "lm", aes(fill = Habitat)) +               # Adding linear model fit, colour-code by country
   theme_ps() +
   scale_fill_manual(values = c("#43CD80", "lightskyblue2", "#FFB90F")) +                # Adding custom colours for solid geoms (ribbon)
   scale_colour_manual(values = c("#43CD80", "lightskyblue2", "#FFB90F")) +             # Adding custom colours for lines and points 
   ylab("Leaf Phosphorous concentration (in mg/g)\n") +                             
   xlab("\nLeaf Carbon concentration (in mg/g)") +
   theme(legend.position = "none")
)

## Question b: ----

# Generalist and upland show the same pattern: the concentration of Phosphorous increases 
# linearly as the concentration of Carbon increases in the leaf 
# However, in the foodplain habitat, the concentration of phosphorous decreases as the 
# concentration of carbon increases (the concentration of phosphourous is highest when 
# the concentration of carbon is lowest)

# Foodplain species are more likely to be adapted to live in high concentrations of 
# phosphorous
# Generalist and upland species are more liekly to be phosphourous limited. Therefore when 
# then concentration of phosphourous increases they perform better at photosynthesis thus 
# increasing their carbon concentration 

# Choose P-limited and non P-limited species: 

inga_traits <- inga_traits %>% 
  mutate(new_habitat = 
           case_when(Habitat %in% c("Generalist", "Upland") ~ "Altitude adapted",
                     Habitat == "Foodplain" ~ "Non Altitude adapted")) 

# Construct a statistical model where you have both habitat group and leaf carbon 
# concentration as predictors of leaf phosphorous concentration

habitat_leaf_C_lm <-lm(P_Leaf ~ new_habitat*C_Leaf, data = inga_traits)

# Using interaction term allows us to state that the relationship between phosphourous 
# concentration and leaf carbon concentration might not be the same in both habitats. 
# We have sene that the relationship bewteen phosph and carbon varies in the two habitats.

anova(habitat_leaf_C_lm)
summary(habitat_leaf_C_lm)

## Question c: ----
# Evaluate your statistical model using diagnostic plots

new_habitat_resids <- resid(habitat_leaf_C_lm)# obtain the residuals 

shapiro.test(new_habitat_resids) # test the normality of the residuals

plot(habitat_leaf_C_lm)

# Revised models: 

# Model 2: 

habitat_leaf_C_lm2 <-lm(log.phosph ~ new_habitat*C_Leaf, data = inga_traits)
anova(habitat_leaf_C_lm2)
summary(habitat_leaf_C_lm2)

new_habitat_resids2 <- resid(habitat_leaf_C_lm2)# obtain the residuals 

shapiro.test(new_habitat_resids2) # test the normality of the residuals

plot(habitat_leaf_C_lm2)

# Model 3: 

habitat_leaf_C_lm3 <-lm(P_Leaf ~ new_habitat + C_Leaf, data = inga_traits)

new_habitat_resids3 <- resid(habitat_leaf_C_lm3)# obtain the residuals 

shapiro.test(new_habitat_resids3) # test the normality of the residuals

plot(habitat_leaf_C_lm3)

# Model 4: 

habitat_leaf_C_lm4 <-lm(log.phosph ~ new_habitat + C_Leaf, data = inga_traits)

new_habitat_resids4 <- resid(habitat_leaf_C_lm4)# obtain the residuals 

shapiro.test(new_habitat_resids4) # test the normality of the residuals

plot(habitat_leaf_C_lm4)

## Question d: ----
# In non-statistical terms, please describe your analysis and what the results 
# mean for the biology of Inga species


### Exercice 4: Generalised Linear Models ----

## Question a: ----
# Construct separate generalised linear models that individually test the influence of 
# leaf expansion rate and leaf trichome density on whether or not leaves produce the 
# defence chemical mevalonic acid (1 = yes, 0 = no).

# Restrict comparisons to a subset of data that has information for all the variables 

inga_traits_finite <- inga_traits %>% 
  filter(is.finite(Expansion)) %>% 
  filter(is.finite(Mevalonic_Acid)) %>% 
  filter(is.finite(Trichome_Density))

# MODEL 1: test the influence of leaf expansion rate on whether or not leaves
#          produce the defence chemical mevalonic acid


MA_LER_glm <- glm(Mevalonic_Acid ~ Expansion, data = inga_traits_finite, family = binomial)
summary(MA_LER_glm)

# Need to check: 
# - the sign of the coefficient for Expansion: the sign is positive: 

exp(0.07631-3.47421)

# - dividing the residual deviance (also given) by the residual degrees of freedom. 
# If this value is greater than one, then you have overdispersion.

30.528/24

# A little bit of overdispersion: 1.27

MA_null <- glm(Mevalonic_Acid ~ 1, data = inga_traits_finite, family = binomial)
AIC(MA_LER_glm, MA_null)


# MODEL 2: test the influence of leaf trichome density on whether or not leaves 
#          produce the defence chemical mevalonic acid


MA_LTD_glm <- glm(Mevalonic_Acid ~ Trichome_Density, data = inga_traits_finite, family = binomial)
summary(MA_LTD_glm)

# Need to check: 
# - the sign of the coefficient for Expansion: the sign is negative

exp(-0.1744 + 0.4942)
# - dividing the residual deviance (also given) by the residual degrees of freedom. 
# If this value is greater than one, then you have overdispersion.

29.844/24

# A little bit of overdispersion: 1.24

AIC(MA_LTD_glm, MA_null)

## Question b: ---- 
# construct a model incorporating both expansion rate and trichome density

MA_both_glm <- glm(Mevalonic_Acid ~ Trichome_Density + Expansion, data = inga_traits_finite, family = binomial)

MA_both_glm2 <- glm(Mevalonic_Acid ~ Trichome_Density * Expansion, data = inga_traits_finite, family = binomial)

AIC(MA_both_glm, MA_both_glm2, MA_null, MA_LER_glm, MA_LTD_glm)

summary(MA_both_glm)

22.865/23

summary(MA_both_glm2)
# Need to check: 
# - the sign of the coefficient for Expansion: the sign is positive

# - dividing the residual deviance (also given) by the residual degrees of freedom. 
# If this value is greater than one, then you have overdispersion.

22.865/23

# No overdispersion: 0.99

plot(MA_both_glm)

## Question c: ----
# Explain in simple terms what your results mean

## Question d: ----
# Make a figure that shows how one or both of your predictor variables influence 
# your response variable

# Create categories

inga_traits_finite <- inga_traits_finite %>%   # overwriting our data frame
  mutate(MA_status =   # creating our new column
           case_when(Mevalonic_Acid > 0 ~ "Presence",
                     Mevalonic_Acid == 0 ~ "Absence")
  )

# Make final plot 
plot(Mevalonic_Acid ~ Expansion, data = inga_traits_finite, pch = 16)
points(inga_traits_finite$Expansion, fitted(MA_LER_glm), col="blue")

# or using ggplot

predictions <- ggpredict(MA_LER_glm, terms = c("Expansion [all]")) 

(binomial_figure <- ggplot() +
  geom_point(data = inga_traits_finite, 
             aes(x = Expansion, y = Mevalonic_Acid)) +
  geom_point(data = predictions, 
             aes(x = x, y = predicted, colour = "#CD7054")) + 
    theme_ps() + 
    theme(legend.position = "none") + 
    ylab("Mevalonic Acid\n") +                             
    xlab("\nExpansion (in percent/day)") 
  )


(MA_boxplot <- ggplot(inga_traits_finite, aes(MA_status, Expansion)) + 
    geom_boxplot(aes(fill = MA_status)) +
    theme_ps() +
    scale_fill_manual(values = c("#43CD80", "#2E8B57")) +               # Adding custom colours
    scale_colour_manual(values = c("#43CD80", "#2E8B57")) +             # Adding custom colours
    ylab("Trichome density (number of hairs/cm2)\n") +                             
    theme(legend.position = "none")
)

theme_niwot <- function(){
  theme_bw() +
    theme(text = element_text(family = "Helvetica Light"),
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 18),
          axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
          plot.title = element_text(size = 18, vjust = 1, hjust = 0),
          legend.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.position = c(0.95, 0.15),
          legend.key = element_blank(),
          legend.background = element_rect(color = "black",
                                           fill = "transparent",
                                           size = 2, linetype = "blank"))
}

source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

(MA_violon <- ggplot(inga_traits_finite, aes(x = MA_status, y = Expansion)) +
    geom_violin(aes(fill = MA_status, colour = MA_status), alpha = 0.5) +
    # alpha controls the opacity
    geom_boxplot(aes(colour = MA_status), width = 0.2) +
    theme_niwot() + 
    ylab("Expansion (percent/day)\n") +   
    xlab("Mevalonic acid\n") +
    theme(legend.position = "none") 
  )

(MA_cloud <- ggplot(inga_traits_finite, aes(x = MA_status, y = Expansion, fill = MA_status)) +
    geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
    # alpha controls the opacity
    geom_point(aes(y = Expansion, color = MA_status),
               position = position_jitter(width = 0.15), size = 1, alpha = 0.9) +
    geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.8) +
    theme_niwot() + 
    ylab("Expansion (percent/day)\n") +   
    xlab("Mevalonic acid\n") +
    scale_fill_manual(values = c("#54FF9F", "#2E8B57")) +
    scale_colour_manual(values = c("#54FF9F", "#2E8B57")) +
    coord_flip() +
    theme(legend.position = "none") + 
    guides(fill = FALSE, color = FALSE)
)


### Save outputs ----

save_plot <- function(plot_name, # first put the plot object name
                      file_name = "plot", # give it a title 
                      width = 13, # set the width, heigh and dpi
                      height = 8, 
                      dpi = 150) {
  
  ggsave(
    paste0(file_name, ".png"), plot_name, width = width,  # save as png
    height = height, dpi = dpi) 
  
  ggsave(
    paste0(file_name, ".pdf"), plot_name, width = width, # save as pdf
    height = height, dpi = dpi
  )
}

save_plot(hist_leafarea, file_name = "Figures/Leaf_area_histogram", width = 13, 
          height = 8, dpi = 150) # use the save plot function created to save barblot

save_plot(hist_leafarea2, file_name = "Figures/Logged_leaf_area_histogram", width = 13, 
          height = 8, dpi = 150) # use the save plot function created to save barblot

save_plot(phosph_boxplot, file_name = "Figures/Phosphorous_habitat_boxplot", width = 13, 
          height = 8, dpi = 150) # use the save plot function created to save barblot

save_plot(P_C_scatter, file_name = "Figures/Phosphorous_Carbon_scatterplot", width = 13, 
          height = 8, dpi = 150) # use the save plot function created to save barblot

save_plot(MA_violon, file_name = "Figures/MA_violonplot", width = 13, 
          height = 8, dpi = 150) # use the save plot function created to save barblot

save_plot(MA_cloud, file_name = "Figures/MA_cloudplot", width = 13, 
          height = 8, dpi = 150) # use the save plot function created to save barblot

save_plot(binomial_figure, file_name = "Figures/MA_binomial", width = 13, 
          height = 8, dpi = 150) # use the save plot function created to save barblot


