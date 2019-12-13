
library(plyr)
library(tidyverse)
library(gplots)
library(corrplot)
library(GGally)

######### HEPATATIS ###############

# rawhepatitis <- read_csv("./workshops/4_data_visualization/material/hepatitis.csv", na = "?")
# glimpse(hepatitis)
# 
# rawhepatitis$Class <- revalue(factor(rawhepatitis$Class), c("1"="DIE", "2"="LIVE") )
# rawhepatitis$SEX <- revalue(factor(rawhepatitis$SEX), c("1"="male","2"="female"))
# 
# clean_hepatitis <- rawhepatitis %>% mutate_at(vars(STEROID:VARICES, HISTOLOGY), as.factor) %>% 
#   mutate_at(vars(STEROID:VARICES, HISTOLOGY), funs(case_when(. == 1 ~ "No", . ==  2 ~ "Yes" )))
# 
# clean_hepatitis %>% write_csv("./workshops/4_data_visualization/material/clean_hepatitis.csv")

#### ToothGrowth

# Logistic model to use as example
model <- glm(supp ~ len + dose, family = "binomial", data = ToothGrowth)
summary(model)

# Extract the estimates and the confidence intervals into a tibble
toforest <- cbind("Beta" = coef(model), confint(model)) %>%
  # Rownames to specify in which column the row names will be stored
  as_tibble(., rownames = "Variable")

toforest %>% ggplot(aes(y = Variable, x = Beta, xmin = `2.5 %`, xmax = `97.5 %`)) +
  # Add points for the estimates
  geom_point(color = 'black') +
  # Add horizontal errorbar using the xmin and xmax specified 
  geom_errorbarh(height = .05) +
  # Change the lower and upper limits of the plot
  scale_x_continuous(limits=c(-5,5), name='Estimate') +
  ylab('Variable') +
  # Add vertical line
  geom_vline(xintercept=0, color='black', linetype='dashed') +
  theme_minimal()
  


#### Hepatitis
clean_hepatitis <- read_csv("./workshops/4_data_visualization/material/clean_hepatitis.csv")
clean_hepatitis <- clean_hepatitis %>% mutate_if(is.character, as.factor)


hep_mat_factors <- as.matrix(clean_hepatitis %>% select_if(is.factor) %>% mutate_all(as.numeric))

heatmap(hep_mat_factors, scale="none")

hep_mat_numeric <- clean_hepatitis %>% select_if(is.numeric)  %>% as.matrix()
heatmap(hep_mat_numeric, scale="col")


cormat <- cor(hep_mat_factors, use = "pairwise.complete.obs", method = "pearson")
corrplot(cormat, type = "lower", order = "AOE")
corrplot.mixed(cormat, lower.col = "black", upper = "color", number.cex = .7)

