
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

clean_hepatitis <- read_csv("./workshops/4_data_visualization/material/clean_hepatitis.csv")
clean_hepatitis <- clean_hepatitis %>% mutate_if(is.character, as.factor)


hep_mat_factors <- as.matrix(clean_hepatitis %>% select_if(is.factor) %>% mutate_all(as.numeric))

heatmap(hep_mat_factors, scale="none")

hep_mat_numeric <- clean_hepatitis %>% select_if(is.numeric)  %>% as.matrix()
heatmap(hep_mat_numeric, scale="col")


cormat <- cor(hep_mat_factors, use = "pairwise.complete.obs", method = "pearson")
corrplot(cormat, type = "lower", order = "AOE")
corrplot.mixed(cormat, lower.col = "black", upper = "color", number.cex = .7)

