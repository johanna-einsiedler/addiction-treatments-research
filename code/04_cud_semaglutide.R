library(ggplot)
library(readxl)
library(ggpubr)
library(scales)
library(dplyr)
library(ggrepel)
library(ggtext)
library(stringr)

setwd('/Users/htr365/Documents/Side_Projects/13_war_on_drugs/github/')
source('utils/clean_WHO.R')

# import theme & set base parameters
source('utils/theme.R')
base_size <- define_base_size()
colors <- set_colors()

data <- data.frame(groups=c('Overall','Women','Men','Overall','Women','Men'),
                   treatment =c('Semaglutide','Non-GLP-1RAs','Semaglutide','Non-GLP-1RAs','Semaglutide','Non-GLP-1RAs'),
                   values =c(28,24,29,48,35,65))
data$groups <- factor(data$groups, levels=c('Overall','Women','Men'))

data %>% ggplot(aes(x=groups,
           y=values,
           fill=treatment)) + 
  scale_fill_manual(values=colors)+
  geom_bar(stat='identity',position='dodge') + 
  geom_text(aes(x = groups, group=treatment, y = values + 5, 
                  label = paste0(values,'%')), 
              color="black", size=5, position=position_dodge(.9), hjust=.5) +  theme_ind2()+
  ylab("[%]")+
labs(title = "Percent of patients receiving a diagnosis of\n cannabis use disorder (CUD)  after beginning\n treatment with an anti-obesity medication") +
theme(plot.title = element_text(hjust = 0.5),
      legend.title=element_blank())

