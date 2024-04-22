library(ggplot2)
library(readxl)
library(ggpubr)
library(scales)
library(dplyr)
library(ggrepel)
library(ggtext)
library(stringr)
library(ggsignif)
library(showtext)

setwd('/Users/htr365/Documents/Side_Projects/13_war_on_drugs/github/')
source('utils/clean_WHO.R')

font_add('halyard_light','/Users/htr365/Documents/Side_Projects/13_war_on_drugs/github/halyard-font/Halyard-Text-Book.ttf')
font_add('halyard','/Users/htr365/Documents/Side_Projects/13_war_on_drugs/github/halyard-font/Halyard-Text-Medium.ttf')

showtext_auto()

# import theme & set base parameters
source('utils/theme.R')
base_size <- define_base_size()
colors <- set_colors()



data <- data.frame(groups =c('All','All','People with prior history of depression or suidice atempt','People with prior history of depression or suidice atempt'),
  treatment=c('Treated with Gliptins','Treated with GLP-1RAs','Treated with Gliptins','Treated with GLP-1RAs'),
                   sample_size = c(373041,372944,88325,88325),
                   suicides = c(106,250,68,180))

############# generate 'naked' graph


data %>% ggplot(aes(x=groups,
                      y=suicides/sample_size,
                      fill=treatment)) + 
  scale_fill_manual(values=colors)+
  geom_bar(stat='identity',position ='dodge',width=.8) + 
  theme_ind2()+
  ylab("%")+
  scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, "foo" , " "),
                                        width = 30))+
  theme(plot.title = element_text(hjust = 0.5),
        legend.title=element_blank(),
        axis.title.y= element_text(), 
        axis.text.y  = element_text(family = "halyard_light"),
        axis.text.x  = element_text(family = "halyard"),
        text = element_text(family = "halyard_light"),
        legend.text=element_text(family='halyard_light')
  ) 


