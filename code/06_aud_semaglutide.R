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

#switch color order
tmp <- colors[1]
colors[1] <- colors[2]
colors[2] <- tmp

# data based on https://www.nature.com/articles/s41380-024-02498-5/figures/1
data_a <- data.frame(groups=c('Overall','Women','Men','Overall','Women','Men'),
                   treatment =c('Semaglutide','Semaglutide','Semaglutide','Non-Semaglutide', 'Non-Semaglutide','Non-Semaglutide'),
                   values =c(0.28,0.24,0.29,0.48,0.35,0.65))
data_a$groups <- factor(data_a$groups, levels=c('Overall','Women','Men'))


data_b <- data.frame(groups=c('Overall','Women','Men','Overall','Women','Men'),
                     treatment =c('Semaglutide','Semaglutide','Semaglutide','Non-Semaglutide', 'Non-Semaglutide','Non-Semaglutide'),
                     values =c(13.0,11.6,13.9,20.4,21.5,26.5))
data_b$groups <- factor(data_b$groups, levels=c('Overall','Women','Men'))


# data on obesity
# from table 2
data_c <- data.frame(groups=c('Overall','Women','Men','Overall','Women','Men'),
                     treatment =c('Semaglutide','Semaglutide','Semaglutide','Non-Semaglutide', 'Non-Semaglutide','Non-Semaglutide'),
                     values =c(0.21,0.21,0.22,0.48,0.34,0.46))
data_c$groups <- factor(data_c$groups, levels=c('Overall','Women','Men'))


data_d <- data.frame(groups=c('Overall','Women','Men','Overall','Women','Men'),
                     treatment =c('Semaglutide','Semaglutide','Semaglutide','Non-Semaglutide', 'Non-Semaglutide','Non-Semaglutide'),
                     values =c(13.7,12.6,11.2,19.1,21.8,25.6))
data_d$groups <- factor(data_d$groups, levels=c('Overall','Women','Men'))


# 
# #### generate bar plot
# data_a %>% ggplot(aes(x=groups,
#            y=values,
#            fill=treatment)) + 
#   scale_fill_manual(values=colors)+
#   geom_col(stat='identity',position ='dodge',width=.8) + 
#   geom_text(aes(x = groups, group=treatment, y = values + 0.05, 
#                   label = paste0(values,'%'),
#                 family = "halyard_light"), 
#               color="black", size=5, position=position_dodge(0.9)) +  theme_ind2()+
#   ylab("%")+
# 
#   geom_signif(stat="identity",       tip_length = c(0.2, 0.04), manual=TRUE, vjust=55,
#               y_position = 1,
#               data = data.frame(x=c(0.75, 1.75,2.75), xend=c(1.25, 2.25,3.25),
#                                 y=c(0.66, 0.55,0.8), annotation=c(" 0.20%", "0.11%","0.36%"),
#                                 treatment=c('Semaglutide','Non-GLP-1RAs',NA)),
#               aes(x=x,xend=xend, y=y, yend=y, annotation=formatC(annotation),textsize=5,  family="halyard_light"))+
#   labs(title = "Percent of patients receiving a diagnosis of\n cannabis use disorder (CUD)  after beginning\n treatment with an anti-obesity medication") +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.title=element_blank(),
#         axis.title.y= element_text(), 
#         axis.text.y  = element_text(family = "halyard_light"),
#         axis.text.x  = element_text(family = "halyard"),
#         text = element_text(family = "halyard_light"),
#         legend.text=element_text(famil='halyard_light')
# scale_y_continuous(limits = c(0, 0.9))  



############# generate 'naked' graph


data_a %>% ggplot(aes(x=groups,
                    y=values,
                    fill=treatment)) + 
  scale_fill_manual(values=colors)+
  geom_bar(stat='identity',position ='dodge',width=.8) + 
  theme_ind2()+
  ylab("%")+
 
  theme(plot.title = element_text(hjust = 0.5),
        legend.title=element_blank(),
        axis.title.y= element_text(), 
        axis.text.y  = element_text(family = "halyard_light"),
        axis.text.x  = element_text(family = "halyard"),
        text = element_text(family = "halyard_light"),
        legend.text=element_text(family='halyard_light')
  ) 


