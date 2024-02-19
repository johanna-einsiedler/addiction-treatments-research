library(ggplot)
library(readxl)
library(ggpubr)
library(scales)
library(cowplot)


setwd('/Users/htr365/Documents/Side_Projects/13_war_on_drugs/github/')
source('utils/clean_WHO.R')

# import theme & set base parameters
source('utils/theme.R')
base_size <- define_base_size()
colors <- set_colors()

# read in data
df19 <- read_excel('data/WHO/ghe2019_yll-2019-country.xlsx',sheet=2, skip=7)


#a. Opioid use disorders
#b. Cocaine use disorders
#c. Amphetamine use disorders
#d. Cannabis use disorders
#e. Other drug use disorders
# need to add 2 to level due to column structure
df19_level4 <- get_country_data(df19,'USA',level=6) %>% mutate(year=2019)
# alcohol disorders
df19_level3 <- get_country_data(df19,'USA',level=5) %>% mutate(year=2019)

# get data for different drug use disorders
df_drugs <- df19_level4 %>% filter(cause %in% c('Opioid use disorders',
                                    'Cocaine use disorders',
                                    'Amphetamine use disorders',
                                    'Other drug use disorders')) %>%
  mutate(type='Drug \nAbuse')

# get data for alcohol disorders
df_alc <- df19_level3 %>% filter(cause =='Alcohol use disorders') %>%
  mutate(type='Alcohol')

# COVID 19 - first year of pandemic
# The data reflect the period 1 January 2020 through 31 January 2021
# Years of life lost associated with COVID-19 deaths in the USA during the first year of the pandemic
# data from https://doi.org/10.1093/pubmed/fdab123
# Table 1Number of deaths and YLLs by jurisdiction, first line
# 119 deaths per 10,000 -> 1190 deaths per 100,000
df_covid <- data.frame(cause='COVID-19*',deaths = 1190,year=2020) %>% mutate(type='Other \nDiseases')

df <- rbind(df_drugs,df_alc,df_covid)
df$cause <- factor(df$cause, levels = levels(reorder(df$cause, df$deaths)))
#df$cause <- factor(df$cause, levels = c("Other drug use disorders",levels(df$cause)[!levels(df$cause) %in% "Other drug use disorders"]))

df <- df %>% arrange(cause)

p <- df %>% ggplot(aes(x=cause,y=deaths,group=type,fill=type)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values=colors[1:3])+
  theme_ind() + 
  #coord_flip() +
  scale_y_continuous(labels=comma)+
  labs(title = "Opiod use disorders in 2019 caused more than  \ntwice as many YLLs than the first year of the COVID-19 pandemic", 
       subtitle ="% change in DALYs per 100,000 population from 2000 to 2019",
       x = "")+
  scale_x_discrete(labels=function(x) str_wrap(x, width = 10))+
  facet_grid(~type,scales='free',space='free')+
  geom_text(aes(x=cause,y=deaths,label=comma(round(deaths))),
            vjust=0.5,nudge_y=100,size=base_size/3)+
  theme(panel.grid.major.y=ggplot2::element_line(color = "#cbcbcb"),
        legend.justification = c(0, 1))+
  theme(strip.background = element_blank(),
        axis.text.x = element_text(size=base_size*.8),
        strip.text = element_text(size=base_size)) +
  geom_text(aes(x=0,y=-1200,vjust=0, hjust=0,label='*COVID-19 deaths for the period of Jan 1st, 2020 to Jan 31st, 2021'),data=df %>% filter(type=='Alcohol'))+
  #annotate("text", x = 0, y =-800, hjust=0, label = '*COVID-19 deaths for the period of Jan 1st,2020 to Jan 31st, 2021')+
  coord_cartesian(ylim=c(0,3000),clip = "off") +
  theme(plot.margin = margin(20, 20, 40, 20)) 
p

source('utils/theme.R')

# save plot
finalise_plot(p,
              source=str_wrap('Sources: WHO; Troy Quast, Ross Andel, Sean Gregory, Eric A Storch, Years of life lost associated with COVID-19 deaths in the USA during the first year of the pandemic, Journal of Public Health, Volume 44, Issue 1, March 2022, Pages e20–e25, https://doi.org/10.1093/pubmed/fdab123',
                              width=150),
              save_filepath = 'figures/04_yll_comparison.png',
              width_pixels =640,
              height_pixels =550)


