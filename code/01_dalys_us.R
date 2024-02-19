library(ggplot)
library(readxl)
library(ggpubr)
library(scales)

setwd('/Users/htr365/Documents/Side_Projects/13_war_on_drugs/github/')

# import theme & set base parameters
source('utils/theme.R')
base_size <- define_base_size()
colors <- set_colors()

# read in data
df <- read_excel('data/WHO/ghe2019_dalys-2019-country.xlsx',sheet=2, skip=2)

# select only data for USA
df <- df %>% select(1:5,USA)

# only keep level 3 for all people
df <- df %>% filter(`...1`=='Persons') %>%  select(4,USA) 
names(df) <- c('cause','deaths')

df <- df %>% drop_na(cause) %>% # drop higher levels
  mutate(deaths = as.numeric(deaths)) %>% # turn deaths into numeric values
  mutate(cause = gsub("[0-9.]","",cause)) # remove numbers and dots from names of diseases

df$cause<- factor(df$cause, levels = df$cause[order(df$deaths)]) # turn cause into factor variable & order by death count

# color drug use in different color
df <- df %>% mutate(drug = ifelse(cause==" Drug use disorders",1,0))


# Create the bar plot
bar_plot <- df %>% arrange(desc(deaths)) %>%  slice(1:10) %>%  # only take top 10
  ggplot(aes(x = factor(cause), y = deaths,group=cause,fill=factor(drug))) + #specify attributes
  geom_bar(stat = "identity", width=0.7,color=NA) + # define bars 
  labs(title = "10 most frequent causes of DALYs in the U.S.", 
       subtitle ="DALYs per 100,000 population",
       x = "")+
       #y = "DALYs per 100,000 population") +
  scale_y_continuous(labels = label_comma())+
  coord_flip() +
  geom_text(aes(y=0, label = comma(deaths)), hjust=0,nudge_y=200, size = base_size/4,
            color='white',fontface = 2) + theme_ind() +
  scale_fill_manual(values=colors[1:2]) +
  theme(legend.position = "none",
        panel.grid.major.y =element_blank(),
        panel.grid.major.x=  ggplot2::element_line(color = "#cbcbcb")
        )

bar_plot

# save plot
finalise_plot(bar_plot,
              source='Source: WHO',
              save_filepath = 'figures/01_basic_barplot_dalys.png',
              width_pixels =640,
              height_pixels =350)


