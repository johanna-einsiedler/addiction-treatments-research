library(ggplot)
library(readxl)
library(ggpubr)
library(scales)

setwd('/Users/htr365/Documents/Side_Projects/13_war_on_drugs/github/')
source('utils/clean_WHO.R')

# import theme & set base parameters
source('utils/theme.R')
base_size <- define_base_size()
colors <- set_colors()

# read in data
df19 <- read_excel('data/WHO/ghe2019_dalys-2019-country.xlsx',sheet=2, skip=2)
df00 <- read_excel('data/WHO/ghe2019_dalys-2000-country.xlsx',sheet=2, skip=2)

df19 <- get_country_data(df19,'USA',level=3) %>% mutate(year=2019)
df00<- get_country_data(df00,'USA',level=3) %>% mutate(year=2000)
df <- rbind(df00,df19)

#df <- df %>% mutate(drug = ifelse(cause==" Drug use disorders",1,0))
df_top10 <- df %>% filter(cause %in% c(df19  %>% arrange(desc(deaths)) %>%  slice(1:10) %>% select(cause))$cause)


# calculate difference from 2000 to 2019
diff_labels <- df_top10 %>% spread(year,deaths) %>% group_by(cause) %>% mutate(diff = `2019`/`2000` -1)
diff_labels <- diff_labels %>% mutate(year=ifelse(diff>0,2019,2000),
                                      deaths =ifelse(diff>0,`2019`,`2000`),
                                      label = ifelse(diff>0,paste0("+",round(diff*100),"%"),paste0("",round(diff*100),"%") ))

#create plot
df_top10 <- df_top10 %>% left_join(diff_labels %>% select(cause,diff),by='cause')
df_top10 %>% arrange(year,desc(diff))
df_top10$cause<-factor(df_top10$cause, levels = rev(c(df19  %>% arrange(desc(deaths)) %>%  slice(1:10) %>% select(cause))$cause))



p <- df_top10 %>% arrange(year,desc(diff)) %>% ggplot(aes(x=deaths, y = cause, label = round(deaths))) +
geom_line(aes(group = cause),size=1) +
geom_point(aes(color = factor(year)),size=2)+
  labs(title = "Drug abuse ist the fastest growing cause \nof DALYs in the U.S.", 
       subtitle ="% change in DALYs per 100,000 population from 2000 to 2019",
       x = "")+
  # right side labels
  geom_text(data = diff_labels, aes(color = factor(year), label = label,x=deaths),
                                                size = base_size/4, hjust = -.5, vjust=-.4, show.legend=FALSE) +
  theme_ind(base_size) +
  scale_color_manual(values=colors[1:2]) + 
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) + 
  scale_y_discrete(expand = c(0.1, 0.1)) + 
  theme(panel.grid.major.y=  ggplot2::element_line(color = "#cbcbcb")) 

p
# save plot
finalise_plot(p,
              source='Source: WHO',
              save_filepath = 'figures/02_year_comparison_dalys.png',
              width_pixels =640,
              height_pixels =400)



############################
# Lollipop Plot version
############################



p2 <- df_top10 %>%  ggplot(aes(x = reorder(cause, -deaths), y = deaths)) + 
  geom_segment(aes(x = reorder(cause, -deaths),
                   xend = reorder(cause, -deaths),
                   y = 0, yend = deaths),
               color = colors[1], lwd = 1) +
  geom_point( size=4, aes(fill=factor(year),color=factor(year),shape=factor(year)),stroke=1) + 
  scale_fill_manual(values=c('white',colors[1])) +
  scale_color_manual(values=c(colors[1],colors[1])) +
  scale_shape_manual(values=c(21,16))+
  xlab("Group") +
  ylab("") +
  coord_flip() +
  theme_ind()+ 
  labs(title = "Drug abuse ist the fastest growing cause \nof DALYs in the U.S.", 
       subtitle ="% change in DALYs per 100,000 population from 2000 to 2019",
       x = "") + 
  scale_y_continuous(limits=c(0,max(df$deaths)),expand = c(0.1, 0.1),labels=comma)+
  # right side labels
  geom_text(data = diff_labels, aes(color = factor(year), label = label,y=deaths),
            size = base_size/4, hjust = -0.5, vjust=.5, show.legend=FALSE) +
  #scale_shape_manual(values = c(21, 16)) + 
  theme(axis.text.y = element_text(vjust = -0.9,
                                   hjust=0,
                                   size=base_size*.6,
                                   margin = margin(l = 0,r=-235)),
        panel.grid.major.x=ggplot2::element_line(color = "#cbcbcb"))+
  theme(plot.margin = margin(20, 20, 20, 30), # Increase margins on all sides by 20 units
        legend.justification = c(0.1, 1))

p2

# save plot
finalise_plot(p2,
              source='Source: WHO',
              save_filepath = 'figures/02_year_comparison_dalys_lollipop.png',
              width_pixels =640,
              height_pixels =450)

