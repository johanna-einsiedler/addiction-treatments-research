# clean WHO data
get_country_data <- function(df,country,level){
  # select only data for USA
  df <- df %>% select(1:8,eval(country))
  
  # only keep lonly from one specific level
  df <- df %>% filter(`...1`=='Persons') %>%  select(level+1,eval(country)) 
  names(df) <- c('cause','deaths')
  
  df <- df %>% drop_na(cause) %>% # drop higher levels
    mutate(deaths = as.numeric(deaths)) %>% # turn deaths into numeric values
    mutate(cause = gsub("[0-9.]","",cause)) # remove numbers and dots from names of diseases
  
  #df$cause<- factor(df$cause, levels = df$cause[order(df$deaths)]) # turn cause into factor variable & order by death count
  return(df)
}