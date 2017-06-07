#Recap from yesterday, we did not have time for Git version control
# GAPMINDER PLUS 
download.file(url = "https://raw.githubusercontent.com/dmi3kno/SWC-tidyverse/master/data/gapminder_plus.csv", 
              destfile = "Data/gapminder_plus.csv")
#Load some libraries
library("tidyverse")
#
#Read in this dataset, assign it to dataframe gapminder.plus
#
gapminder_plus <- read.csv(file="Data/gapminder_plus.csv")

gapminder_plus %>% 
  filter(continent=="Africa", year==2007) %>% 
  #make a new column with dead babies, so multiply pop (/1000) with babies dead (per thousand pop)
  mutate(babiesDead=infantMort*pop/1e3) %>% 
  #filter for countries where more than 2 million babes die
  filter(babiesDead>2e6) %>% 
  select(country) %>% 
  #limit our dataset to only these countries, you could also just make a new dataset to save this info
  left_join(gapminder_plus) %>% 
  #re_add the babiesdead column with mutate
  mutate(babiesDead=infantMort*pop/1e3)

#semi_join(x, y) can drop 
#inner_join(x, y) to join two datasets but only overlapping values
#full_join(x,y) to fully join two datasets, but non-overlapping values will have NA values
#left_join(x, y) retaining everything we have on the left side, sacrificing on the right side
# example: in above example we retained all countries in the short list we filtered
#right_join(x, y) 
   
gapminder_plus %>% 
  filter(continent=="Africa", year==2007) %>% 
  #make a new column with dead babies, so multiply pop (/1000) with babies dead (per thousand pop)
  mutate(babiesDead=infantMort*pop/1e3) %>% 
  #filter for countries where more than 2 million babes die
  filter(babiesDead>2e6) %>% 
  select(country) %>% 
  #limit our dataset to only these countries, you could also just make a new dataset to save this info
  left_join(gapminder_plus) %>% 
  #re_add the babiesdead column with mutate
  mutate(babiesDead=infantMort*pop/1e3,
         gdp_bln=gdpPercap*pop/1e9, pop_mln=pop/1e6) %>% 
  #for faceting (multiple columns) we need to get the col names into variable (but not year and country)
  #key is the common names for all those items
  #value is the value for keys
  #drop continent, population and babiesdead, from the data
  select(-continent, -pop, -babiesDead) %>% 
  gather(key=variables, value=columnValues, -c(country, year)) %>% 
  #dot (.) is a placeholder for all above data, or if no dot, into the first place
  ggplot() +
  geom_text(data=. %>% filter(year==2007) %>% group_by(variables) %>%
              mutate(max_value=max(columnValues)) %>% 
              filter(columnValues==max_value),
            aes(x=year, y=columnValues, label=country, color=country))+
  geom_line(mapping=aes(x=year, y=columnValues, color=country))+
  facet_wrap(~variables, scales="free_y")+
#the y-axis is very big however (fixed above with scales)
  labs(title="Final Project",
       subtitle="Some subtitle",
       caption="Caption",
       #supress the title of y axis
       y=NULL,
       x="Year")+
  #learning something new: theme
  #can specify a theme for the graph
  theme_bw()+
  #lots of more things that can be done with theme!
  theme(legend.position = "none")



