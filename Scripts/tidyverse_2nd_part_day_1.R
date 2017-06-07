#we are loading a tinyverse package
#USe dplyr to manipulate data frames
#select() choose variables from a data frame
#filter() choose data upon a given input
#group_by() grouping stuff
#mutate() create new variables

library(tidyverse)
gapminder <-read.csv(file= "Data/gapminder-FiveYearData.csv")

#a function that repeat a thing severa times
rep("This is an example",times=3)

#This will then pipe the string to rep method that will use the string three times as the above example
"This is an example" %>% rep(times=3)

#Restrict the datafram to only three selected columns
year_country_gdp <-select(gapminder,year,country,gdpPercap)
head(year_country_gdp)

year_country_gdp <-gapminder %>% select(year, country, gdpPercap)
head(year_country_gdp)

#Sending gapminder to filter function, selecting year 2002, then sending this to ggplot doing some mapping,
#then boxplotting this
gapminder %>% 
  filter(year==2002) %>% 
  ggplot(mapping = aes(x=continent, y=pop))+
    geom_boxplot()

#further restrict
year_country_gdp_europe <- gapminder %>% 
  filter(continent=="Europe") %>% 
  select(year,country,gdpPercap)


#Challenge 1, filter for Norway and then we want to look at year, life expectancy and gdp per capita
year_lifeExp_gdp_Norway <- gapminder %>% 
  filter(country=="Norway"|country=="Sweden") %>% 
  select(country,year,lifeExp,gdpPercap)
year_lifeExp_gdp_Norway

#Using the group by function, by continent (will create 5 groups)
gapminder %>% 
  group_by(continent)

#This summarize function uses the grouping we made and we apply mean to it
gapminder %>% 
  group_by(continent) %>% 
  summarize(mean_gdp_percap=mean(gdpPercap))

#plotting above
gapminder %>% 
  group_by(continent) %>% 
  summarize(mean_gdp_percap=mean(gdpPercap)) %>% 
  ggplot(mapping=aes(x=continent, y=mean_gdp_percap))+
  geom_point(size=10, color="blue", alpha=0.8)


#Challenge 2
gapminder %>%
  filter(continent=="Asia") %>% 
  group_by(country) %>% 
  summarize(mean_lifeExp=mean(lifeExp)) %>% 
  #another filter trick, filter to minimum of mean and maximum, or is the bar | in this case
  filter(mean_lifeExp==min(mean_lifeExp)|mean_lifeExp==max(mean_lifeExp))

#visual solution
gapminder %>%
  filter(continent=="Asia") %>% 
  group_by(country) %>% 
  summarize(mean_lifeExp=mean(lifeExp)) %>% 
  ggplot(mapping=aes(x=country, y=mean_lifeExp))+
  geom_point()+
  coord_flip()

#introducing mutate function, summary for every continent and year
gapminder %>%
  mutate(gpd_billion=gdpPercap*pop/10^9) %>% 
  group_by(continent,year) %>% 
  summarize(mean_gdp_billion=mean(gpd_billion))

#visualise the average global life expectancy on a map of the world

#creating the input for below, taking gapminder data, grouping by country and summarizing by the mean of life Expectancy
gapminder_country_summary <- gapminder %>% 
  group_by(country) %>% 
  summarize(mean_lifeExp = mean(lifeExp))

library(maps)
#this dataframe use region instead of country, so we have to rename the variables
map_data("world") %>% 
  rename(country=region) %>%
  left_join(gapminder_country_summary, by="country") %>% 
  ggplot()+
  geom_polygon(aes(x=long, y=lat, group=group, fill = mean_lifeExp))+
  scale_fill_gradient(low="blue",high="red")+
  coord_equal()
  




