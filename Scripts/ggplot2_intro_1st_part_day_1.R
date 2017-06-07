# R-workshop Tidyverse 2017-06-06 
library("tidyverse")
library("maps")
library("ggplot")
#Read in the data
# read_csv(file = "C:/R_WD_TEMP/Training/2017-06-06_R_tidyverse_workshop/2017-06-06_R_tidyverse_workshop/Data/gapminder-FiveYearData.csv")

gapminder <-read.csv(file= "Data/gapminder-FiveYearData.csv")
#plot1
ggplot(data = gapminder) +
  geom_point(mapping = aes(x = gdpPercap, y = lifeExp))

#plot2, adding a third dimension, coloring continent
ggplot(data = gapminder) +
  geom_jitter(mapping = aes(x = gdpPercap, y = lifeExp, color = continent))

#plot 3 Adding size to the colored dots according to population size
ggplot(data = gapminder) +
  geom_point(mapping = aes(x = log(gdpPercap), y = lifeExp, color = continent, size = pop))

#plot 4 Changing some looks of the graph
ggplot(data = gapminder) +
  geom_point(mapping = aes(x = log(gdpPercap), y = lifeExp), alpha=0.1, size=2, color="blue")

#plot 5 new geom (line)
ggplot(data = gapminder) +
  geom_line(mapping = aes(x = year, y = lifeExp, group=country, color = continent),size=1)

#plot 6 Boxplot
ggplot(data = gapminder) +
  geom_boxplot(mapping = aes(x = continent, y = lifeExp), size=1)

#plot 7 Jitter and Boxplot together, first one will overlay on the second one
ggplot(data = gapminder) +
  geom_jitter(mapping= aes(x = continent, y = lifeExp, color=continent))+
  geom_boxplot(mapping = aes(x = continent, y = lifeExp, color=continent), size=1)

#plot 8 Jitter and Boxplot. Having a general theme first then you can add specific things, every layer will inherit
ggplot(data = gapminder, 
       mapping = aes(x = continent, y = lifeExp, color=continent)) +
  geom_jitter()+
  geom_boxplot(size=0.6, alpha=0.8)


#plot 9 Change x axis to log of gdpcap. Jitter is spreading the points slightly (explaination of this came now)
#lm means a "lineal model" 
ggplot(data = gapminder, 
       mapping = aes(x = log(gdpPercap), y = lifeExp, color=continent)) +
  geom_jitter(alpha=0.1)+
  geom_smooth(method="lm")


#plot 10 hidig color from bottom layers, we place it only in the layer we need it. Remember we need to wrap it in mapping, coz R don't see the data otherwise
ggplot(data = gapminder, 
       mapping = aes(x = log(gdpPercap), y = lifeExp)) +
  geom_jitter(mapping=aes(color=continent), alpha=0.1)+
  geom_smooth(method="lm")

#Boxplot challenge life expectancy by year (need to as.factor or as.character year as it is cont variable and R does not know how to deal with that)
ggplot(data = gapminder) +
  geom_boxplot(mapping = aes(x = as.factor(year), y = lifeExp))

#Boxplot challenge life gdpPercap by year (need to as.factor or as.character year as it is cont variable and R does not know how to deal with that)
ggplot(data = gapminder) +
  geom_boxplot(mapping = aes(x = as.factor(year), y = log(gdpPercap)))

ggplot(data = gapminder) +
  geom_density2d(mapping = aes(x = lifeExp, y = log(gdpPercap)))

#Moving on to one more cool feature of ggplot. We have added several layers here.
#facet_wrap is plotting as many graphs as there are continent
ggplot(data = gapminder, mapping = aes(x=log(gdpPercap), y=lifeExp)) +
  geom_point()+
  geom_smooth() +
  scale_x_log10()+
  facet_wrap(~ continent)

#Assignment: facet acc to year, use a linear model for the smooth to create a straigh line
ggplot(data = gapminder, mapping = aes(x=log(gdpPercap), y=lifeExp)) +
  geom_point()+
  geom_smooth(method="lm") +
  scale_x_log10()+
  facet_wrap(~ year)

#Assignment: facet acc to continent, use a linear model for the smooth to create a straigh line.
#The slope is different here comparing the continents
ggplot(data = gapminder, mapping = aes(x=log(gdpPercap), y=lifeExp)) +
  geom_point()+
  geom_smooth(method="lm") +
  scale_x_log10()+
  facet_wrap(~ continent)

#This is to filter gapminder to only contain the reecords set to year 2007
#Shrinking the dataset to only one year, snapshot of 2007
#We are adding stat
ggplot(data=filter(gapminder, year==2007))+
  geom_bar(mapping=aes(x=continent), stat = "count")

#Further refine datasource, compare australia and new zeeland
filter(gapminder, year==2007, continent=="Oceania")

#But we changed it in the end to aSIA and flipped the axis
ggplot(data=filter(gapminder, year==2007, continent=="Asia"))+
  geom_col(mapping=aes(x=country, y=pop))+
  coord_flip()


#Final before-lunch graph. Also showing pop size divided by 3 to show in millions
#Playing around with labs(), adding title, subtitle and caption, etc
ggplot(data = gapminder, mapping = aes(x=log(gdpPercap), y=lifeExp, 
                                       color=continent, size=pop/10^6)) +
  geom_point()+
  scale_x_log10()+
  facet_wrap(~ year)+
  labs(title="Life Expectancy vs GDP per capita over time",
       subtitle="In the last 50 years, life expectancy as improved in most countries in the world.",
       caption="Source: Gapminder foundation, gapminder.com",
       x= "GDP per capita,in '000 USD'",
       y= "Life Expectancy in years",
       color="Continent",
       size="Population, in millions")


#ggsave remember last plot printed and saves it to disk
ggsave("my_fancy_plot.png")








