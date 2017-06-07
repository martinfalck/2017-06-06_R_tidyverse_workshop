#Introduction to vectors and lists
x <- 5 * 6
x

#R can check if x is a vector with is.vector, is x an vector? (output should be true)
is.vector(x)

#say we want a second number, after 30, this is where [] come into play
#in our vector x, second position
x[2] <- 31
x

#Trying something not intuitive, it filled two values with NA values!
x[5] <- 44
x

#overwriting x with something else!, 1:4 will put integer values from 1 to 4 including both numbers
x <- 1:4
x
#y based on the value x, in the sense that it is x squared. It applies to the power of two to each
#elemen to the vector seperately
y <- x^2
y


#anotherh feature of R that is uaseul, is recycling of vectors
x <- 1:5

y <- 3:7

#Drop position number 5 by using -
z <- y[-5]

#Now adding them together again show the recycling, for the y we dropped pos 5, so it will add first 4 
#positions but then it will go back to pos 1 and add it to pos 5 for x.
x+z

#cohersion, c, when we have vectors and we want to put them into one object
b <- str(c("Hello", "Workshop", "Participants"))
b

b <- c(9:11, 200, x) 
sort.int(b)

#What happens if we combine more delicate elements, a word and integers, it will all be string
c("something", pi, 2:4, pi > 3)

#Creating a vector with 10 random numbers for the normal distribution (mean is 0, SD is 1?)
w <- rnorm(10)
seq_along(w)
#checking what numbers are negative, but this will give you the positions for these numbers
which(w < 0)

#this will give you the nunmbers in vector w that are negative, not the positions
w[w<0]

#Can drop positions with -c(positions)
w <- rnorm(10)
w
w[-c(2,5)]

#a list can retain all the data
b <- list("Something", pi, 2:4, pi > 3)
#look at the structure
str(b)

#for the list you can also specify names
x <- list(vegetable = "Cabbage",
     number = pi,
     series = 2:4,
     telling = pi > 3)

str(x)

#what if im only interested in vegetable? How do I get what vegetable is?
x$vegetable
#the same as if you know the pos (1), x[1], but square brackers return the list and the dollar 
#return the content

#SIngle square brackers return a list, double square brackets return the content. 
#Single brackers return the phone in the package, the double removes the packaing and give you the phone
str(x[[3]])


mod <- lm(lifeExp ~ gdpPercap, data=gapminder_plus)
str(mod)

#Missed first 10 minutes since they took 45 minute lunch w/o specify. They are grabbing things inside vectors?
#to mod, was assigned a lineal model? (lm) of lifeexpectancy vs gdpPercap, using the specified data

#Drill down to some type of complexity, and pull out a number, inspect with str() 
mod$qr$qr[1,1]








#the summary is an interation on all the elements
gapminder_plus %>% 
  group_by(continent) %>% 
  summarize(mean_le=mean(lifeExp),
            min_le=min(lifeExp),
            max_le=max(lifeExp))

gapminder_plus %>% 
  ggplot()+
  geom_line(mapping=aes(x=year, y=lifeExp, color=continent, group=country))+
  geom_smooth(mapping=aes(x=year, y=lifeExp),method="lm", color="black")+
  facet_wrap(~continent)

#We need to find the countries that are outliers
#Fitting a model manually

#REsiduals are numbers that dont fit the lineal line, outliers will be very large
by_country <- gapminder_plus %>% group_by(continent, country) %>% 
  nest()

#this one below contains data for Afghanistan
by_country$data[[1]]
#Nest function is new here. 

#The beuty is that we can fit ind models to all these

#map(list, function)
#map lets you apply a function to a certain list
#example map(1:3, sqrt)

model_by_country <- by_country %>% 
  mutate(model=purrr::map(data, ~lm(lifeExp~year, data=.x))) %>%  #we need to pass data column to the map func
  mutate(summr=purrr::map(model, broom::glance)) %>% 
  unnest(summr) %>% arrange(r.squared) %>% filter(r.squared<0.6)

model_by_country


by_country %>% 
  mutate(model=purrr::map(data, ~lm(lifeExp~year, data=.x))) %>%  #we need to pass data column to the map func
  mutate(summr=purrr::map(model, broom::glance)) %>% 
  unnest(summr) %>% arrange(r.squared) %>% filter(r.squared<0.3) %>% 
  select(country) %>% left_join(gapminder_plus) %>% 
  ggplot()+
  geom_line(mapping=aes(x=year, y=lifeExp, color=country, group=country))
 


#last challenge
#life expectancy dependent on gdp per capita, does money make you live longer?
#lifeExp ~gdpPercap
by_country %>% 
  mutate(model=purrr::map(data, ~lm(lifeExp~log(gdpPercap), data=.x))) %>%  
  mutate(summr=purrr::map(model, broom::glance)) %>% 
  unnest(summr) %>% arrange(r.squared) %>% filter(r.squared<0.1) %>% 
  select(country) %>% left_join(gapminder_plus) %>% 
  ggplot()+
  geom_point(mapping=aes(x=log(gdpPercap), y=lifeExp, color=country, group=country, size=1))

#save into special object
saveRDS(by_country, "by_country_tibble.rds")
#how to load dimenisonal data, it is possible to save anything!
my_fresh_by_country <- readRDS("by_country_tibble.rds")
my_fresh_by_country