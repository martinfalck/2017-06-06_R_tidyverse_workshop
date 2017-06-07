library("readxl")
raw_fert <- read_excel(path = "Data/indicator undata total_fertility_2.xlsx", sheet="Data")
raw_infantMort <- read_excel(path = "Data/indicator gapminder infant_mortality_2.xlsx")
raw_fert

#we can use the gather fuction to tidy up our fetility data
fert <- raw_fert %>% 
  rename(country=`Total fertility rate`) %>% 
  gather(key=year, value=fertility,-country) %>% 
  mutate(year=as.integer(year))
fert
#we have now collected all years s