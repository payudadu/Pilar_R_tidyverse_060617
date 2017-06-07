download.file(url = "http://docs.google.com/spreadsheet/pub?key=phAwcNAVuyj0NpF2PTov2Cw&output=xlsx", 
              destfile = "data2/indicator gapminder infant_mortality.xlsx")

download.file(url = "http://docs.google.com/spreadsheet/pub?key=phAwcNAVuyj0NpF2PTov2Cw&output=xlsx", 
              destfile = "data2/indicator undata total_fertility.xlsx")

library("readxl")

raw_fert <- read_excel(path = "data3/indicator undata total_fertility.xlsx", sheet = "Data")
raw_infantMort <- read_excel(path = "data3/indicator gapminder infant_mortality.xlsx")

#Gapminder está en formato tidy
gapminder <- read_csv(file = "data3/gapminder-FiveYearData.csv")

gapminder

#vamos a poner los otros datasets en formato tidy

#la fertilidad no está en formato tidy si lo miras así, para cada dato hay una columna
raw_fert

#gather es la función para hacer un tidy, hay que darle tres parámetros:
#name of the id variable, here key; name for the observation variable, here value
#third, dice que se lo hagas a todas las columnas menos country (escribir -country) 

fert <- raw_fert %>%
  rename(country=`Total fertility rate`) %>%
  gather(key=year, value=fert, -country) %>%
  mutate(year=as.integer(year))
fert
  
  

download.file(url = "https://raw.githubusercontent.com/dmi3kno/SWC-tidyverse/master/data/gapminder_plus.csv", 
              destfile = "Data/gapminder_plus.csv")
  

library("tidyverse")
gapminder_plus <- read_csv(file = "Data/gapminder_plus.csv")
  
gapminder_plus
  
  
#African countries with over 2e6 baby deaths in 2007  

filter(gapminder_plus, year==2007, continent=="Africa")
  
filter(gapminder_plus, continent=="Africa")


#funcióm mutate, sirve para crear nuevas variables, en este caso con una 
#ecuación entre las variables disponibles

gapminder_plus %>%
  mutate(TotinfantMort=(infantMort*pop/10^3)) %>%
  head()


gapminder_plus %>%
  filter(continent=="Africa") %>%
  group_by(country) %>%
  summarise(mean_lifeExp=mean(lifeExp)) %>%
  filter(mean_lifeExp==min(mean_lifeExp)|mean_lifeExp==max(mean_lifeExp))

#Ahora la solución
#•Para hacer el gráfico hay que usar facet y gather. 
#Usar el nombre de columna como variable
#%>% View()
#Puedes seleccionar variables como c(fert, infantMort) o elegir lo que no quieres
#que se seleccione, select(-country) o -c(country, year)


gapminder_plus %>% 
  filter(continent=="Africa", year==2007) %>%
  mutate(babiesDead=infantMort*pop/10^3) %>%
  filter(babiesDead>2e6) %>%
  select(country) %>%
  left_join(gapminder_plus) %>%
  mutate(babiesDead=infantMort*pop/10^3, 
         gdp_bln=gdpPercap*pop/1e9, 
         pop_mln=pop/1e6) %>%
  select(-continent, -pop, -babiesDead) %>%
  gather(key= variables, value= values, -c(country, year)) %>%
  ggplot() +
  geom_line(mapping=aes(x=year, y=values, color=country)) +
  facet_wrap(~ variables, scales = "free_y") +
  labs(title="h", 
       subtitle="y", 
       caption="ggg", 
       y=NULL,
       x="Year")+
  theme_bw() +
  theme(legend.position = "none")


#Test con el pipe, lo mismo que antes pero con modificaciones
gapminder_plus %>% 
  filter(continent=="Africa", year==2007) %>%
  mutate(babiesDead=infantMort*pop/10^3) %>%
  filter(babiesDead>2e6) %>%
  select(country) %>%
  left_join(gapminder_plus) %>%
  mutate(babiesDead=infantMort*pop/10^3, 
         gdp_bln=gdpPercap*pop/1e9, 
         pop_mln=pop/1e6) %>%
  select(-continent, -pop, -babiesDead) %>%
  gather(key= variables, value= values, -c(country, year)) %>%
  ggplot() +
  geom_text(data=. %>% filter(year==2007) %>% group_by(variables) %>%
              mutate(max_value=max(values)) %>%
              filter(values==max_value),
            aes(x=year, y=values, label=country, color=country))+
  geom_line(mapping = aes(x=year, y=values, color=country))+
  facet_wrap(~ variables, scales = "free_y") +
  labs(title="h", 
       subtitle="y", 
       caption="ggg", 
       y=NULL,
       x="Year")+
  theme_bw() +
  theme(legend.position = "none")


  
  
  
  
  
  
  
  
  