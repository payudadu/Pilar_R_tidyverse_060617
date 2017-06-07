x <- 5 * 6
x
is.vector(x)
length(x)

x[2] <- 31 #introduce un segundo valor para x, entre [] son las posiciones
x

x[5] <- 44 #faltan valores para posicines 3 y 4, se meten NA automaticamente, no cero porque es un valor
x

x[11]
x[0] #en R el primer valor está en posición 1, en phyton es 0. Cuidado con esto

x <- 1:4
x

y <- x^2
y

#recycling of vectors
x <- 1:5
y <- 3:7

x
y

x+y #Suma los valores según su posición

z <- y[-5] #elimina el valor en posición 5, no el número 5
z

x+z #Las suma pero da un warning porque hay una posición que no tiene valor para sumar
#En este caso recicla el vector y suma un valor repetido cuando se acaba una de las listas
#Cuando sumas dos listas de diferente número de componentes la corta se recicla, y si la grande es múltiplo de la otra no da warning
#Hay que tener en cuenta que esto está pasando
#recycling es inportante, pero hay que tener en cuenta que está pasando

z^x
y^x

#missin values are contagieous
x
y <- x[-5]
y
x+y


#Unir vectores en el mismo objeto, combination sign: c()
c("Hello", "Goodbye", "Heyhey")

str(c("Hello", "Goodbye", "Heyhey"))

c(9:11, 200, x)
str(c(9:11, 200, x))

c("something", pi, 2:4, pi > 3)
str(c("something", pi, 2:4, pi > 3))


c(pi, 2:4, pi > 3)
str(c(pi, 2:4, pi > 3))


w <- rnorm(10) #coge números al azar en una distribución normal, cada vez te da un resultado
seq_along(w) #te dice las posiciones, irá del 1 al 10
w
which(w < 0)
w[which(w < 0)]

w

w[-c(2, 5)]

list("Something", pi, 2:4, pi>3) #te da listas de cada uno de los tipos de componentes del conjunto: numérico, lógico, caracter
str(list("Something", pi, 2:4, pi>3))

list(vegetable = "cabbage",
     number = pi, 
     series = 2:4,
     telling = pi > 3)

x <- list(vegetable = "cabbage",
          number = pi, 
          series = 2:4,
          telling = pi > 3)
str(x)

x$vegetable #Al poner $ te da la lista de diferentes componentes que puedes elegir

x[1]

str(x$vegetable)
str(x[1])

x[[3]]
#Esto son diferentes métodos para coger elementos o listas. Es un poco lioso
#Se pueden usar single o double squared brackets: [] [[]] y dan diferentes resultados
#El doble o el $ te da el elemento, el simple te da una lista con el elemento dentro. 


x <- list(vegetable = list("cabbage", "carrot", "spinach"),
          number = list(c(pi, 0, 2.14, NA)),
          series = list(list(2:4, 3:5)),
          telling = pi > 3)
str(x)

gapminder_plus <- read_csv(file = "Data/gapminder_plus.csv")

mod <- lm(lifeExp ~ gdpPercap, data=gapminder_plus)
str(mod)

mod[["df.residual"]]
mod$df.residual
mod$df.residual

str(mod$df.residual)

mod$qr$qr[[1]]

#summarize average life expentancy by the continent


  gapminder_plus %>%
  group_by(continent) %>%
  summarise(mean_lifeExp=mean(lifeExp),
            min_le = min(lifeExp),
            max_le = max(lifeExp))



#plot life expectancy by year in all the countries
gapminder_plus %>%
  ggplot() +
  geom_line(mapping = aes(x=year, y=lifeExp, color = continent, group=country)) +
  geom_smooth(mapping=aes(x=year, y=lifeExp), method = "lm", color="black")+
  facet_wrap(~continent) 
  
by_country <- gapminder_plus %>% group_by(continent, country) %>%
  nest()

str(by_country$data)

by_country$data[[1]]

#Estamos haciendo pequeñas tablas o datasets dentro de otras tablas
#esto ayuda a que se puedan hacer modelos lineales, por ejemplo, de cada dataset

#map(list, funtion)
map(1:3, sqrt)

#el package maps tiene una función maps que entra en conflicto con la que esramos usando ahora
#hay que recargar el package que estamos usando o especificar de cuál lo queremos
#asi: mutate(model=purrr::map(data...etc))
library(purrr)

by_country

by_country %>%
  mutate(model=map(data, ~lm(lifeExp~year, data=.x)))


model_by_country <- by_country %>%
  mutate(model=purrr::map(data, ~lm(lifeExp~year, data=.x))) %>%
  mutate(summr=map(model, broom::glance)) %>%
  unnest(summr) %>% arrange(r.squared)
model_by_country



##
by_country %>%
  mutate(model=purrr::map(data, ~lm(lifeExp~year, data=.x))) %>%
  mutate(summr=map(model, broom::glance)) %>%
  unnest(summr) %>% arrange(r.squared) %>%
  ggplot()+
  geom_jitter(mapping = aes(x=continent, y=r.squared))


##
by_country %>%
  mutate(model=purrr::map(data, ~lm(lifeExp~year, data=.x))) %>%
  mutate(summr=map(model, broom::glance)) %>%
  unnest(summr) %>% arrange(r.squared) %>% filter(r.squared<0.3) %>%
  select(country) %>% left_join(gapminder_plus) %>%
  ggplot() + 
  geom_line(mapping = aes(x=year, y=lifeExp, color=country, group=country))
  
  
#life expentancy dependent on gdp per capita
#

gapminder_plus %>% ggplot() + 
  geom_line(mapping = aes(x=log(gdpPercap), y=lifeExp, color=continent, 
                           group=country))


by_country %>%
  mutate(model=purrr::map(data, ~lm(lifeExp~log(gdpPercap), data=.x))) %>%
  mutate(summr=map(model, broom::glance)) %>%
  unnest(summr) %>% arrange(r.squared) %>% filter(r.squared<0.1) %>%
  select(country) %>% left_join(gapminder_plus) %>%
  ggplot() + 
  geom_point(mapping = aes(x=log(gdpPercap), y=lifeExp, 
                           color=country))

#Para guardar los datos
# saveRDS(aquí lo que quieres guardar, "aquí el nombre del archivo.rds")
#.rds solo puede leerse en R

saveRDS(by_country, "by_country_tibble.rds")

my_fresh_by_country <- readRDS ("by_country_tibble.rds")

my_fresh_by_country

write_csv(gapminder_plus, "gapminder_plus_for_proffesor.csv")

























