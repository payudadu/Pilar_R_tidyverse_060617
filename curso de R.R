# To load the different packages write library("name of the package") and run

library("tidyverse")

#gapminder es el nombre del objeto. Así el programa lee el file en data y crea un objeto llamado así
gapminder <- read_csv(file = "Data/gapminder-FiveYearData.csv")

#Ahora al escribir el nombre del objeto y ejecutarlo vemos todo lo que contiene la carpeta data
gapminder

ggplot(data = gapminder) +
  geom_point(mapping = aes(x = gdpPercap, y = lifeExp))

ggplot(data = gapminder) +
  geom_jitter(mapping = aes(x = gdpPercap, y = lifeExp, color = continent))

#
ggplot(data = gapminder) +
  geom_point(mapping = aes(x = log(gdpPercap), y = lifeExp, color = continent, size = pop))

#alpha es para indicar opacidad de los puntos, size es para tamaño de los puntos y color da el mismo color a todos los puntos
ggplot(data = gapminder) +
  geom_point(mapping = aes(x = log(gdpPercap), y = lifeExp), 
             alpha = 0.1, size = 2, color = "blue")
                           
ggplot(data = gapminder) +
  geom_line(mapping = aes(x = year, y = lifeExp, 
             group = country, color = continent))

ggplot(data = gapminder) +
  geom_boxplot(mapping = aes(x = continent, y = lifeExp)) 


ggplot(data = gapminder) +
  geom_jitter(mapping = aes(x = continent, y = lifeExp, color = continent)) +
  geom_boxplot(mapping = aes(x = continent, y = lifeExp, color = continent)) 


ggplot(data = gapminder) +
   geom_boxplot(mapping = aes(x = continent, y = lifeExp, color = continent)) +
    geom_jitter(mapping = aes(x = continent, y = lifeExp, color = continent))

ggplot(data = gapminder,
  mapping = aes(x = continent, y = lifeExp, color = continent)) +
  geom_boxplot() +
  geom_jitter()

#ggplot es la función para decir de donde se cogen los datos. Hemos usado diferentes
#geoms que es geometrical representation. mapping es para indicar qué va en cada
#parte del gráfoco (le dices qué va en x, y, z, colores para variables...)
#Hay que usar los nombres de cada columna de la tabla de datos con la que se empieza
#Se pueden dar más dimensiones añadiendo color, tamaño de burbuja...
#Las geoms pueden agruparse y aparecen en capas superpuestas

#lm is linear mode; alpha da transparencia. Lo puedes aplicar a la capa que quieras
ggplot(data = gapminder,
       mapping = aes(x = log(gdpPercap), y = lifeExp, color = continent)) +
  geom_jitter(alpha= 0.1) +
  geom_smooth(method= "lm")

#Para esconder el color de una de las capas se quita de la primera linea 
#y se incluye solo en la capa que quieres que tenga colores
#Así conseguimos que el modelo lineal no se divida en colores y de solo una linea
ggplot(data = gapminder,
       mapping = aes(x = log(gdpPercap), y = lifeExp)) +
  geom_jitter(mapping=aes(color = continent), alpha= 0.1) +
  geom_smooth(method= "lm")

#Si usas year solo R no entiende que tiene que separar los datos por año
#si se pone una funcion por año, como as.character, separa los datos de cada año
ggplot(data = gapminder) +
  geom_boxplot(mapping = aes(x = as.character(year), y = lifeExp))

ggplot(data = gapminder) +
  geom_boxplot(mapping = aes(x = as.character(year), y = log(gdpPercap)))

ggplot(data = gapminder) +
  geom_density2d(mapping = aes(x = lifeExp, y = log(gdpPercap)))

#Este gráfico separa los gráficos en función de lo que hay dentro de facet_wrap
#Para escribir ~ en mi ordenador es AltGr+4 y pulsar espacio 
ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  geom_smooth() +
  scale_x_log10() +
  facet_wrap(~ continent)

#Challenge 7.
#Try faceting by year, keeping the linear smoother. 
  #Para tener un modo lineal hay que poner method = "lm" dentro de geom_smooth
#Is there any change in slope of the linear trend over the years? 

ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10() +
  facet_wrap(~ year)

#What if you look at linear models per continent?
ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10() +
  facet_wrap(~ continent)


#Nueva función, filter: aquí sirve para seleccionar datos de solo un año, 
#por ejemplo. Ahora se puede usar como una fuente de datos más pequeña
filter(gapminder, year==2007)

#Si no indicas una variable y te hace un gráfico con counts, que es el número
#de datos que hay para cada continente (creo)
ggplot(data=filter(gapminder, year==2007)) + 
  geom_bar(mapping = aes(x=continent))

#Se puede escribir también así
ggplot(data=filter(gapminder, year==2007)) + 
  geom_bar(mapping = aes(x=continent), stat = "count")


#Nueva data source , oceania: Sólo coge datos de un continente

filter(gapminder, year==2007, continent=="Oceania")

ggplot(data=filter(gapminder, year==2007, continent=="Oceania")) + 
  geom_bar(mapping = aes(x=country, y=pop), stat = "identity")
#stat = "identity" NO ENTIENDO IDENTITY, creo que es que coja el valor de cada
#variable, y no el número de datos, que sería counts

ggplot(data=filter(gapminder, year==2007, continent=="Oceania")) + 
  geom_bar(mapping = aes(x=country), stat = "count")

#count indica que hay un dato para cada variable
ggplot(data=filter(gapminder, year==2007, continent=="Asia")) + 
  geom_col(mapping = aes(x=country, y=pop))


#coord_flip() sirve para hacer el gráfico de barras tumbado
ggplot(data=filter(gapminder, year==2007, continent=="Asia")) + 
  geom_col(mapping = aes(x=country, y=pop)) +
  coord_flip()

#Para meterle colores etc. Labs means lables

ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp, 
                                       color=continent, size=pop/1e+06)) +
  geom_point() +
  scale_x_log10() +
  facet_wrap(~ year)+
  labs(title="El título que le quieras dar a la cosa",
       subtitle="Subtitula como te parezca y te enrollas si te da la gana",
       caption="Esto va en la parte de abajo del gráfico",
       x="Titulo del eje x que quieras", 
       y="Titulo del eje y que te parezca", 
       color="lo que representa el color", 
       size="lo que representa el tamaño de las bolas")

#┬Para guardar automáticamente el gráfico. Va a la carpeta default
#Lo bueno de hacer esto en vez de guardarlo es que re-escribe el gráfico cada vez
#Así no te quedas con un gráfico no actualizado
ggsave("my_fancy_plot.jpg")


