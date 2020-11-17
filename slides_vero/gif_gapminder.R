# Paquetes necesarios
# install.packages("tidyverse")
# install.packages("gganimate")
# install.packages("gifski")
# install.packages("png")
# intall.packages("gapminder")

# Cargamos los paquetes
library(tidyverse); library(gapminder); library(gganimate)

# Creamos el gráfico con ggplot
p <- ggplot(gapminder, aes(gdpPercap, lifeExp, 
                           color = continent)) +
  geom_point(aes(size = pop), show.legend = FALSE) +
  scale_y_continuous(breaks = seq(20,90,10)) +
  scale_size(range = c(2,12)) + 
  scale_x_log10()

# Con este código creamos el gif
p + labs(title = 'Año: {frame_time}',
         x = 'PIB per capita',
         y = 'Esperanza de vida') +
  transition_time(year) + 
  ease_aes('linear')