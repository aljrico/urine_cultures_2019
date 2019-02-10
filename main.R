library(tidyverse)
library(data.table)
library(harrypotter)
library(lubridate)
library(tictoc)
library(ggrepel)
library(gridExtra)
source('cp_province.R')

# SPAIN MAP functions
library(broom)
library(sp)
library(rgdal)
library(maptools)
library(splancs)
require(gpclib)
gpclibPermit()

simpleCap <- function(x) {
  output <- c()
  for(i in seq_along(x)){
    s <- x[[i]] %>% tolower()
    s <- strsplit(s, " ")[[1]]
    output[[i]] <- paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ") 
  }
  return(output)
}


# Preprocessing -----------------------------------------------------------

not_all_na <- function(x)!all(is.na(x))

general <- fread('data/general.csv') %>% 
  select_if(not_all_na) %>%
  data.table()

general[, FECHA := FECHA %>% myd()]
general[, EDAD :=  EDAD %>% as.numeric()]
general[, province := CP %>% as.character() %>%  substr(start = 1, stop = 2) %>% cp_to_province()]
general[, animal :=  `PERRO/GATO` %>% as.factor() ]
general[, species1 := `ESPECIE 1`]
general[, infected := (species1 != '0')]
general[, ca := `C.A` %>% simpleCap() %>% 
          str_replace('C. León', 'Castilla y León') %>% 
          str_replace('C. La Mancha', 'Castilla-La Mancha') %>% 
          str_replace('Murcia', 'Región de Murcia') %>% 
          str_replace('Navarra', 'Comunidad Foral de Navarra') %>% 
          str_replace('Canarias', 'Islas Canarias') %>% 
          str_replace('Asturias', 'Principado de Asturias') %>% 
          str_replace('Baleares', 'Islas Baleares') %>% 
          str_replace('Madrid', 'Comunidad de Madrid') %>% 
          str_replace('Ceuta/meilla', 'Ceuta y Meilla') %>% 
          str_replace('Valencia', 'Comunidad Valenciana') %>% 
          str_replace('Andalucia', 'Andalucía') %>% 
          str_replace('Aragon', 'Aragón')]
general[, ca := ifelse(province == 'Madrid', 'Comunidad de Madrid', ca)]
general[, ca := ifelse(province == 'Barcelona', 'Cataluña', ca)]
general[, ca := ifelse(province == 'A Coruña', 'Galicia', ca)]
general[, ca := ifelse(province == 'Cantabria', 'Cantabria', ca)]
general[, ca := ifelse(province == 'Málaga', 'Andalucía', ca)]


# % per animal ----------------------------------------------------------------

gg1 <- general %>% 
  filter(animal == 1) %>% 
  group_by(infected) %>% 
  summarise(n = n()) %>% 
  mutate(n = n / sum(n)) %>% 
  arrange(n) %>% 
  mutate(pos = (cumsum(n) - n/2)) %>% 
  ggplot(aes(x = '', y = n, fill = infected)) +
  geom_col(position = 'fill') +
  coord_polar(theta ='y', start = 0) +
  geom_label(aes(label = (100*n) %>% round(2) %>% paste0('%'), x = 1, y = pos), 
             fill = 'white',
             size = 5.5,
             alpha = 0.7) +
  theme_void() +
  scale_fill_hp_d(option = 'Gryffindor2', name = 'Infected?', labels = c('No', 'Yes'), direction = -1) +
  ggtitle('Proportion of infected in Animal 1')

gg2 <- general %>% 
  filter(animal == 2) %>% 
  group_by(infected) %>% 
  summarise(n = n()) %>% 
  mutate(n = n / sum(n)) %>% 
  arrange(n) %>% 
  mutate(pos = (cumsum(n) - n/2)) %>% 
  ggplot(aes(x = '', y = n, fill = infected)) +
  geom_col(position = 'fill') +
  coord_polar(theta ='y', start = 0) +
  geom_label(aes(label = (100*n) %>% round(2) %>% paste0('%'), x = 1, y = pos), 
             fill = 'white',
             size = 5,
             alpha = 0.7) +
  theme_void() +
  scale_fill_hp_d(option = 'Gryffindor2', name = 'Infected?', labels = c('No', 'Yes'), direction = -1) +
  ggtitle('Proportion of infected in Animal 2')


grid.arrange(gg1,gg2, ncol = 2)
  

# Spain Map --------------------------------------------------------------

spmapesp <- readOGR('data/gadm36_ESP_shp/gadm36_ESP_1.shp')
mymap <- tidy(spmapesp)
mymap$ca <-  spmapesp@data[ as.character(mymap$id), "NAME_1"]
mymap$id <- factor(mymap$id)

general %>% 
  group_by(ca) %>% 
  summarise(n = n(),
            infected = sum(infected, na.rm = TRUE)) %>% 
  mutate(proportion_infected = infected / n) %>% 
  right_join(mymap) %>% 
  ggplot() +
  geom_polygon(aes( x= long, y = lat, group = group, fill = 100 * proportion_infected))  +
  scale_fill_hp(name = '% Infected') +
  theme_void()

prov_table <- general %>% 
  group_by(ca) %>% 
  summarise(n = n(),
            infected = sum(infected)) %>% 
  mutate(proportion_infected = infected / n) %>% 
  mutate(no_infected = n - infected ) %>% 
  na.omit()

cas_list <- prov_table$ca %>% unique()
cas_result <- tibble(ca = cas_list, importance = 0)


for(i in 1:1e3){
  s_prov <- sample(cas_list, 25, replace = TRUE)
  
  p_value <- prov_table %>% 
    dplyr::filter(ca %in% s_prov) %>% 
    dplyr::select(infected, no_infected) %>% 
    chisq.test() %>% 
    .$p.value
  
  guilt <- prov_table %>% 
    dplyr::filter(ca %in% s_prov) %>% 
    mutate(average_prop = mean(proportion_infected)) %>% 
    filter(proportion_infected > average_prop) %>% 
    .$ca
  
  if(p_value < 0.05){
    pos <- which(cas_result$ca %in% guilt)
    cas_result[pos,'importance'] <- cas_result[pos,'importance'] + 1
  }
  
  cat(paste0('... ', i, ' ... \n'))
}

cas_result %>% 
  arrange(desc(importance)) %>% 
  left_join(prov_table)

prov_table %>% 
  dplyr::select(infected, no_infected) %>% 
  chisq.test()
