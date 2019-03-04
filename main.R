library(tidyverse)
library(data.table)
library(harrypotter)
library(lubridate)
library(tictoc)
library(ggrepel)
library(gridExtra)
source('cp_province.R')
source('bootstrap_test.R')

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
general[, species2 := `ESPECIE 2`]
general[, species3 := `ESPECIE 3`]
general[, season := `ESTACIÓN`]
general[, n_species := `nº ESPECIES`]
# general[, species1 := ifelse(species1 == '', '0', species1)]
# general[, species1 := ifelse(species2 == '', '0', species2)]
# general[, species1 := ifelse(species3 == '', '0', species3)]
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



# Seasons Infection % -----------------------------------------------------
general %>% 
  filter(season != '') %>% 
  group_by(season, infected) %>% 
  summarise(n = n()) %>% 
  group_by(season) %>% 
  mutate(perc = 100 * n / sum(n)) %>% 
  filter(infected) %>% 
  ggplot(aes(x = season, y = perc)) +
  geom_col()

general %>% 
  filter(season != '') %>% 
  group_by(season, infected) %>% 
  summarise(n = n()) %>% 
  dcast(season~infected, value = n) %>% 
  set_names('season', 'no_infected', 'infected') %>% 
  select(no_infected, infected) %>% 
  chisq.test()


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

sp_map <- function(level = 2){
  spmapesp <- readOGR(paste0('data/gadm36_ESP_shp/gadm36_ESP_', level,'.shp'))
  mymap <- tidy(spmapesp)
  mymap$province <-  spmapesp@data[ as.character(mymap$id), paste0("NAME_", level, "")]
  mymap$id <- factor(mymap$id)
  return(mymap)
}

mymap <- sp_map(level = 2)

general %>% 
  group_by(province) %>% 
  summarise(n = n(),
            infected = sum(infected, na.rm = TRUE)) %>% 
  mutate(proportion_infected = infected / n) %>% 
  right_join(mymap) %>% 
  ggplot() +
  geom_polygon(aes( x= long, y = lat, group = group, fill = 100 * proportion_infected))  +
  scale_fill_hp(name = '% Infected', option = 'Mischief') +
  theme_void()


# Chi-Squared Testing -----------------------------------------------------

prov_table <- general %>% 
  group_by(province) %>% 
  summarise(n = n(),
            infected = sum(infected)) %>%
  mutate(no_infected = n - infected ) %>% 
  mutate(proportion_infected = infected / n) %>% 
  na.omit()

cas_list <- prov_table$province %>% unique()
cas_result <- tibble(province = cas_list, importance = 0)


for(i in 1:1e3){
  s_prov <- sample(cas_list, 25, replace = TRUE)
  
  p_value <- prov_table %>% 
    dplyr::filter(province %in% s_prov) %>% 
    dplyr::select(infected, no_infected) %>% 
    chisq.test() %>% 
    .$p.value
  
  guilt <- prov_table %>% 
    dplyr::filter(province %in% s_prov) %>% 
    mutate(average_prop = mean(proportion_infected)) %>% 
    filter(proportion_infected > average_prop) %>% 
    .$ca
  
  if(p_value < 0.05){
    pos <- which(cas_result$province %in% guilt)
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

# Bacteria Species Distribution ------------------------------------------------------

# GENERAL
bacteris_animal <- general %>% 
  select(species1, animal) %>% 
  set_names('species', 'animal') %>% 
  rbind(
    general %>% 
      select(species2, animal) %>% 
      set_names('species', 'animal')
  ) %>% 
  rbind(
    general %>% 
      select(species3, animal) %>% 
      set_names('species', 'animal')
  ) %>% 
  table() %>% 
  as_tibble() %>% 
  filter(species != 0) %>% 
  filter(species != '')

bacteris_animal %>% 
  filter(n > 0) %>% 
  group_by(animal) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = animal, y = n, fill = animal)) +
  geom_col(colour = 'black', size = 0.15) +
  geom_label_repel(aes(label = n), fill = 'white', size = 7, direction = 'y') +
  xlab('Animal') +
  ylab('N') +
  scale_fill_hp_d(option = 'Gryffindor2') +
  ggtitle('Number of Different Bacteria Species per Animal') +
  theme_bw() +
  theme(legend.position = 'none')

gg1 <- bacteris_animal %>% 
  filter(animal == 1) %>% 
  group_by(species) %>% 
  summarise(n = sum(n)) %>% 
  mutate(n =100 * n / sum(n)) %>% 
  ggplot(aes(x = reorder(species, n), y = n, fill = n)) +
  geom_col() +
  geom_label(aes(label = (n) %>% round(2) %>% paste0('%')), 
             fill = 'white',
             size = 2) +
  scale_fill_hp(option = 'HermioneGranger', guide = FALSE) +
  ggtitle('Proportion of Species in Animal 1') +
  coord_flip() +
  xlab('') +
  ylab('(%)') +
  theme_minimal()
  
gg2 <- bacteris_animal %>% 
  filter(animal == 2) %>% 
  group_by(species) %>% 
  summarise(n = sum(n)) %>% 
  mutate(n =100 * n / sum(n)) %>% 
  ggplot(aes(x = reorder(species, n), y = n, fill = n)) +
  geom_col() +
  geom_label(aes(label = (n) %>% round(2) %>% paste0('%')), 
             fill = 'white',
             size = 2.5) +
  scale_fill_hp(guide = FALSE, begin = 0.2, end = 0.6, direction = -1) +
  ggtitle('Proportion of Species in Animal 2') +
  coord_flip() +
  xlab('') +
  ylab('(%)') +
  theme_minimal()

grid.arrange(gg1,gg2, ncol = 2)
gg1
gg2


# PROVINCES
bacteria_province <- general %>% 
  select(species1, province) %>% 
    set_names('species', 'province') %>% 
    rbind(
      general %>% 
        select(species2, province) %>% 
        set_names('species', 'province')
    ) %>% 
    rbind(
      general %>% 
        select(species3, province) %>% 
        set_names('species', 'province')
    ) %>% 
    table() %>% 
    as_tibble() %>% 
    filter(species != 0) %>% 
    filter(species != '') %>% 
  group_by(province) %>% 
  mutate(n = 100 * n / sum(n))

top_species_dog <- bacteris_animal %>% 
  filter(animal == 1) %>% 
  group_by(species) %>% 
  summarise(c = sum(n)) %>% 
  arrange(-c) %>% 
  top_n(5, c) %>% 
  .$species

top_species_cat <- bacteris_animal %>% 
  filter(animal == 2) %>% 
  group_by(species) %>% 
  summarise(c = sum(n)) %>% 
  arrange(-c) %>% 
  top_n(5, c) %>% 
  .$species
  

bacteria_province %>% 
  filter(species %in% top_species_dog) %>% 
  group_by(province) %>% 
  arrange(-n, .by_group = TRUE) %>% 
  group_by(province) %>% 
  na.omit() %>% 
  right_join(mymap) %>% 
  ggplot() +
  geom_polygon(aes( x= long, y = lat, group = group, alpha = n), fill = hp(10)[[4]])  +
  theme_void() +
  facet_wrap(.~species)
  
  
# CA
table1 <- general %>% 
  select(species1, ca) %>% 
  table() %>% 
  as_tibble() %>% 
  set_names(c('species', 'province', 'n1'))

table2 <- general %>% 
  select(species2, ca) %>% 
  table() %>% 
  as_tibble() %>% 
  set_names(c('species', 'province', 'n2'))

table3 <- general %>% 
  select(species3, ca) %>% 
  table() %>% 
  as_tibble() %>% 
  set_names(c('species', 'province', 'n3'))

bacteria_distribution <- table1 %>% 
  left_join(table2) %>% 
  left_join(table3) %>% 
  mutate(n = n1 + n2 + n3)

bacteria_distribution %>% 
  filter(n > 0) %>% 
  filter( species != 0) %>% 
  mutate(n = ifelse(is.na(n), 0, n)) %>% 
  select( species, province, n) %>% 
  group_by(province) %>% 
  arrange(-n, .by_group = TRUE) %>% 
  group_by(province) %>% 
  slice(1) %>% 
  right_join(sp_map(level = 1)) %>% 
  ggplot() +
  geom_polygon(aes( x= long, y = lat, group = group, fill = species)) +
  scale_fill_hp_d(option = 'hermionegranger', name = 'Bacteria Species') +
  theme_void()


# Bacteria Species on Seasons ---------------------------------------------
table1 <- general %>% 
  filter(infected) %>% 
  select(species1, season) %>% 
  table() %>% 
  as_tibble() %>% 
  set_names(c('species', 'season', 'n1'))

table2 <- general %>% 
  filter(infected) %>% 
  select(species2, season) %>% 
  table() %>% 
  as_tibble() %>% 
  set_names(c('species', 'season', 'n2'))

table3 <- general %>% 
  filter(infected) %>% 
  select(species3, season) %>% 
  table() %>% 
  as_tibble() %>% 
  set_names(c('species', 'season', 'n3'))

bacteria_season <- table1 %>% 
  outer_join(table2) %>% 
  outer_join(table3) %>% 
  filter(season != '') %>% 
  mutate(n = n1 + n2 + n3)

bacteria_season %>% 
  filter(species != 0) %>% 
  ggplot(aes(x = season, y = n, fill = species)) +
  geom_col(position = 'fill')


general %>% 
  select(species1, season) %>% 
  set_names('species', 'season') %>% 
  rbind(
    general %>% 
      select(species2, season) %>% 
      set_names('species', 'season')
  ) %>% 
  rbind(
    general %>% 
      select(species3, season) %>% 
      set_names('species', 'season')
  ) %>% 
  table() %>% 
  as_tibble() %>% 
  filter(season != '') %>% 
  filter(species != 0) %>%
  filter(species != '') %>% 
  ggplot(aes(x = season, y = n, fill = species)) +
  geom_col()


# Age ---------------------------------------------------------------------
 
general %>% 
  group_by(animal, infected, EDAD) %>% 
  summarise(n = n()) %>%
  group_by(animal) %>% 
  mutate(n = n/sum(n)) %>% 
  na.omit() %>% 
  ggplot(aes(x = EDAD, y = n, fill = infected)) +
  geom_col() +
  facet_wrap(.~animal)
  

general %>% 
  filter(!is.na(animal)) %>% 
  ggplot(aes(x = infected, y = EDAD, fill = infected)) +
  geom_boxplot(alpha = 1, size = 0.25) +
  facet_wrap(.~animal) +
  scale_fill_hp_d(option = 'sprout', name = 'Infected?', labels = c('No', 'Yes')) +
  theme_minimal() +
  xlab('Infected?') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab('Age')

# ANIMAL 1
x <- general %>% filter(animal == 1) %>%  filter(infected) %>% .$EDAD
y <- general %>% filter(animal == 1) %>% filter(!infected) %>% .$EDAD
bootstrap_test(x,y)
t.test(y,x)
wilcox.test(y,x)
bootstrap_test3(y,x)

# ANIMAL 2
x <- general %>% filter(animal == 2) %>%  filter(infected) %>% .$EDAD
y <- general %>% filter(animal == 2) %>% filter(!infected) %>% .$EDAD
bootstrap_test(x,y)
t.test(x,y)
wilcox.test(y,x)
bootstrap_test3(y,x)


# N Species ---------------------------------------------------------------

general %>% 
  filter(animal == 1) %>% 
  group_by(n_species) %>% 
  summarise(n = n()) %>% 
  mutate(n = 100 * n/sum(n)) %>% 
  na.omit() %>% 
  ggplot(aes(x = n_species, y = n)) +
  geom_col(fill = hp(10)[[4]], colour = 'black', size= 0.15) +
  geom_label(aes(label = n %>% round(2) %>% paste0(' %'))) +
  xlab('N Species') +
  ylab('(%)') +
  theme_minimal() +
  ggtitle('Dog')

general %>% 
  filter(animal == 2) %>% 
  group_by(n_species) %>% 
  summarise(n = n()) %>% 
  mutate(n = 100 * n/sum(n)) %>% 
  na.omit() %>% 
  ggplot(aes(x = n_species, y = n)) +
  geom_col(fill = hp(10)[[7]], colour = 'black', size= 0.15) +
  geom_label(aes(label = n %>% round(2) %>% paste0(' %'))) +
  xlab('N Species') +
  ylab('(%)') +
  theme_minimal() +
  ggtitle('Cat')
