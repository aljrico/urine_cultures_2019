cp_to_province <- function(x){
  
  dicc <- c('01 Araba/Álava',
            '02 Albacete',
            '03 Alicante',
            '04 Almería',
            '05 Ávila',
            '06 Badajoz',
            '07 Balearic Islands',
            '08 Barcelona',
            '09 Burgos',
            '10 Cáceres',
            '11 Cádiz',
            '12 Castellón',
            '13 Ciudad Real',
            '14 Córdoba',
            '15 A Coruña',
            '16 Cuenca',
            '17 Girona',
            '18 Granada',
            '19 Guadalajara',
            '20 Gipuzkoa',
            '21 Huelva',
            '22 Huesca',
            '23 Jaén',
            '24 León',
            '25 Lleida',
            '26 La Rioja',
            '27 Lugo',
            '28 Madrid',
            '29 Málaga',
            '30 Murcia',
            '31 Navarre',
            '32 Ourense',
            '33 Asturias',
            '34 Palencia',
            '35 Las Palmas',
            '36 Pontevedra',
            '37 Salamanca',
            '38 Santa Cruz de Tenerife',
            '39 Cantabria',
            '40 Segovia',
            '41 Seville',
            '42 Soria',
            '43 Tarragona',
            '44 Teruel',
            '45 Toledo',
            '46 Valencia',
            '47 Valladolid',
            '48 Bizkaia',
            '49 Zamora',
            '50 Zaragoza',
            '51 Ceuta',
            '52 Melilla'
  )
  
  cp <- dicc %>% str_extract_all("\\(?[0-9,.]+\\)?") %>% unlist() %>% as.numeric()
  province <-  dicc %>% str_remove_all("\\(?[0-9,.]+\\)?") %>% unlist() %>% trimws()
  
  result <- c()
  for(i in seq_along(x)){
    y <- x[[i]]
    
    if(!is.na(as.numeric(y))){
      y <- as.numeric(y)
      position <- which(cp == y)
      if(length(position) > 0) result[[i]] <- province[position]
    }else{
      y <- as.character(y)
      position <- which(province == y)
      if(length(position) > 0) result[[i]] <- cp[position]
    }
  }
  return(result)
}
