library(tidyverse)
library(readxl)

files <- list.files("datat") 
filesPath <- file.path("datat", files) 
vuosi <- parse_numeric(filesPath)
meta <- data.frame(file=files, filesPath= filesPath, vuosi =vuosi)


cities <-  filesPath %>% 
  lapply(., excel_sheets ) %>% 
  # lisätään vuosi taulukoihin
  mapply(function(vektori, vuosi) data.frame(kaupunki=vektori, vuosi= vuosi),
         .,
         vuodet,SIMPLIFY = F) %>% 
  bind_rows() %>% 
  left_join(meta)

brands <- c("Citymarket","K-Supermarket","Lidl","Prisma", "Smarket", "Valintalo") 
brand_rules <- paste(brands, collapse="|")

data <- mapply(function(fil, cit) read_excel(path=fil, sheet=cit, col_names = F),
               cities$kaupunki,
               cities$filesPath,
               SIMPLIFY = F)
                 
  
  
  
  lapply(cities, function(name) read_excel("datat/2016.xlsx", sheet=name, col_names = F) )

kaupat <- lapply(data, function(df) df %>% 
                   select(1) %>% 
                   setNames("kauppa") %>% 
                   mutate(vastine = grepl( tolower(brand_rules), tolower(kauppa ) ) ) %>% 
                   filter(vastine==T) %>% 
                   select(-vastine)) %>% 
  mapply(function(df, city) df %>% 
           mutate(kaupunki=city), 
         .,
         cities, SIMPLIFY = F) %>% 
  bind_rows() 
  
