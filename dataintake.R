pirate <- read_csv("data_final.csv")

pirate <- pirate %>% 
  head(163)

pirate <- pirate %>%
  mutate(
    boat_status = tolower(boat_status),
    attack_status = tolower(attack_status),
    type = tolower(type),
    success = tolower(success),
    date = dmy(date),
    time = as.numeric(time),
    latitude = ifelse(str_detect(latitude,"S"),str_c("-",latitude),latitude),# For all South latitudes, add a minus sign at the beginning
    latitude = str_replace(latitude, "S", ""),
    latitude = str_replace(latitude, "N", ""),
    longitude = ifelse(str_detect(longitude,"W"),str_c("-",longitude),longitude), # For all West longitudes, add a minus sign at the beginning
    longitude = str_replace(longitude, "W", ""),
    longitude = str_replace(longitude, "E", "")) %>% 
  separate(longitude, c("lo_deg","lo_min"), sep = ":") %>% #Separate longitude and latitude degrees and minutes
  separate(latitude, c("la_deg", "la_min"), sep = ":") %>%
  mutate(la_deg = as.numeric(la_deg),
         lo_deg = as.numeric(lo_deg),
         la_min = ifelse(la_deg < 0, str_c("-", la_min), la_min ),
         lo_min = ifelse(lo_deg < 0, str_c("-", lo_min), lo_min ),
         la_min = round(as.numeric(la_min)),
         lo_min = round(as.numeric(lo_min)),
         longitude = lo_deg + (lo_min / 60),
         latitude = la_deg + (la_min / 60)) %>% 
  select(-lo_deg, -lo_min, -la_deg, -la_min)