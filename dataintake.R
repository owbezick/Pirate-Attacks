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

#Create islands list
islands <- c("Antigua and Barbuda", "Bahamas","Bahrain", "Barbados","Brunei","Cape Verde","Comoros","Cook Islands","Cuba","Cyprus","Dominica","Dominican Republic","East Timor","Federated States of Micronesia","Fiji","Grenada, Carriacou and Petite Martinique","Haiti","Iceland","Indonesia","Republic of Ireland","Jamaica","Japan","Kiribati","Madagascar", "Maldives","Malta","Marshall Islands","Mauritius","Nauru","New Zealand","Niue", "Northern Cyprus","Palau","Papua New Guinea","Philippines","Saint Kitts and Nevis","Saint Lucia","Saint Vincent and the Grenadines","Samoa","São Tomé and Príncipe", "Seychelles","Singapore","Solomon Islands","Sri Lanka","Taiwan","Tonga","Trinidad and Tobago","Tuvalu","United Kingdom","Vanuatu", "Cayman Island", "Isle of Man", "Hong Kong")

#add a column in pirates dataset on whether or not the nation is an island
pirate <- pirate %>%
  mutate(typeC = case_when(
    flag %in% islands ~ "Island Nation", TRUE ~ "Mainland Nation"))

write_rds(pirate, "df_pirate.RDS")
