###Week 4 Homework##
install.packages("countrycode")


library(easypackages)
libraries("countrycode", "dplyr", "tidyverse", "sf","tmap")

composit <- read.csv("HDR23-24_Composite_indices_complete_time_series.csv",
                     header = TRUE,
                     sep = ",",
                     encoding = "latin1",
                     check.names = FALSE)

names(composit)

#selecting column using contains
data_2years <- composit %>%  dplyr::select(
  contains("iso3"),
  contains("country"),
  contains("gii_2010"),
  contains("gii_2019"))

#Difference in Gii world> no need to continue, I initially
#want to create column that can specify which country above/below world
n <- 206
w_difference <- data_2years$gii_2019[n] - data_2years$gii_2010[n]
w_difference

###Delete a column !
data_3years <- data_2years[-206,] #world
cleaned_csv <- data_3years[-c(196:205),] #unnecessary column

world_gii <- cleaned_csv %>%
  mutate(difference = (gii_2019-gii_2010)) %>%
  mutate(compare = case_when(difference >= 0 ~ "Index Increasing",
                             difference < 0 ~ "Index Decreasing",
                             TRUE ~ as.character(NA)))

# "NA" appears as a label in the legend> replace NA with a specific string:
world_gii <- world_gii %>%
  mutate(compare = ifelse(is.na(compare), "NA", compare))

#open geoJSON
world_json <- st_read("World_Countries_(Generalized)_9029012925078512962.geojson")


#creating iso3 from a country in geojson file, because csv iso3 already exist in csv
world_json2 <- world_json %>%
mutate(iso3_code = countrycode(COUNTRY, "country.name", "iso3c"))


#join the data (it is common to use iso3 instead iso2
world_gii_joined <- world_json2 %>% 
  merge(.,
        world_gii,
        by.x = 'iso3_code',
        by.y = 'iso3',
        no.dups = TRUE) %>%
  distinct()

###Delete unnecessary columns after joined 
world_gii_joined2 <- world_gii_joined[,-c(2:6)]

#Create a map
tmap_mode("plot")


tm_shape(world_gii_joined2) + 
tm_fill("compare", title = "Legend")+
tm_borders()+
tm_layout(main.title = "World Gender Inequality Index (2019-2010)",
          main.title.size = 1,  # Title size
          main.title.position = c("center", "top"),
          legend.text.size = 0.5,
          legend.title.size = 0.5,
          legend.position = c("left", "bottom"))

 library(usethis)
