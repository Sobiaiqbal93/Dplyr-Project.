library(dplyr)
# First data file is about coutries, continent, region and their codes etc
# Sec data file is about different index like quality of life, purchasing power etc 
# Files to read
country_continent="countryContinent.csv"
Country_index="Quality_of_life.xlsx"

# Function to read file
file_reader=function(file_name){
  
  split_file=strsplit(file_name,split = '.', fixed = TRUE)[[1]][2]
  
  if(split_file=="csv"){ 
    read_test=read.csv(file_name)
  }else if(split_file=="xlsx"){
    read_test=readxl::read_excel(file_name)
  }else if(split_file=="xls"){
    read_test=read_excel(file_name)
  }else {
    print("invalid file extension")
    print('read only csv, xls, and xlsx files')
    read_test=NULL
  }
  
  return(read_test)
}

# Read continent file
sec_data=file_reader(country_continent)

# Read country indexes
first_data=file_reader(Country_index)

# Join two data frames using dplyr  
join_data = first_data %>% 
  full_join(sec_data, by="country") 

names(join_data) = c("RANK", "COUNTRY", "QUALITY_OF_LIFE_INDEX", "PURCHASING_POWER_INDEX",        
                      "SAFETY_INDEX", "HEALTH_CARE_INDEX", "COST_OF_LIVING_INDEX", "PROPERTY_PRICE_TO_INCOME_RATIO",
                      "TRAFFIC_COMMUTE_TIME_INDEX", "POLLUTION_INDEX", "CLIMATE_INDEX", "CODE_2",                        
                      "CODE_3", "COUNTRY_CODE" ,  "ISO_3166_2" , "CONTINENT",                     
                      "SUB_REGION", "REGION_CODE", "SUB_REGION_CODE")

#' In this section the joined data is used to get the best country in a continent. The criteria which is used
#' to get the best country based on evaluation of different indexes i.e., QUALITY_OF_LIFE_INDEX, SAFETY_INDEX,
#' POLLUTION_INDEX. 
#' 
#' First of all, the data is grouped by the continents and then the best country for respective continent is 
#' selected based on the respective index. For example, in Africa, South Africa has the highest QUALITY_OF_LIFE_INDEX
#' whereas in Asia, Oman holds the crown. After sorting out the countries according to the continent and the indexes, 
#' the most repeted country, in context of best indexes, is choose to be the best country in the continent. The column 
#' BEST_COUNTRY in aggregate_continent the best country is sorted.
aggregate_continent = join_data %>%
  mutate(CONTINENT = if_else(is.na(CONTINENT) | CONTINENT == "", "NOT_BELONG_TO_CONTINENT", CONTINENT)) %>%
  rename_all(funs(paste(., sep = "_"))) %>% 
  group_by(CONTINENT) %>%
  summarise(
    TOTAL_COUNTRY = n(),
    BIGGEST_POPULATION = COUNTRY[which(POLLUTION_INDEX == max(POLLUTION_INDEX, na.rm = TRUE))],
    HIGHEST_LIFE_IND = COUNTRY[which(QUALITY_OF_LIFE_INDEX == max(QUALITY_OF_LIFE_INDEX, na.rm = TRUE))],
    LOWEST_COST_LIVING = COUNTRY[which(COST_OF_LIVING_INDEX == min(COST_OF_LIVING_INDEX, na.rm = TRUE))],
    HIGHEST_PURCH_POW = COUNTRY[which(PURCHASING_POWER_INDEX == max(PURCHASING_POWER_INDEX, na.rm = TRUE))],
    HIGHEST_SAFETY_IND = COUNTRY[which(SAFETY_INDEX == max(SAFETY_INDEX, na.rm = TRUE))],
    BEST_HEALTH_CARE = COUNTRY[which(HEALTH_CARE_INDEX == max(HEALTH_CARE_INDEX, na.rm = TRUE))],
    LOWAST_COMMUTE_TIME = COUNTRY[which(TRAFFIC_COMMUTE_TIME_INDEX == min(TRAFFIC_COMMUTE_TIME_INDEX, na.rm = TRUE))],
    MOST_POLUTED = COUNTRY[which(POLLUTION_INDEX == max(POLLUTION_INDEX, na.rm = TRUE))],
    BEST_CLIMATE = COUNTRY[which(CLIMATE_INDEX == max(CLIMATE_INDEX, na.rm = TRUE))],
    ) %>%
  rowwise() %>%
  mutate(
    BEST_COUNTRY = names(which(table(unlist(., use.names = FALSE)) ==  max(table(unlist(., use.names = FALSE)))))
  )

#' In this section the join data is used to calculate the regional mean of each index.
aggregate_region = join_data %>%
  mutate(REGION_CODE = if_else(is.na(REGION_CODE), 9999999, as.double(REGION_CODE))) %>%
  group_by(REGION_CODE) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  rename_all(funs(paste("MEAN", toupper(.), sep = "_"))) 



