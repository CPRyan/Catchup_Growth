# Create Date
# 
# 


# create date
# This function is an easy way to smash together year month day into a single 'date' object. You can then work with the dates arithemetically. 

# e.g. date_intrv - date_birth = age_intrv

# Default add_millenium = 1900 (1983), can customize to add 2000. Note, if its already in that ormat the formula won't 'fix' it.  

# If lubridate is not installed, install it. 
# install.packages("lubridate")
# 
# If not already installed in tidyverse
# library(lubridate)


create_date <- function(df, year, month, day, add_millenium = 1900) {
  fixed_year <-if_else(df$year < 1900, 
                       df$year + add_millenium, 
                       df$year)
  created_year <-lubridate::ymd(paste(fixed_year , 
                       df$month, 
                       df$day, sep = "/"))
  created_year
}



# Example: 
# mbirth2$ic_birthday <-create_date(mbirth2, yearbirt, mnthbrth, daybirth)
# or
# mbirth2 <-mbirth2 %>% 
#  mutate(ic_birthday = create_date(mbirth2, yearbirt, mnthbrth, daybirth))



# Function to turn times (645, 1120) into times (6:45)
turn_to_time <- function(x) {
  mins  <-  substr(x, nchar(x)-1, nchar(x))
  hour  <-  substr(x, 0, nchar(x)-2)
  time  <-  paste0(hour, ':', mins)
  hms::parse_hm(time)
}



# Function to get z-scores for boys, girls, different years using left_join merging approach
# 
# 
who_z <- function(clhns_data, who_data, sex_variable, z_variable) {
  growth91_dataz <-left_join(clhns_data %>% 
    filter(sexchild == sex_variable) %>% 
    mutate(age_mo_91_rounded = round(age_mo_91)), 
  who_data, 
  by = c("age_mo_91_rounded" = "Month"))

growth91_dataz %>% 
  mutate(new_z = (z_variable - M)/StDev) %>% 
  select(new_z)
}






# Test it
who_z(clhns_data = growth91_data, 
      who_data = who2007_girls, 
      sex_variable = 2, 
      z_variable = hightndx)




growth91_dataz <-left_join(growth91_data %>%
                             filter(sexchild == 2) %>% 
                             mutate(age_mo_91 = round(age_mo_91)), 
                           who2007_girls, 
                           by = c("age_mo_91" = "Month"))


growth91_dataz %>% 
  mutate(heightz = (hightndx - M)/StDev)
