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
