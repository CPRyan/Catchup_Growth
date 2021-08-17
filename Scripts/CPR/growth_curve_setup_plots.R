library(tidyverse) # great collection of packages for data carpentry, modelling, and visualization
library(haven) # package for loading Stata's .dta files. 
library(sjlabelled) # good for renaming, changing classes, etc. in the piped dplyr mode
library(lubridate)
here::here()
# sets a placeholder 'here'. Then we refer to our files based on the *relative* not the *absolute* location.
# 
# 
mbirth2 <-read_dta(here::here("Data/zip_mother_1983_86/mbirth2.dta")) %>% # note: here::here calls the here function
rename_all(tolower)

dim(mbirth2)



growth83_86_data <-mbirth2 %>% 
  select(basebrgy,hhldnubi, basewman, sexchild, outcome, daybirth, mnthbrth, yearbirt, dayintw, mothintw, yearintw, bbweight,weight1, weightak, weightgm, heightcm ) %>% 
  mutate(weightak_kg = (weightak/1000)) %>% 
  mutate(birthdate = make_date(yearbirt+1900, mnthbrth, daybirth)) %>% 
  mutate(intwdate = make_date(yearintw, mothintw, dayintw)) %>% 
  mutate(age_at_weigh_days = as.numeric(intwdate - birthdate)) %>%
  filter(outcome != "2" | outcome !="3" ) %>% 
  filter(age_at_weigh_days < 30)



mlong <- read_dta("Data/zip_mother_1983_86/mlong.dta")


mlong_anthro <-mlong %>% 
  select(basebrgy, basewman, weight, height, yearms, mntham, daymes) %>% 
  mutate(date_inf_meas = lubridate::make_date(year = yearms+1900,
                                              month = mntham,
                                              day = daymes)) %>% 
  arrange(desc(basewman, basebrgy, date_inf_meas))


mlong_anthro_bday <-left_join(mlong_anthro, growth83_86_data, by = c("basebrgy", "basewman")) %>% 
  mutate(age_inf_meas = date_inf_meas - birthdate) %>% 
  unite(basebrgy, basewman, sep = "_", col = "basebrgy_basewman") 
  

mlong_anthro_bday <-mlong_anthro_bday %>% 
  group_by(basebrgy_basewman) %>% 
  mutate(growth_accel = max(height, na.rm = T) - min(height, na.rm = T)) %>% 
  ungroup()


mlong_anthro_bday %>% 
  ggplot(., aes(x = growth_accel))+geom_histogram()

# Growth Curves
mlong_anthro_bday %>% 
  filter(!is.na(sexchild)) %>% 
  ggplot(., aes(x = age_inf_meas, y = height, group = basebrgy_basewman, col = as_character(sexchild)))+
  geom_point(alpha = 0.1)+
  geom_line(alpha = 0.2)+
  scale_color_manual(values = c("navyblue", "firebrick1"))+
  facet_wrap(.~sexchild)+
  theme_bw()


# Boxplots by age group
mlong_anthro_bday %>% 
  filter(!is.na(sexchild)) %>%
  mutate(growth_accel_bin = cut_interval(growth_accel, n = 8)) %>% 
  ggplot(., aes(x = age_inf_meas, y = height, group = basebrgy_basewman, col = growth_accel_bin))+
  geom_point(alpha = 0.2)+
  geom_line(alpha = 0.4)+
 # scale_color_manual(values = c("black", "brown", "blue", "green", "red", "white"))+
  facet_wrap(.~sexchild)+
  scale_color_brewer(palette = 3)+
  theme_bw()



