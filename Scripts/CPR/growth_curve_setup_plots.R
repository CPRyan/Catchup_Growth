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


# Growth Curves for height by age
mlong_anthro_bday %>% 
  filter(!is.na(sexchild) & age_inf_meas > -10) %>%
  mutate(growth_accel_bin = cut_interval(growth_accel, n = 4)) %>% 
  ggplot(., aes(x = age_inf_meas, y = height))+
 # geom_point(alpha = 0.2)+
  geom_line(alpha = 0.2, aes(group = basebrgy_basewman, col = growth_accel_bin))+
 # geom_smooth(aes(group = sexchild, col = as_factor(sexchild)))+
 # scale_color_manual(values = c("black", "brown", "blue", "green", "red", "white"))+
  facet_wrap(.~sexchild)+
  scale_color_brewer(type = "div", palette = 9)+
  theme_bw()



mlong_anthro_bday %>% 
  select(basebrgy_basewman, weight, height, date_inf_meas, age_inf_meas, birthdate) %>% 
  group_by(basebrgy_basewman) %>%
  mutate(id = row_number()) %>% 
  spread(key = id, value = weight, drop = TRUE) 
# Not ideal. 
# 
# 
# PIck the first and last ones. 

mlong_for_spread <-mlong_anthro_bday %>% 
  select(basebrgy_basewman, weight, height, date_inf_meas, age_inf_meas, birthdate) %>% 
  group_by(basebrgy_basewman) %>%
  mutate(id = row_number()) %>% 
  filter(id == max(id) | id == min(id)) %>% 
  arrange(basebrgy_basewman, id)


mlong_for_spread %>% 
  mutate(weight_diff = weight - lag(weight, default = first(weight)), 
         height_diff = height - lag(height, default = first(height)), 
         time_diff =   age_inf_meas - lag(age_inf_meas)) %>% 
  select(basebrgy_basewman, weight_diff, height_diff, time_diff, everything(.))

###########################################
# Bring in epigenetic age data, color slopes by epigenetic clocks. 
###########################################
###########################################
# New clocks data
clocks_data <- read_csv(here::here ("Data/Clocks/datout_badprobes_notcalled_NA_orderSamples_not_fixed.output.csv")) 

# Remove replicates
# 20361
# 21540
# 22623
# 23222 
# since I have no other reason - remove all but first
# Also, remove 1 sample that has an 'evaporated' replicate (23222)
# Also, remove 1 sample that has only "rep" (have to include the id number because str_detect will just pull of any that have "rep" in the name).

clocks_data <-clocks_data %>% 
  filter(!str_detect(Sample_Name, "rep2") & 
           !str_detect(Sample_Name, "rep3") &
           !str_detect(Sample_Name, "rep4") &
           !str_detect(Sample_Name, "rep5") &
           !str_detect(Sample_Name, "rep6") &
           !str_detect(Sample_Name, "rep7") &
           !str_detect(Sample_Name, "rep8") &
           !str_detect(Sample_Name, "rep9") &
           !str_detect(Sample_Name, "rep10") &
           !str_detect(Sample_Name, "_evaporated") &
           !str_detect(Sample_Name, "22623_rep")) 


clocks_data <-clocks_data %>% 
  unite("basebrgy_basewman", c(basebrgy, basewman))


growth_clocks_data <- read_csv(here::here ("Output/Data/CPR", "growth_clocks_data.csv"))

mlong_anthro_bday_clocks <-left_join(mlong_anthro_bday, growth_clocks_data, by = "basebrgy_basewman")




mlong_anthro_bday_clocks %>% 
  filter(!is.na(sexchild.x) & age_inf_meas > -10) %>%
#  filter(sexchild.x == 2) %>% 
  mutate(AgeAccel_bin = cut_interval(AgeAccelGrim, n = 2)) %>% 
  select(basebrgy_basewman, height, sexchild.x, age_inf_meas, AgeAccel_bin, was_preg_no_na) %>% 
  ggplot(., aes(x = age_inf_meas, y = height))+
  # geom_point(alpha = 0.2)+
  geom_line(alpha = 0.2, aes(group = basebrgy_basewman, col = AgeAccel_bin))+
  geom_smooth(aes(group = AgeAccel_bin, col = as_factor(AgeAccel_bin)))+
  facet_wrap(~sexchild.x)+
  scale_color_manual(values = c("green", "blue"))+
  theme_bw()
