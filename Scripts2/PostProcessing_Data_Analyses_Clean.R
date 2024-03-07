#### Final Analyses Cleaned ####
#Load Libraries
library(tidyverse)
library(magrittr)
library(broom)
library(lme4)
library(waterquality)

#Load Data
#5_Day
df_5Day <- read.csv("C:/R_Packages/Comprehensive_Algorithms/Data-Processed/Nationwide_HAB_5Day_Huc2_Water.csv")

df_5Day <- df_5Day %>% 
  filter(WQ_Value >= 5) %>% 
  filter(Parameter == "Chl")

#3_Day
df_3Day <- read.csv("C:/R_Packages/Comprehensive_Algorithms/Data-Processed/Nationwide_HAB_3Day_Huc2_Water.csv")

df_3Day <- df_3Day %>% 
  filter(WQ_Value >= 5) %>% 
  filter(Parameter == "Chl")

#1_Day
df_1Day <- read.csv("C:/R_Packages/Comprehensive_Algorithms/Data-Processed/Nationwide_HAB_1Day_Huc2_Water.csv")

df_1Day <- df_1Day %>% 
  filter(WQ_Value >= 5) %>% 
  filter(Parameter == "Chl")

#### Set Column Types #### 
df_5Day$Date <- as.Date(df_5Day$Date, format= "%m/%d/%Y")
#df_5Day <- df_5Day %>% rename(Method_Description = Method_Des)
df_5Day <- df_5Day %>% rename(region = name)
#df_5Day <- df_5Day %>% dplyr::mutate_at(c('B2','B3','B4','B5','B6','B7'), as.numeric)

df_3Day$Date <- as.Date(df_3Day$Date, format= "%m/%d/%Y")
#df_3Day <- df_3Day %>% rename(Method_Description = Method_Des)
df_3Day <- df_3Day %>% rename(region = name)
#df_3Day <- df_3Day %>% dplyr::mutate_at(c('B2','B3','B4','B5','B6','B7'), as.numeric)

df_1Day$Date <- as.Date(df_1Day$Date, format= "%m/%d/%Y")
#df_3Day <- df_3Day %>% rename(Method_Description = Method_Des)
df_1Day <- df_1Day %>% rename(region = name)
#df_3Day <- df_3Day %>% dplyr::mutate_at(c('B2','B3','B4','B5','B6','B7'), as.numeric)


##### Data Refinement and Filtering #####
#### 5 Day ####
df_5Day_out <-
  df_5Day %>% 
  group_by(Method_Description) %>%
  rstatix::identify_outliers(WQ_Value)

# Remove Outliers
df_5Day_filter <- df_5Day %>% 
  anti_join(df_5Day_out, by = c("Site_ID", "Date")) 

df_5Day_filter_SS <- df_5Day_filter %>% 
  group_by(Method_Description) %>%
  summarize(
    min = min(WQ_Value),
    max = max(WQ_Value),
    mean = mean(WQ_Value),
    nobs = n())


df_5Day_filter <- df_5Day_filter %>% 
filter(Method_Description == "Chlorophyll a, phytoplankton, chromatographic-fluorometric method, micrograms per liter" |
         Method_Description == "Chlorophylls, water, in situ, fluorometric method, excitation at 470 +-15 nm, emission at 685 +-20 nm, micrograms per liter" |
         Method_Description == "Chlorophyll a, water, trichromatic method, uncorrected, micrograms per liter" |
         Method_Description == "Chlorophyll fluorescence (fChl), water, in situ, fluorometric method, excitation at 470 +-15 nm, emission at 685 +-20 nm, relative fluorescence units (RFU)" |
         Method_Description == "Chlorophyll a, water, fluorometric method, corrected, micrograms per liter" |
         Method_Description == "Chlorophyll relative fluorescence (fChl), water, in situ, relative fluorescence units (RFU)" |
         Method_Description == "Chlorophyll fluorescence (fChl), water, in situ, concentration estimated from reference material, micrograms per liter as chlorophyll")


write.csv(df_5Day_filter, "Data-Processed/Final_HAB_Data_Filtered/HAB_5Day_Filtered.csv")

#### 3 Day ####
df_3Day_out <-
  df_3Day %>% 
  group_by(Method_Description) %>%
  rstatix::identify_outliers(WQ_Value)

# Remove Outliers
df_3Day_filter <- df_3Day %>% 
  anti_join(df_3Day_out, by = c("Site_ID", "Date")) 

df_3Day_filter_SS <- df_3Day_filter %>% 
  group_by(Method_Description) %>%
  summarize(
    min = min(WQ_Value),
    max = max(WQ_Value),
    mean = mean(WQ_Value),
    nobs = n())

df_3Day_filter <- df_3Day_filter %>% 
  filter(Method_Description == "Chlorophyll a, phytoplankton, chromatographic-fluorometric method, micrograms per liter" |
           Method_Description == "Chlorophylls, water, in situ, fluorometric method, excitation at 470 +-15 nm, emission at 685 +-20 nm, micrograms per liter" |
           Method_Description == "Chlorophyll a, water, trichromatic method, uncorrected, micrograms per liter" |
           Method_Description == "Chlorophyll fluorescence (fChl), water, in situ, fluorometric method, excitation at 470 +-15 nm, emission at 685 +-20 nm, relative fluorescence units (RFU)" |
           Method_Description == "Chlorophyll a, water, fluorometric method, corrected, micrograms per liter")

write.csv(df_3Day_filter, "Data-Processed/Final_HAB_Data_Filtered/HAB_3Day_Filtered.csv")

#### 1 Day ####
df_1Day_out <-
  df_1Day %>% 
  group_by(Method_Description) %>%
  rstatix::identify_outliers(WQ_Value)

# Remove Outliers
df_1Day_filter <- df_1Day %>% 
  anti_join(df_1Day_out, by = c("Site_ID", "Date")) 

# Method Summary Stats
df_1Day_filter_SS <- df_1Day_filter %>% 
  group_by(Method_Description) %>%
  summarize(
    min = min(WQ_Value),
    max = max(WQ_Value),
    mean = mean(WQ_Value),
    nobs = n())

df_1Day_filter <- df_1Day_filter %>% 
  filter(Method_Description == "Chlorophyll a, phytoplankton, chromatographic-fluorometric method, micrograms per liter" |
           Method_Description == "Chlorophylls, water, in situ, fluorometric method, excitation at 470 +-15 nm, emission at 685 +-20 nm, micrograms per liter" |
           Method_Description == "Chlorophyll a, water, trichromatic method, uncorrected, micrograms per liter")

write.csv(df_1Day_filter, "Data-Processed/Final_HAB_Data_Filtered/HAB_1Day_Filtered.csv")






































#### Subset Data to growing season only #### 
df_5Day_GS <- df_5Day[df_5Day$Date >= "2019-05-01" & df_5Day$Date <= "2019-10-31",] %>% 
  rbind(df_5Day[df_5Day$Date >= "2020-05-01" & df_5Day$Date <= "2020-10-31",]) %>% 
  rbind(df_5Day[df_5Day$Date >= "2021-05-01" & df_5Day$Date <= "2021-10-31",]) %>%
  rbind(df_5Day[df_5Day$Date >= "2022-05-01" & df_5Day$Date <= "2022-10-31",])

df_3Day_GS <- df_3Day[df_3Day$Date >= "2019-05-01" & df_3Day$Date <= "2019-10-31",] %>% 
  rbind(df_3Day[df_3Day$Date >= "2020-05-01" & df_3Day$Date <= "2020-10-31",]) %>% 
  rbind(df_3Day[df_3Day$Date >= "2021-05-01" & df_3Day$Date <= "2021-10-31",]) %>%
  rbind(df_3Day[df_3Day$Date >= "2022-05-01" & df_3Day$Date <= "2022-10-31",])

#### Plotting using specific data ####
df_5Day %>%
  filter(., Method_Description == 'Chlorophyll a, phytoplankton, chromatographic-fluorometric method, micrograms per liter') %>% 
  select(., WQ_Value,NDCI) %>%
  ggplot(aes(NDCI, WQ_Value)) +
  geom_point() +
  geom_smooth(method='lm',se = FALSE, col = "darkgray") +
  geom_smooth(method='gam',se = FALSE, col = "blue") +
  xlab("NDCI")+
  ylab("Chl-a\nlog(ug/L)") +
  theme_bw()+
  theme(axis.title.y = element_text(angle=0, vjust=0.5))

#### Plot by Algorithm (Long Data)
# Convert to long form
df_long <- df_5Day %>% gather(., Algorithm, Index_Value, NDCI:ThreeBDA_a, factor_key=TRUE)
df_long %>%
  group_by(Method_Description) %>% 
  filter(n()>25) %>%
  ggplot(aes(Index_Value, WQ_Value)) +
  geom_point() +
  facet_wrap(Method_Description~Algorithm,  scales="free") + 
  geom_smooth(method='lm',se = FALSE, col = "darkgray") +
  geom_smooth(method='gam',se = FALSE, col = "blue") +
  theme_bw()

#### Run CV-LM from waterquality and Extract Best####
## Grouped by method & HUC
cvlm_meth_huc2 <- df_3Day %>%
  mutate(WQ_Value_log = log(WQ_Value)) %>% 
  group_by(Method_Description,region) %>%
  filter(n()>10) %>%
  select(.,-Latitude,-Longitude,-areasqkm) %>%
  do(waterquality::extract_lm_cv_all(parameters = c('WQ_Value','WQ_Value_log'), df=.)) 

cvlm_meth_huc2 %>%
  filter(.,Method_Description == 'Chlorophyll fluorescence (fChl), water, in situ, concentration estimated from reference material, micrograms per liter as chlorophyll' &
           Algorithms == 'WQ_Value_log_TwoBDA_alt')

# Extract the best(highest CV R squared) HUC-Method Pair 
cvlm_meth_huc2_best <- cvlm_meth_huc2 %>% 
  group_by(Method_Description) %>% 
  filter(CV_R_Squared== max(CV_R_Squared)) %>% 
  arrange(CV_R_Squared)

# Create frequency table for huc-method combinations (>10 observations)
vars = c("Method_Description", "region")  
cvlm_meth_huc2_count <- df_3Day %>%
  group_by(Method_Description, region) %>%
  filter(n()>10) %>% 
  plyr::count(., vars)

#Combine frequency with HUC-method Pairs 
cvlm_meth_huc2_best_final <- left_join(cvlm_meth_huc2_best,cvlm_meth_huc2_count)
write.csv(cvlm_meth_huc2_best_final, "C:/R_Packages/Comprehensive_Algorithms/Results/Top_Performer/CV_Results_3Day_methods_HUC2_Top.csv", row.names = FALSE)

cvlm_meth_huc2_all_final <- left_join(cvlm_meth_huc2,cvlm_meth_huc2_count)
write.csv(cvlm_meth_huc2_all_final, "C:/R_Packages/Comprehensive_Algorithms/Results/All/CV_Results_3Day_methods_HUC2.csv", row.names = FALSE)


#### EXTRAS ####

# Subset Data to growing season only #
df_5Day_GS <- df_5Day[df_5Day$Date >= "2019-05-01" & df_5Day$Date <= "2019-10-31",] %>% 
  rbind(df_5Day[df_5Day$Date >= "2020-05-01" & df_5Day$Date <= "2020-10-31",]) %>% 
  rbind(df_5Day[df_5Day$Date >= "2021-05-01" & df_5Day$Date <= "2021-10-31",]) %>%
  rbind(df_5Day[df_5Day$Date >= "2022-05-01" & df_5Day$Date <= "2022-10-31",])

df_3Day_GS <- df_3Day[df_3Day$Date >= "2019-05-01" & df_3Day$Date <= "2019-10-31",] %>% 
  rbind(df_3Day[df_3Day$Date >= "2020-05-01" & df_3Day$Date <= "2020-10-31",]) %>% 
  rbind(df_3Day[df_3Day$Date >= "2021-05-01" & df_3Day$Date <= "2021-10-31",]) %>%
  rbind(df_3Day[df_3Day$Date >= "2022-05-01" & df_3Day$Date <= "2022-10-31",])

# Plotting using specific data #
df_5Day %>%
  filter(., Method_Description == 'Chlorophyll a, phytoplankton, chromatographic-fluorometric method, micrograms per liter') %>% 
  select(., WQ_Value,NDCI) %>%
  ggplot(aes(NDCI, WQ_Value)) +
  geom_point() +
  geom_smooth(method='lm',se = FALSE, col = "darkgray") +
  geom_smooth(method='gam',se = FALSE, col = "blue") +
  xlab("NDCI")+
  ylab("Chl-a\nlog(ug/L)") +
  theme_bw()+
  theme(axis.title.y = element_text(angle=0, vjust=0.5))

# Plot by Algorithm (Long Data)
# Convert to long form
df_long <- df_5Day %>% gather(., Algorithm, Index_Value, NDCI:ThreeBDA_a, factor_key=TRUE)
df_long %>%
  group_by(Method_Description) %>% 
  filter(n()>10) %>%
  ggplot(aes(Index_Value, WQ_Value)) +
  geom_point() +
  facet_wrap(Method_Description~Algorithm,  scales="free") + 
  geom_smooth(method='lm',se = FALSE, col = "darkgray") +
  geom_smooth(method='gam',se = FALSE, col = "blue") +
  theme_bw()
