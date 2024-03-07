#### Post-Processing ALL Co-incident Data ####
#Load Libraries
library(tidyverse)

#### Load Combined GEE & In situ Data ####
#5_Day
df <- read.csv("C:/Johansen_FY_22/Generalizable_HAB_Algorithms_v2/Data/Post_Processed_Data/Nationwide_HAB_Data_Combined_5Day_All_Huc2.csv")
#3_Day
df <- read.csv("C:/Johansen_FY_22/Generalizable_HAB_Algorithms_v2/Data/Post_Processed_Data/Nationwide_HAB_Data_Combined_3Day_All_Huc2.csv")
#1_Day
df <- read.csv("C:/Johansen_FY_22/Generalizable_HAB_Algorithms_v2/Data/Post_Processed_Data/Nationwide_HAB_Data_Combined_1Day_All_Huc2.csv")

#### Selecting SCL Classes ####
#Select Water Class only from SCL Layer
#table(National_HAB_SCL_All$SCL)
# 0	NO_DATA
# 1	SATURATED_OR_DEFECTIVE
# 2	CAST_SHADOWS
# 3	CLOUD_SHADOWS
# 4	VEGETATION
# 5	NOT_VEGETATED
# 6	WATER
# 7	UNCLASSIFIED
# 8	CLOUD_MEDIUM_PROBABILITY
# 9	CLOUD_HIGH_PROBABILITY
# 10	THIN_CIRRUS
# 11	SNOW or ICE

# Less restrictive (Exclude only clouds, shadows, ice, etc.) filter
#df_SCL_Less <- filter(df, SCL == "6" | SCL == "4" | SCL == "5")

#More Restrictive (Only Water Pixels)
df <- filter(df, SCL == "6")
df <- df %>% rename(Method_Description = Method_Des)
#### Exploring and Subsetting Methodologies #####
#df <- read.csv('C:/Johansen_FY_22/Generalizable_HAB_Algortithms/Data/Combined_InSitu_WQA_Data/CloudCover50_SCL/National_HAB_Sites_LikelyWater_ALL.csv')

#### Remove erroneous Methods ####
df_Sub <-df[!(df$Method_Description =="Allophycocyanin, water, filtered, recoverable, micrograms per liter"
              | df$Method_Description=="Biomass to chlorophyll ratio, periphyton, number" 
            | df$Method_Description=="Chlorophyll a, periphyton, chromatographic-fluorometric method, milligrams per square meter" 
            | df$Method_Description== "Chlorophyll a, fluorometric method, uncorrected, micrograms per liter" 
            | df$Method_Description== "Cyanobacteria fluorescence of phycocyanin (fPC), water, in situ, concentration estimated from reference material, cells per milliliter"
            | df$Method_Description== "Chlorophyll a, fluorometric method, uncorrected, micrograms per liter" 
            | df$Method_Description== "Chlorophyll a, Corrected mg/m3"
            | df$Method_Description== "Chlorophyll a, phytoplankton, spectrophotometric acid method, micrograms per liter"),]

write.csv(df_Sub, "C:/Johansen_FY_22/Generalizable_HAB_Algorithms_v2/Data/FINAL DATA/Nationwide_HAB_Data_Combined_1Day_All_Huc2_Water.csv", row.names = FALSE)

group <- df_Sub %>%
  group_by(Method_Description) %>%
  summarise(min_WQ= min(WQ_Value),
            max_WQ = max(WQ_Value),
            mean_WQ = mean(WQ_Value),
            std_WQ = sd(WQ_Value))

write.csv(group,'C:/Johansen_FY_22/Generalizable_HAB_Algorithms_v2/Data/FINAL DATA/Nationwide_HAB_Data_Combined_1Day_All_Huc2_Water_Table.csv')


#### DO NOT GROUP DATA! ####
#### Group Methods into categories ####
df_Sub$Category <- ifelse(df_Sub$Method_Description=="Chlorophyll a, phytoplankton, chromatographic-spectrophotometric method, micrograms per liter"
                          | df_Sub$Method_Description== "Chlorophyll a, water, trichromatic method, uncorrected, micrograms per liter", 1,
                          ifelse(df_Sub$Method_Description=="Chlorophyll a, phytoplankton, chromatographic-fluorometric method, micrograms per liter"
                                 | df_Sub$Method_Description=="Chlorophylls, water, in situ, fluorometric method, excitation at 470 +-15 nm, emission at 685 +-20 nm, micrograms per liter"
                                 | df_Sub$Method_Description=="Chlorophyll a, phytoplankton, water, filtered (5 micron filter), chromatographic-fluorometric method, micrograms per liter", 2,
                                 ifelse(df_Sub$Method_Description=="YSI EXO Collected Field Total Blue Green Algae"
                                        | df_Sub$Method_Description== "Chlorophyll fluorescence (fChl), water, in situ, concentration estimated from reference material, micrograms per liter as chlorophyll", 3,
                                        ifelse(df_Sub$Method_Description=="Chlorophyll fluorescence (fChl), water, in situ, fluorometric method, excitation at 470 +-15 nm, emission at 685 +-20 nm, relative fluorescence units (RFU)"
                                               | df_Sub$Method_Description== "Chlorophyll relative fluorescence (fChl), water, in situ, relative fluorescence units (RFU)", 4,
                                               ifelse(df_Sub$Method_Description=="Phycocyanin fluorescence (fPC), water, in situ, concentration estimated from reference material, micrograms per liter as phycocyanin", 5,
                                                      ifelse(df_Sub$Method_Description=="Phycocyanin relative fluorescence (fPC), water, in situ, relative fluorescence units (RFU)"
                                                             | df_Sub$Method_Description=="Phycocyanins, water, filtered, recoverable, micrograms per liter"
                                                             | df_Sub$Method_Description== "YSI EXO Collected Field Total Chlorophyll", 6,
                                                             ifelse(df_Sub$Method_Description== "Phycocyanins (cyanobacteria), water, in situ, in vivo fluorescence, in vivo fluorescence units", 7,
                                                                    NA))))))) # Rest to NA (should be None)

write.csv(df_Sub, "C:/Johansen_FY_22/Generalizable_HAB_Algorithms_v2/Data/Post_Processed_Data/Nationwide_HAB_Data_Combined_1Day_All_Huc2_Water.csv", row.names = FALSE)

