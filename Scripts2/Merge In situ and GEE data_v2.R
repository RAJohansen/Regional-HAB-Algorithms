### Merge In situ and GEE data
library(tidyverse)

#### Merge GEE Results ####
# Import & cbind GEE NDCI  Results
file_paths <- list.files("C:/Johansen_FY_22/Generalizable_HAB_Algorithms_v2/Data/GEE_Results/B4", pattern = ".csv", full.names = TRUE) #Change Folder

df <- do.call(cbind, lapply(file_paths, function(x) 
  transform(read.csv(x), store = sub(".csv", "", basename(x)))))

# Set all -9999 values to NA so they can be removed
df[df == -9999] <- NA

#Convert Dates and Values into long format
df_gather <- df %>%
  tidyr::gather(Date,
         B4, # Change Algorithm Name
         starts_with("X"),
         na.rm = TRUE)

#Remove extra columns
df_gather <- df_gather %>% subset(select = c(Site_ID,Date,B4)) #Change Algorithm Name

#Convert Date column into dates
df_gather$Date <- sub('X', '', df_gather$Date)
df_gather$Date <- as.Date(df_gather$Date, format= "%Y%m%d")


#### Import CLEAN in situ and gee results for matching dates and sites ####
in_situ <- read.csv('C:/Johansen_FY_22/Generalizable_HAB_Algorithms_v2/Data/In_Situ_WQ_Data/National_HAB_Data_clean.csv')

# Format Date as Date
in_situ$Date <- as.Date(in_situ$Date, format= "%Y-%m-%d")

# Add columns for +/- 2 days of acquisition
in_situ_plus24 <- in_situ
in_situ_plus24$Date <- in_situ_plus24$Date+1

in_situ_plus48 <- in_situ
in_situ_plus48$Date <- in_situ_plus48$Date+2

in_situ_minus24 <- in_situ
in_situ_minus24$Date <- in_situ_minus24$Date-1

in_situ_minus48 <- in_situ
in_situ_minus48$Date <- in_situ_minus48$Date-2

#### Merge Time series GEE results with In situ data ####
HAB <- merge(in_situ, df_gather, by.x = c("Site_ID", "Date"), all.x =FALSE)
#HAB_plus24 <- merge(in_situ_plus24, df_gather, by.x = c("Site_ID", "Date"),all.x =FALSE)
#HAB_minus24 <- merge(in_situ_minus24, df_gather, by.x = c("Site_ID", "Date"),all.x =FALSE)
#HAB_plus48 <- merge(in_situ_plus48, df_gather, by.x = c("Site_ID", "Date"),all.x =FALSE)
#HAB_minus48 <- merge(in_situ_minus48, df_gather, by.x = c("Site_ID", "Date"),all.x =FALSE)

#Combine data sets for day of acquisitions, plus and minus 24 hour of acquisition
#HAB_All_5 <- rbind(HAB,HAB_plus24,HAB_minus24,HAB_plus48,HAB_minus48)
#HAB_All_3 <- rbind(HAB,HAB_plus24,HAB_minus24)

#HAB_All$Date_Ext <- HAB_All_plusminus$Date
#HAB_All <- select(HAB_All, -Date_minus24,-Date_plus24)

# Reorganize Columns
HAB_1 <- select(HAB, "X", "Site_ID", "Date","Latitude","Longitude","Parameter","WQ_Value",everything())   
#HAB_All_5 <- select(HAB_All_5, "X", "Site_ID", "Date","Latitude","Longitude","Parameter","WQ_Value",everything())   
#HAB_All_3 <- select(HAB_All_3, "X", "Site_ID", "Date","Latitude","Longitude","Parameter","WQ_Value",everything())   

#Export
write.csv(HAB_1,'C:/Johansen_FY_22/Generalizable_HAB_Algorithms_v2/Data/Combined_GEE_In_Situ_Data/1_Day/Nationwide_HAB_Data_Combined_B4_1Day.csv', row.names = FALSE)
#write.csv(HAB_All_5,'C:/Johansen_FY_22/Generalizable_HAB_Algorithms_v2/Data/Combined_GEE_In_Situ_Data/Nationwide_HAB_Data_Combined_B7_5Day.csv', row.names = FALSE)
#write.csv(HAB_All_3,'C:/Johansen_FY_22/Generalizable_HAB_Algorithms_v2/Data/Combined_GEE_In_Situ_Data/Nationwide_HAB_Data_Combined_B7_3Day.csv', row.names = FALSE)


