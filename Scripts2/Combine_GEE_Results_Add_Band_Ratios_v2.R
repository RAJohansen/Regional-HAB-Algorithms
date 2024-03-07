#### Combine All GEE Results & Calculate Band Ratios ####
#Load Libraries
library(tidyverse)

#Combine all bands
setwd("C:/Johansen_FY_22/Generalizable_HAB_Algorithms_v2/Data/Combined_GEE_In_Situ_Data/1_Day")
df2 <- list.files(path = "C:/Johansen_FY_22/Generalizable_HAB_Algorithms_v2/Data/Combined_GEE_In_Situ_Data/1_Day",
                  pattern = "*.csv") %>% 
  lapply(read_csv) %>%
  Reduce(function(x, y) left_join(x, y),.)

df2$NDCI <- (df2$B5-df2$B4)/(df2$B5+df2$B4)
df2$TwoBDA <- df2$B5/df2$B4
df2$RE10 <- (df2$B5-df2$B8A)/(df2$B4-df2$B8A)
df2$MCI <- df2$B5-df2$B4+(df2$B6-df2$B4)*(0.52)
df2$ThreeBDA <- ((df2$B4^-1)-(df2$B5^-1))-df2$B6
df2$NDCI_alt <- (df2$B4-df2$B3)/(df2$B4+df2$B3)
df2$TwoBDA_alt <- df2$B4/df2$B3
df2$MCI_alt <- df2$B4-df2$B3+(df2$B5-df2$B3)*(0.72)
df2$ThreeBDA_alt <- ((df2$B3^-1)-(df2$B4^-1))-df2$B5

write.csv(df2,'C:/Johansen_FY_22/Generalizable_HAB_Algorithms_v2/Data/Combined_GEE_In_Situ_Data/1_Day/Nationwide_HAB_Data_Combined_1DAY_ALL.csv', row.names = FALSE)

#### ARCHIVED APPROACH #####
B2 <- read.csv('C:/Johansen_FY_22/Generalizable_HAB_Algorithms_v2/Data/Combined_GEE_In_Situ_Data/1_Day/Nationwide_HAB_Data_Combined_B2_1Day.csv')
B3 <- read.csv('C:/Johansen_FY_22/Generalizable_HAB_Algorithms_v2/Data/Combined_GEE_In_Situ_Data/1_Day/Nationwide_HAB_Data_Combined_B3_1Day.csv')
B4 <- read.csv('C:/Johansen_FY_22/Generalizable_HAB_Algorithms_v2/Data/Combined_GEE_In_Situ_Data/1_Day/Nationwide_HAB_Data_Combined_B4_1Day.csv')
B5 <- read.csv('C:/Johansen_FY_22/Generalizable_HAB_Algorithms_v2/Data/Combined_GEE_In_Situ_Data/1_Day/Nationwide_HAB_Data_Combined_B5_1Day.csv')
B6 <- read.csv('C:/Johansen_FY_22/Generalizable_HAB_Algorithms_v2/Data/Combined_GEE_In_Situ_Data/1_Day/Nationwide_HAB_Data_Combined_B6_1Day.csv')
B7 <- read.csv('C:/Johansen_FY_22/Generalizable_HAB_Algorithms_v2/Data/Combined_GEE_In_Situ_Data/1_Day/Nationwide_HAB_Data_Combined_B7_1Day.csv')
SCL <- read.csv('C:/Johansen_FY_22/Generalizable_HAB_Algorithms_v2/Data/Combined_GEE_In_Situ_Data/1_Day/Nationwide_HAB_Data_Combined_SCL_1Day.csv')
#NDCI_GEE <- read.csv('C:/Johansen_FY_22/Generalizable_HAB_Algorithms_v2/Data/Combined_GEE_In_Situ_Data/5_Day/Nationwide_HAB_Data_Combined_NDCI.csv')

All_GEE <- left_join(B2, B3, by.x=c('Site_ID','Date')) %>%
  left_join(., B4, by.x=c('Site_ID','Date')) %>%
  left_join(., B5, by.x=c('Site_ID','Date')) %>%
  left_join(., B6, by.x=c('Site_ID','Date')) %>%
  left_join(., B7, by.x=c('Site_ID','Date')) %>%
  left_join(., SCL, by.x=c('Site_ID','Date')) 

All_GEE$SCL <- as.factor(All_GEE$SCL)
#left_join(., NDCI_GEE, by.x=c('Site_ID','Date'))

# Calculate Band Ratio Algorithms #
All_GEE$NDCI <- (All_GEE$B5-All_GEE$B4)/(All_GEE$B5+All_GEE$B4)
All_GEE$NDCI_alt <- (All_GEE$B4-All_GEE$B3)/(All_GEE$B4+All_GEE$B3)
All_GEE$TwoBDA <- All_GEE$B5/All_GEE$B4
All_GEE$TwoBDA_alt <- All_GEE$B4/All_GEE$B3
All_GEE$MCI <- All_GEE$B5-All_GEE$B4+(All_GEE$B6-All_GEE$B4)*(0.52)
All_GEE$MCI_alt <- All_GEE$B4-All_GEE$B3+(All_GEE$B5-All_GEE$B3)*(0.72)
All_GEE$ThreeBDA <- ((All_GEE$B4^-1)-(All_GEE$B5^-1))-All_GEE$B6
All_GEE$ThreeBDA_alt <- ((All_GEE$B3^-1)-(All_GEE$B4^-1))-All_GEE$B5

write.csv(All_GEE,'C:/Johansen_FY_22/Generalizable_HAB_Algorithms_v2/Data/Combined_GEE_In_Situ_Data/1_Day/Nationwide_HAB_Data_Combined_1DAY_ALL.csv', row.names = FALSE)

