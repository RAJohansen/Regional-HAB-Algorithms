library(tidyverse)
library(magrittr)
library(broom)
library(lme4)

#### Refined Datasets analyses####
df5 <- read.csv("Data-Processed/Final_HAB_Data_Filtered/HAB_5Day_Filtered.csv")
df3 <- read.csv("Data-Processed/Final_HAB_Data_Filtered/HAB_3Day_Filtered.csv")
df1 <- read.csv("Data-Processed/Final_HAB_Data_Filtered/HAB_1Day_Filtered.csv")

## Set Date Column
df$Date <- as.Date(df$Date, format= "%m/%d/%Y")

### Extract lm Cv MAPE ####
extract_lm_cv_MAPE <- function(parameter, algorithm, df, train_method = "lm", control_method = "repeatedcv", folds = 3, nrepeats =5){
  if (!requireNamespace("caret", quietly = TRUE))
    stop("package caret required, please install it first") 
  my_formula = as.formula(paste(parameter, "~" ,algorithm))
  caret_model = caret::train(form = my_formula,
                             data = df,
                             method = "lm",
                             na.action = na.exclude,
                             #repeated k-fold validation
                             trControl = caret::trainControl(method = "repeatedcv",
                                                             number = folds, repeats = nrepeats))
  my_lm = caret_model$finalModel
  CV_R_Squared = caret::getTrainPerf(caret_model)[, "TrainRsquared"]
  RMSE = caret::getTrainPerf(caret_model)[, "TrainRMSE"]
  MAE = caret::getTrainPerf(caret_model)[, "TrainMAE"]
  MAPE = mean(abs((caret_model$trainingData$.outcome-caret_model$finalModel$fitted.values)/caret_model$trainingData$.outcome)) * 100
  R_Squared = summary(my_lm)$r.squared
  P_Value = summary(my_lm)$coefficients[8]
  Slope = summary(my_lm)$coefficients[2]
  Intercept = summary(my_lm)$coefficients[1]
  tibble::tibble(R_Squared = R_Squared, Slope = Slope, Intercept = Intercept, 
                 P_Value = P_Value, CV_R_Squared = CV_R_Squared, RMSE = RMSE, MAE = MAE, MAPE = MAPE)
}
#### Extract all MAPE ####
extract_lm_cv_all_MAPE <- function(parameters, df, train_method = "lm", control_method = "repeatedcv", folds = 3, nrepeats = 5){
  if (!requireNamespace("caret", quietly = TRUE))
    stop("package caret required, please install it first") 
  list = list()
  for (i in seq_along(parameters)) {
    algorithms = df %>%
      dplyr::select(which(sapply(., class) == "numeric"), -parameters) %>%
      names()
    names(algorithms) <- algorithms %>%
      purrr::map_chr(., ~ paste0(parameters[[i]], "_", .))
    list[[i]] = algorithms %>%
      purrr::map_dfr(~extract_lm_cv_MAPE(parameter = parameters[[i]], algorithm = ., df = df, 
                                         train_method = train_method, control_method = control_method,
                                         folds = folds, nrepeats = nrepeats), .id = "Algorithms")
  }
  extract_lm_cv_all_results <- (do.call(rbind, list))
}

#### Theil Sen Function ####
sen <- function(..., weights = NULL) {
  RobustLinearReg::theil_sen_regression(...)
}

#### Plot ####
trace(ggpubr:::.stat_lm, edit = TRUE)

#TwoBDA Chl1
p1 <- df %>%
  filter(Method_Description == 'Chlorophyll a, phytoplankton, chromatographic-fluorometric method, micrograms per liter') %>% 
  ggplot(aes(TwoBDA, WQ_Value)) +           
  geom_point(size =2) +
  ggnewscale::new_scale_colour() +
  stat_smooth(method = "lm",
              formula = y~x,
              geom = "smooth", se=FALSE) +
  coord_cartesian(ylim=c(0, 175)) +
  ggpubr::stat_regline_equation(label.y = 165, aes(label = ..eq.label..)) +
  ggpubr::stat_regline_equation(label.y = 150, aes(label = ..rr.label..)) +
  labs(x="",
       y = paste("Chl\u00b9","(\U00B5g/L)"))+
  theme_bw(base_size = 14, base_family = "Times") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = ggtext::element_markdown(angle = 0, vjust = 0.5))

#TwoBDA Chl2
p2 <- df %>%
  filter(Method_Description == 'Chlorophylls, water, in situ, fluorometric method, excitation at 470 +-15 nm, emission at 685 +-20 nm, micrograms per liter') %>% 
  ggplot(aes(TwoBDA, WQ_Value)) +           
  geom_point(size =2) +
  ggnewscale::new_scale_colour() +
  stat_smooth(method = "lm",
              formula = y~x,
              geom = "smooth", se=FALSE) +
  coord_cartesian(ylim=c(0, 35)) +
  ggpubr::stat_regline_equation(label.y = 33, aes(label = ..eq.label..)) +
  ggpubr::stat_regline_equation(label.y = 29, aes(label = ..rr.label..)) +
  labs(x= "",
       y = paste("Chl\u00b2","(\U00B5g/L)"))+
  theme_bw(base_size = 14, base_family = "Times") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(angle = 0, vjust = 0.5))

#TwoBDA Chl3
p3 <- df %>%
  filter(Method_Description == 'Chlorophyll a, water, trichromatic method, uncorrected, micrograms per liter') %>% 
  ggplot(aes(TwoBDA, WQ_Value)) +           
  geom_point(size =2) +
  ggnewscale::new_scale_colour() +
  stat_smooth(method = "lm",
              formula = y~x,
              geom = "smooth", se=FALSE) +
  ggpubr::stat_regline_equation(label.y = 75, aes(label = ..eq.label..)) +
  ggpubr::stat_regline_equation(label.y = 65, aes(label = ..rr.label..)) +
  labs(x = "2BDA\n(Band Ratio)",
       y = paste("Chl\u00b3","(\U00B5g/L)"))+
  theme_bw(base_size = 14, base_family = "Times") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(angle = 0, vjust = 0.5))

# Visualize
plots <- gridExtra::grid.arrange(p1, p2, p3, nrow = 3)

#save
plotsave <- gridExtra::arrangeGrob(p1, p2, p3, nrow=3) #generates g
ggsave("Results/Figures/2BDA_1Day_allmethods.jpeg", plotsave, width = 8,height = 6, dpi = 300)


#### Run cvkms ####
#Grouped by method & HUC
results1 <- df1 %>%
  filter(Method_Description == "Chlorophyll a, phytoplankton, chromatographic-fluorometric method, micrograms per liter" |
           Method_Description == "Chlorophylls, water, in situ, fluorometric method, excitation at 470 +-15 nm, emission at 685 +-20 nm, micrograms per liter" |
           Method_Description == "Chlorophyll a, water, trichromatic method, uncorrected, micrograms per liter") %>% 
  group_by(Method_Description) %>%
  select(WQ_Value,TwoBDA,NDCI,MCI,Method_Description) %>%
  do(extract_lm_cv_all_MAPE(parameters = c('WQ_Value'), df=.))   %>% 
  mutate(Temporal_Window = '1')

results <- rbind(results1,results3,results5)

write.csv(results, "Results/Final_Filter/Nationwide_HAB_2BDA_temporalwindows_5ugl.csv")

#### Filter by method and calculate Chl
Annual_df1 <- df1 %>%
  filter(Method_Description == "Chlorophyll a, phytoplankton, chromatographic-fluorometric method, micrograms per liter") %>% 
  mutate(Year = str_sub(Date,start = -4))

table(Annual_df1$Year)

#df1
#2019 2020 2021 
#56   75   84

#df5
#2019 2020 2021 
#241  301  357

df1_2019_chl1 <- Annual_df1 %>% 
  filter(Year == "2019") %>% 
  select(WQ_Value,TwoBDA,NDCI,MCI) %>%
do(extract_lm_cv_all_MAPE(parameters = c('WQ_Value'), df=.)) %>% 
  mutate(Year = '2019')

df1_2020_chl1 <- Annual_df1 %>% 
  filter(Year == "2020") %>% 
  select(WQ_Value,TwoBDA,NDCI,MCI) %>% 
  do(extract_lm_cv_all_MAPE(parameters = c('WQ_Value'), df=.)) %>% 
  mutate(Year = '2020')

df1_2021_chl1 <- Annual_df1 %>% 
  filter(Year == "2021") %>% 
  select(WQ_Value,TwoBDA,NDCI,MCI) %>% 
  do(extract_lm_cv_all_MAPE(parameters = c('WQ_Value'), df=.)) %>% 
  mutate(Year = '2021')

chl1_multiyear <- rbind(df1_2019_chl1,df1_2020_chl1,df1_2021_chl1)
write.csv(chl1_multiyear, "Results/Final_Filter/Nationwide_HAB_MultiYear_Chl1.csv")

df5_2019_chl1 <- Annual_df5 %>% 
  filter(Year == "2019") %>% 
  select(WQ_Value,TwoBDA,NDCI,MCI) %>%
  do(extract_lm_cv_all_MAPE(parameters = c('WQ_Value'), df=.)) %>% 
  mutate(Year = '2019')

df5_2020_chl1 <- Annual_df5 %>% 
  filter(Year == "2020") %>% 
  select(WQ_Value,TwoBDA,NDCI,MCI) %>% 
  do(extract_lm_cv_all_MAPE(parameters = c('WQ_Value'), df=.)) %>% 
  mutate(Year = '2020')

df5_2021_chl1 <- Annual_df5 %>% 
  filter(Year == "2021") %>% 
  select(WQ_Value,TwoBDA,NDCI,MCI) %>% 
  do(extract_lm_cv_all_MAPE(parameters = c('WQ_Value'), df=.)) %>% 
  mutate(Year = '2021')

chl1_multiyear_df5 <- rbind(df5_2019_chl1,df5_2020_chl1,df5_2021_chl1)
write.csv(chl1_multiyear_df5, "Results/Final_Filter/Nationwide_HAB_MultiYear_Chl1.csv")


### Summary of field data by year
df1_2019_chl1_SS <- Annual_df1 %>% 
  filter(Year == "2019") %>% 
  select(WQ_Value) %>%
  pastecs::stat.desc() %>%
  t()
df1_2019_chl1_SS <- as.data.frame(df1_2019_chl1_SS) %>%  
  mutate(Year = "2019")
  
df1_2020_chl1_SS <- Annual_df1 %>% 
  filter(Year == "2020") %>% 
  select(WQ_Value) %>%
  pastecs::stat.desc() %>%
  t()
df1_2020_chl1_SS <- as.data.frame(df1_2020_chl1_SS) %>%  
  mutate(Year = "2020")

df1_2021_chl1_SS <- Annual_df1 %>% 
  filter(Year == "2021") %>% 
  select(WQ_Value) %>%
  pastecs::stat.desc() %>%
  t()
df1_2021_chl1_SS <- as.data.frame(df1_2021_chl1_SS) %>%  
  mutate(Year = "2021")

df1_Chl1_multiyear_SS <- rbind(df1_2019_chl1_SS,df1_2020_chl1_SS,df1_2021_chl1_SS)
write.csv(df1_Chl1_multiyear_SS, "Results/Final_Filter/Nationwide_HAB_MultiYear_Chl1_SS.csv")



#### MCI multiyear Plot ####
trace(ggpubr:::.stat_lm, edit = TRUE)

Annual_df1_2021 <- Annual_df1 %>%
  filter(Year == "2021")
RobustLinearReg::siegel_regression (WQ_Value~MCI, data = Annual_df1_2021)
RobustLinearReg::theil_sen_regression (WQ_Value~MCI, data = Annual_df1_2021)

lm_model <- lm(x~t)

#MCI Chl1 2019
p1 <- Annual_df1 %>%
  filter(Year == "2019") %>% 
  ggplot(aes(MCI, WQ_Value)) +           
  geom_point(size =2) +
  ggnewscale::new_scale_colour() +
  stat_smooth(method = sen, se=FALSE,aes(color="Blue")) +
  stat_smooth(method = "lm",
              formula = y~x,
              geom = "smooth", se=FALSE,aes(color="Red")) +
  coord_cartesian(ylim=c(0, 180)) +
  ggpubr::stat_regline_equation(label.y = 163, aes(label = ..eq.label..)) +
  ggpubr::stat_regline_equation(label.y = 150, aes(label = ..rr.label..)) +
  annotate("text", x=-65, y=180, label= "TS:y=24.59 + 0.103x") +
  labs(x="MCI (2019)",
       y = paste("Chl\u00b9","(\U00B5g/L)"))+
  scale_color_identity(name = "Fit Function",
                       labels = c("Theil–Sen", "Least Squares"),
                       guide = "legend")+
  theme_bw(base_size = 14, base_family = "Times") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = ggtext::element_markdown(angle = 0, vjust = 0.5))

p2 <- Annual_df1 %>%
  filter(Year == "2020") %>% 
  ggplot(aes(MCI, WQ_Value)) +           
  geom_point(size =2) +
  ggnewscale::new_scale_colour() +
  stat_smooth(method = sen, se=FALSE,aes(color="Blue")) +
  stat_smooth(method = "lm",
              formula = y~x,
              geom = "smooth", se=FALSE,aes(color="Red")) +
  coord_cartesian(ylim=c(0, 180)) +
  ggpubr::stat_regline_equation(label.y = 163, aes(label = ..eq.label..)) +
  ggpubr::stat_regline_equation(label.y = 150, aes(label = ..rr.label..)) +
  annotate("text", x=-150, y=180, label= "TS:y=20.31 + 0.0565x") +
  labs(x="MCI (2020)",
       y = paste("Chl\u00b9","(\U00B5g/L)"))+
  scale_color_identity(name = "Fit Function",
                       labels = c("Theil–Sen", "Least Squares"),
                       guide = "legend")+
  theme_bw(base_size = 14, base_family = "Times") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = ggtext::element_markdown(angle = 0, vjust = 0.5))

p3 <- Annual_df1 %>%
  filter(Year == "2021") %>% 
  ggplot(aes(MCI, WQ_Value)) +           
  geom_point(size =2) +
  ggnewscale::new_scale_colour() +
  stat_smooth(method = sen, se=FALSE,aes(color="Blue")) +
  stat_smooth(method = "lm",
              formula = y~x,
              geom = "smooth", se=FALSE,aes(color="Red")) +
  coord_cartesian(ylim=c(0, 180)) +
  ggpubr::stat_regline_equation(label.y = 163, aes(label = ..eq.label..)) +
  ggpubr::stat_regline_equation(label.y = 150, aes(label = ..rr.label..)) +
  annotate("text", x=-115, y=180, label= "TS:y=31.04 + 0.137x") +
  labs(x="MCI (2021)",
       y = paste("Chl\u00b9","(\U00B5g/L)"))+
  scale_color_identity(name = "Fit Function",
                       labels = c(paste("Theil–Sen"), "Least Squares"),
                       guide = "legend")+
  theme_bw(base_size = 14, base_family = "Times") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = ggtext::element_markdown(angle = 0, vjust = 0.5))

# Visualize
plots <- gridExtra::grid.arrange(p1, p2, p3, nrow = 3)

#save
plotsave <- gridExtra::arrangeGrob(p1, p2, p3, nrow=3) #generates g
ggsave("Results/Figures/MCI_1Day_Chl1_multiyear_v2.jpeg", plotsave, width = 8,height = 8, dpi = 300)

#### NDCI multiyear Plot ####
trace(ggpubr:::.stat_lm, edit = TRUE)

#NDCI Chl1 2019
Annual_df1_2021 <- Annual_df1 %>%
  filter(Year == "2021")
RobustLinearReg::theil_sen_regression(WQ_Value~NDCI, data = Annual_df1_2021)

p1 <- Annual_df1 %>%
  filter(Year == "2019") %>% 
  ggplot(aes(NDCI, WQ_Value)) +           
  geom_point(size =2) +
  ggnewscale::new_scale_colour() +
  stat_smooth(method = sen, se=FALSE,aes(color="Blue")) +
  stat_smooth(method = "lm",
              formula = y~x,
              geom = "smooth", se=FALSE,aes(color="Red")) +
  coord_cartesian(ylim=c(0, 180)) +
  ggpubr::stat_regline_equation(label.y = 160, aes(label = ..eq.label..)) +
  ggpubr::stat_regline_equation(label.y = 145, aes(label = ..rr.label..)) +
  annotate("text", x=-0.02, y=180, label= "TS:y=15.57 + 180.44x") +
  labs(x="NDCI (2019)",
       y = paste("Chl\u00b9","(\U00B5g/L)"))+
  scale_color_identity(name = "Fit Function",
                       labels = c("Theil–Sen", "Least Squares"),
                       guide = "legend")+
  theme_bw(base_size = 14, base_family = "Times") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = ggtext::element_markdown(angle = 0, vjust = 0.5))

p2 <- Annual_df1 %>%
  filter(Year == "2020") %>% 
  ggplot(aes(NDCI, WQ_Value)) +           
  geom_point(size =2) +
  ggnewscale::new_scale_colour() +
  stat_smooth(method = sen, se=FALSE,aes(color="Blue")) +
  stat_smooth(method = "lm",
              formula = y~x,
              geom = "smooth", se=FALSE,aes(color="Red")) +
  coord_cartesian(ylim=c(0, 180)) +
  ggpubr::stat_regline_equation(label.y = 160, aes(label = ..eq.label..)) +
  ggpubr::stat_regline_equation(label.y = 145, aes(label = ..rr.label..)) +
  annotate("text", x=-0.02, y=180, label= "TS:y=14.05 + 169.18x") +
  labs(x="NDCI (2020)",
       y = paste("Chl\u00b9","(\U00B5g/L)"))+
  scale_color_identity(name = "Fit Function",
                       labels = c("Theil–Sen", "Least Squares"),
                       guide = "legend")+
  theme_bw(base_size = 14, base_family = "Times") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = ggtext::element_markdown(angle = 0, vjust = 0.5))


p3 <- Annual_df1 %>%
  filter(Year == "2021") %>% 
  ggplot(aes(NDCI, WQ_Value)) +           
  geom_point(size =2) +
  ggnewscale::new_scale_colour() +
  stat_smooth(method = sen, se=FALSE,aes(color="Blue")) +
  stat_smooth(method = "lm",
              formula = y~x,
              geom = "smooth", se=FALSE,aes(color="Red")) +
  coord_cartesian(ylim=c(0, 180)) +
  ggpubr::stat_regline_equation(label.y = 160, aes(label = ..eq.label..)) +
  ggpubr::stat_regline_equation(label.y = 145, aes(label = ..rr.label..)) +
  annotate("text", x=-0.24, y=180, label= "TS:y=18.85 + 184.66x") +
  labs(x="NDCI (2021)",
       y = paste("Chl\u00b9","(\U00B5g/L)"))+
  scale_color_identity(name = "Fit Function",
                       labels = c("Theil–Sen", "Least Squares"),
                       guide = "legend")+
  theme_bw(base_size = 14, base_family = "Times") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = ggtext::element_markdown(angle = 0, vjust = 0.5))


# Visualize
plots <- gridExtra::grid.arrange(p1, p2, p3, nrow = 3)

#save
plotsave <- gridExtra::arrangeGrob(p1, p2, p3, nrow=3) #generates g
ggsave("Results/Figures/NDCI_1Day_Chl1_multiyear_v2.jpeg", plotsave, width = 8,height = 8, dpi = 300)
