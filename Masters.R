library(tidyverse)
library(ggplot2)
library(epiR)
library(aod)
library(data.table)
library(imputeTS)
library(propagate)

install.packages("vroom")
install.packages("readr")
library(vroom)
install.packages("readxl")
library(readxl)
tb_data_fixed <- read_excel("/Users/zanwynia/Desktop/Datasets/tbburden_excel.xlsx")

tbdata_new <- read_excel("/Users/zanwynia/Desktop/Datasets/tb_new2.xlsx")
#Again to make the timeline clear, I've uploaded a new dataset here with additional parameters I want
#to add so I am going through the same data cleaning steps as I did originally

#Getting rid of 2020 and 2021 observations
tbdata_2019 <- tb_data_fixed %>% 
  filter(year<=2019)

tbdata_new <- tbdata_new %>% 
  filter(year<=2019) %>% 
  mutate(HIV_inc_100k = HIV_inc*100) %>% 
  filter(income_cap > 0) %>% 
  mutate(log_income = log(income_cap)) %>% 
  mutate(log_budget = log(budget_cap)) %>% 
  mutate(log_hiv = log(HIV_inc_100k)) %>% 
  mutate(log_inc = log(e_inc_100k)) %>% 
  mutate(log_mort = log(e_mort_100k)) %>% 
  filter(e_inc_num >= 100) %>% 
  mutate(alc_tot = alc_b + alc_w + alc_sp) %>% 
  rename("mal"= Malnutrition_prev)

summary(tbdata_new$e_mort_100k)

#Splitting datasets into regional subsets 

tbdata_EMR_new <- tbdata_new %>% 
  filter(g_whoregion=="EMR")
tbdata_EUR_new <- tbdata_new %>% 
  filter(g_whoregion=="EUR")
tbdata_AFR_new <- tbdata_new %>% 
  filter(g_whoregion=="AFR")
tbdata_WPR_new <- tbdata_new %>% 
  filter(g_whoregion=="WPR")
tbdata_AMR_new <- tbdata_new %>% 
  filter(g_whoregion=="AMR")
tbdata_SEA_new <- tbdata_new %>% 
  filter(g_whoregion=="SEA")

tbdata_lowincome <- tbdata_new %>% 
  subset(income_group %in% c("Low","Lower-middle"))

tbdata_highincome <- tbdata_new %>% 
  subset(income_group %in% c("Upper-middle","High"))

summary(tbdata_lowincome$HIV_inc_100k)
  





tbdata_EMR <- tbdata_2019_new %>% 
  filter(g_whoregion=="EMR")
tbdata_EUR <- tbdata_new %>% 
  filter(g_whoregion=="EUR")
tbdata_AFR <- tbdata_2019_new %>% 
  filter(g_whoregion=="AFR")
tbdata_WPR <- tbdata_2019_new %>% 
  filter(g_whoregion=="WPR")
tbdata_AMR <- tbdata_2019_new %>% 
  filter(g_whoregion=="AMR")
tbdata_SEA <- tbdata_2019_new%>% 
  filter(g_whoregion=="SEA")




#Creating case fatality ratio variable 
tbdata_2019_new <- tbdata_2019_new %>% 
  mutate(case_fatality = e_mort_num / e_inc_num)
tbdata_AFR <- tbdata_AFR %>% 
  mutate(case_fatality = e_mort_num / e_inc_num)
tbdata_AMR <- tbdata_AMR %>% 
  mutate(case_fatality = e_mort_num / e_inc_num)
tbdata_EMR <- tbdata_EMR %>% 
  mutate(case_fatality = e_mort_num / e_inc_num)
tbdata_EUR <- tbdata_EUR %>% 
  mutate(case_fatality = e_mort_num / e_inc_num)
tbdata_SEA <- tbdata_SEA %>% 
  mutate(case_fatality = e_mort_num / e_inc_num)
tbdata_WPR <- tbdata_WPR %>% 
  mutate(case_fatality = e_mort_num / e_inc_num)

summary(tbdata_WPR$case_fatality)

#Generating trendlines for main outcomes of interest (tb incidence & mortality, CFR, and rr-tb incidence) 
unique(tbdata_2019$country)

install.packages("lcsm")
library()

tbdata_2004 <- tbdata_2019 %>% 
  filter(year==2004)
#To generate an average world TB incidence I ran this code and just changed the year== in the filter command for 2000-2019
#To generate average inc for regions I did the same thing but just changed the dataset being used and the number of countries
#I divided the inc by to get an average
avg_inc_data <- tbdata_2019 %>% 
  filter(year==2019) %>% 
  mutate(avg_inc_calc <- sum(e_inc_100k)/215)
glimpse(avg_inc_data$`avg_inc_calc <- sum(e_inc_100k)/215`)

year <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013,
          2014, 2015, 2016, 2017, 2018, 2019)

avg_inc <- c(139, 140, 144, 145, 144, 143, 143, 138, 136, 134, 132, 127, 123, 
             119, 115, 112, 109, 105, 104, 99.9)

avg_inc_frame <- data.frame(year, avg_inc)

#Did it both w/ out a linear trend line and then w/ a linear trend line by adding method = lm in geom_smooth

avg_inc_frame %>% 
  ggplot(aes(x=year, y=avg_inc))+ ggtitle("Global Average Incidence Per 100,000 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point() + xlab("Year") + ylab ("TB Inicdence per 100,000") + geom_smooth()

avg_inc_data <- tbdata_AFR %>% 
  filter(year==2019) %>% 
  mutate(avg_inc_calc <- sum(e_inc_100k)/47)
glimpse(avg_inc_data$`avg_inc_calc <- sum(e_inc_100k)/47`)

avg_inc_AFR <- c(327, 334,  341, 352, 354, 354, 351, 343, 333, 330, 322,  306, 293, 281, 
                 268, 255, 243, 234, 225, 218)

avg_inc_frame_AFR <- data.frame(year, avg_inc_AFR)


avg_inc_frame_AFR %>% 
  ggplot(aes(x=year, y=avg_inc_AFR))+ ggtitle("African Region Average Incidence Per 100,000 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point() + xlab("Year") + ylab ("TB Inicdence per 100,000") + geom_smooth()

avg_inc_data <- tbdata_AMR %>% 
  filter(year==2019) %>% 
  mutate(avg_inc_calc <- sum(e_inc_100k)/45)
glimpse(avg_inc_data$`avg_inc_calc <- sum(e_inc_100k)/45`)

avg_inc_AMR <- c(38.6, 37.3, 36.4, 36.9, 36.4, 35.6, 35.2, 34.2, 34.2, 33.2, 
                 32.2, 31.1, 32, 29.8, 27.9, 28.9, 28.8, 28.9, 28.9, 27.8)

avg_inc_frame_AMR <- data.frame(year, avg_inc_AMR)

avg_inc_frame_AMR$log_inc=log(avg_inc_frame_AMR$avg_inc_AMR)

#Checking to see if a log transformation of incidence makes the realtionship more linear, it does not

avg_inc_frame_AMR %>% 
  ggplot(aes(x=year, y=avg_inc_AMR))+ ggtitle("American Region Average Incidence Per 100,000 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point() + xlab("Year") + ylab ("TB Inicdence per 100,000") + geom_smooth()

avg_inc_data <- tbdata_EMR %>% 
  filter(year==2019) %>% 
  mutate(avg_inc_calc <- sum(e_inc_100k)/22)
glimpse(avg_inc_data$`avg_inc_calc <- sum(e_inc_100k)/22`)


avg_inc_EMR <- c(100, 99.6, 90.6, 89.6, 86.1, 86.7, 85.1, 85.4, 87.7, 87.9, 
                 89.2, 84, 82.3, 78.6, 72.1, 73, 70.9, 67, 65.7, 65.1)

avg_inc_frame_EMR <- data.frame(year, avg_inc_EMR)

avg_inc_frame_EMR %>% 
  ggplot(aes(x=year, y=avg_inc_EMR))+ ggtitle("Eastern Mediterranean Region Average Incidence Per 100,000 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5, size=10)) +
  geom_point() + xlab("Year") + ylab ("TB Inicdence per 100,000") + geom_smooth()

avg_inc_data <- tbdata_EUR %>% 
  filter(year==2019) %>% 
  mutate(avg_inc_calc <- sum(e_inc_100k)/54)
glimpse(avg_inc_data$`avg_inc_calc <- sum(e_inc_100k)/54`)

avg_inc_EUR <- c(51.8, 52, 50.9, 51.4, 49.4, 48.5, 46.6, 45.3, 44.1,  41.7,
                 40.9, 39.5, 37, 36.5, 33.5, 31.6, 29.3, 27.5, 25.9,  25.4)

avg_inc_frame_EUR <- data.frame(year, avg_inc_EUR)

avg_inc_frame_EUR %>% 
  ggplot(aes(x=year, y=avg_inc_EUR))+ ggtitle("European Region Average Incidence Per 100,000 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5, size=10)) +
  geom_point() + xlab("Year") + ylab ("TB Inicdence per 100,000") + geom_smooth()

avg_inc_data <- tbdata_SEA %>% 
  filter(year==2019) %>% 
  mutate(avg_inc_calc <- sum(e_inc_100k)/11)
glimpse(avg_inc_data$`avg_inc_calc <- sum(e_inc_100k)/11`)

avg_inc_SEA <- c(303, 299, 315, 312, 308, 305, 299, 298, 294, 292, 290, 
                 284, 278, 273, 269, 264, 263, 254, 250, 248)

avg_inc_frame_SEA <- data.frame(year, avg_inc_SEA)

avg_inc_frame_SEA %>% 
  ggplot(aes(x=year, y=avg_inc_SEA))+ ggtitle("South East Asia Region Average Incidence Per 100,000 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5, size=10)) +
  geom_point() + xlab("Year") + ylab ("TB Inicdence per 100,000") + geom_smooth()

avg_inc_data <- tbdata_WPR %>% 
  filter(year==2019) %>% 
  mutate(avg_inc_calc <- sum(e_inc_100k)/36)
glimpse(avg_inc_data$`avg_inc_calc <- sum(e_inc_100k)/36`)

avg_inc_WPR <- c(131, 127, 134, 131, 130, 129, 130, 127, 126, 126, 126, 
                 123, 121, 123, 122, 129, 128, 124, 136, 124)

avg_inc_frame_WPR <- data.frame(year, avg_inc_WPR)

avg_inc_frame_WPR %>% 
  ggplot(aes(x=year, y=avg_inc_WPR))+ ggtitle("Western Pacific Region Average Incidence Per 100,000 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5, size=10)) +
  geom_point() + xlab("Year") + ylab ("TB Inicdence per 100,000") + geom_smooth()

#Doing the same thing for average mortality, first getting rid of missing values 


avg_mort_data <- tbdata_2019 %>%
  drop_na(e_mort_100k) %>% 
  filter(year==2019) %>% 
  mutate(avg_mort_calc <- sum(e_mort_100k)/214)
glimpse(avg_mort_data$`avg_mort_calc <- sum(e_mort_100k)/214`)

avg_mort <- c(35.5, 35.4, 34.5, 34.1, 33.3, 33.4, 32.6, 31.9, 30.6, 28.9, 
              26.3, 25.6, 24.8,  23.5, 22.1, 21.1, 19.7, 18.6, 17.6, 16.4)

avg_mort_frame <- data.frame(year, avg_mort)

avg_mort_frame %>% 
  ggplot(aes(x=year, y=avg_mort))+ ggtitle("Average Global TB Mortality Per 100,000 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5, size=10)) +
  geom_point() + xlab("Year") + ylab ("TB Mortality per 100,000") + geom_smooth()

avg_mort_data <- tbdata_AFR %>%
  drop_na(e_mort_100k) %>% 
  filter(year==2019) %>% 
  mutate(avg_mort_calc <- sum(e_mort_100k)/47)
glimpse(avg_mort_data$`avg_mort_calc <- sum(e_mort_100k)/47`)

avg_mort_AFR <- c(113, 114, 111, 111, 110, 112, 110, 108, 103, 96.6, 
                  85, 81.9, 80.4, 75.7, 71.3, 67, 62, 58.3, 53.9, 50.2)

avg_mort_frame_AFR <- data.frame(year, avg_mort_AFR)

avg_mort_frame_AFR %>% 
  ggplot(aes(x=year, y=avg_mort_AFR))+ ggtitle("Average African Region TB Mortality Per 100,000 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5, size=10)) +
  geom_point() + xlab("Year") + ylab ("TB Mortality per 100,000") + geom_smooth()

avg_mort_data <- tbdata_AMR %>%
  drop_na(e_mort_100k) %>% 
  filter(year==2019) %>% 
  mutate(avg_mort_calc <- sum(e_mort_100k)/45)
glimpse(avg_mort_data$`avg_mort_calc <- sum(e_mort_100k)/45`)

avg_mort_AMR <- c(6.14, 5.79, 5.44, 5.38, 5.27, 4.97, 4.89, 4.69, 4.5, 4.19, 4.22, 4.01,
                  3.88, 3.74, 3.45, 3.44, 3.42, 3.31, 3.22, 3.14)

avg_mort_frame_AMR <- data.frame(year, avg_mort_AMR)

avg_mort_frame_AMR %>% 
  ggplot(aes(x=year, y=avg_mort_AMR))+ ggtitle("Avearge American Region TB Mortality Per 100,000 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5, size=10)) +
  geom_point() + xlab("Year") + ylab ("TB Mortality per 100,000") + geom_smooth()


avg_mort_data <- tbdata_EMR %>%
  drop_na(e_mort_100k) %>% 
  filter(year==2019) %>% 
  mutate(avg_mort_calc <- sum(e_mort_100k)/22)
glimpse(avg_mort_data$`avg_mort_calc <- sum(e_mort_100k)/22`)

avg_mort_EMR <- c(18.4, 17.7, 16, 15.6, 14.5, 14, 13.8, 13.4, 13.2, 13.5, 14.3, 
                  13.5, 13, 12.4, 11.2, 11.1, 10.2, 9.11, 8.8, 8.58)

avg_mort_frame_EMR <- data.frame(year, avg_mort_EMR)

avg_mort_frame_EMR %>% 
  ggplot(aes(x=year, y=avg_mort_EMR))+ ggtitle("Average Eastern Mediterranean Region TB Mortality Per 100,000 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5, size=10)) +
  geom_point() + xlab("Year") + ylab ("TB Mortality per 100,000") + geom_smooth()

avg_mort_data <- tbdata_EUR %>%
  drop_na(e_mort_100k) %>% 
  filter(year==2019) %>% 
  mutate(avg_mort_calc <- sum(e_mort_100k)/54)
glimpse(avg_mort_data$`avg_mort_calc <- sum(e_mort_100k)/54`)


avg_mort_EUR <- c(6.59, 6.48, 6.17, 6.1, 5.69, 5.53, 5.25, 4.98, 4.7, 4.38, 
                  4.32, 3.99, 3.66, 3.38, 3.22, 3, 2.73, 2.5, 2.37, 2.31)

avg_mort_frame_EUR <- data.frame(year, avg_mort_EUR)

avg_mort_frame_EUR %>% 
  ggplot(aes(x=year, y=avg_mort_EUR))+ ggtitle("Average European Region TB Mortality Per 100,000 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5, size=10)) +
  geom_point() + xlab("Year") + ylab ("TB Mortality per 100,000") + geom_smooth()

avg_mort_data <- tbdata_SEA %>%
  drop_na(e_mort_100k) %>% 
  filter(year==2019) %>% 
  mutate(avg_mort_calc <- sum(e_mort_100k)/10)
glimpse(avg_mort_data$`avg_mort_calc <- sum(e_mort_100k)/10`)

avg_mort_SEA <- c(69, 66.4, 67, 61.7, 56.8, 54.1, 52.1,  52.9, 51.8, 45, 44.6, 44,
                  43.5, 42.6, 40.9, 40, 38, 37.2, 34.1, 32.3)

avg_mort_frame_SEA <- data.frame(year, avg_mort_SEA)


avg_mort_frame_SEA %>% 
  ggplot(aes(x=year, y=avg_mort_SEA))+ ggtitle("Average South East Asia Region TB Mortality Per 100,000 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5, size=10)) +
  geom_point() + xlab("Year") + ylab ("TB Mortality per 100,000") + geom_smooth()

avg_mort_data <- tbdata_WPR %>%
  drop_na(e_mort_100k) %>% 
  filter(year==2019) %>% 
  mutate(avg_mort_calc <- sum(e_mort_100k)/36)
glimpse(avg_mort_data$`avg_mort_calc <- sum(e_mort_100k)/36`)


avg_mort_WPR <- c(16.8, 16, 17.1, 15.9, 15.7, 15.5, 14.9, 14.2, 14.1, 14.2, 13.9, 
                  13, 11.9, 11.6, 10.8, 11, 10.8, 10.9, 12, 10)

avg_mort_frame_WPR <- data.frame(year, avg_mort_WPR)

avg_mort_frame_WPR %>% 
  ggplot(aes(x=year, y=avg_mort_WPR))+ ggtitle("Average Western Pacific Region TB Mortality Per 100,000 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5, size=10)) +
  geom_point() + xlab("Year") + ylab ("TB Mortality per 100,000") + geom_smooth()


#Same for CFR, but using median instead because when you try to average it equals ~neg infinity 

med_cfr <- tbdata_2019 %>% 
  drop_na(case_fatality) %>% 
  filter(year==2019)

summary(med_cfr$case_fatality)


med_cfr_globe <- c(0.129, 0.130, 0.125, 0.124, 0.126, 0.126, 0.122, 0.120, 0.117, 
                   0.113, 0.110, 0.111, 0.107, 0.107, 0.112, 0.108, 0.106, 0.108, 0.106, 
                   0.105)

cfr_globe_frame <- data.frame(year, med_cfr_globe)

cfr_globe_frame %>% 
  ggplot(aes(x=year, y=med_cfr_globe))+ ggtitle("Global Median TB CFR 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5, size=10)) +
  geom_point() + xlab("Year") + ylab ("TB Case Fatality Ratio") + geom_smooth()

med_cfr_AFR <- tbdata_AFR %>% 
  drop_na(case_fatality) %>% 
  filter(year==2019)

summary(med_cfr_AFR$case_fatality)

med_cfr_AFR <- c(0.329, 0.317, 0.307, 0.300, 0.284, 0.288, 0.282, 0.283, 0.274, 0.253, 
                 0.242, 0.239, 0.237, 0.236, 0.229, 0.231, 0.224, 0.226, 0.207, 0.203)

cfr_AFR_frame <- data.frame(year, med_cfr_AFR)

cfr_AFR_frame %>% 
  ggplot(aes(x=year, y=med_cfr_AFR))+ ggtitle("African Region Median TB CFR 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5, size=10)) +
  geom_point() + xlab("Year") + ylab ("TB Case Fatality Ratio") + geom_smooth()

med_cfr_AMR <- tbdata_AMR %>% 
  drop_na(case_fatality) %>% 
  filter(year==2018)

summary(med_cfr_AMR$case_fatality)

med_cfr_AMR <- c(0.133, 0.140, 0.125, 0.131, 0.128, 0.128, 0.130, 0.125, 0.126, 0.123,
                 0.119, 0.115, 0.110, 0.111, 0.116, 0.113, 0.109, 0.101, 0.099, 0.098)

cfr_AMR_frame <- data.frame(year, med_cfr_AMR)

cfr_AMR_frame %>% 
  ggplot(aes(x=year, y=med_cfr_AMR))+ ggtitle("American Region Median TB CFR 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5, size=10)) +
  geom_point() + xlab("Year") + ylab ("TB Case Fatality Ratio") + geom_smooth()

med_cfr_EMR <- tbdata_EMR %>% 
  drop_na(case_fatality) %>% 
  filter(year==2019)

summary(med_cfr_EMR$case_fatality)

med_cfr_EMR <- c(0.100, 0.092, 0.090, 0.088, 0.093, 0.083, 0.085, 0.084,
                 0.085, 0.084, 0.082, 0.083, 0.080, 0.081, 0.084, 0.082, 0.083, 
                 0.080, 0.079, 0.080)

cfr_EMR_frame <- data.frame(year, med_cfr_EMR)

cfr_EMR_frame %>% 
  ggplot(aes(x=year, y=med_cfr_EMR))+ ggtitle("Eastern Mediterranean Region Median TB CFR 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5, size=10)) +
  geom_point() + xlab("Year") + ylab ("TB Case Fatality Ratio") + geom_smooth()

med_cfr_EUR <- tbdata_EUR %>% 
  drop_na(case_fatality) %>% 
  filter(year==2019)

summary(med_cfr_EUR$case_fatality)

med_cfr_EUR <- c(0.101, 0.103, 0.106, 0.096, 0.096, 0.091, 0.087, 0.095, 0.086, 0.084, 
                 0.085, 0.080, 0.081, 0.069, 0.077, 0.077, 0.073, 0.071, 0.078, 0.082)

cfr_EUR_frame <- data.frame(year, med_cfr_EUR)


cfr_EUR_frame %>% 
  ggplot(aes(x=year, y=med_cfr_EUR))+ ggtitle("European Region Median TB CFR 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5, size=10)) +
  geom_point() + xlab("Year") + ylab ("TB Case Fatality Ratio") + geom_smooth()

med_cfr_SEA <- tbdata_SEA %>% 
  drop_na(case_fatality) %>% 
  filter(year==2019)

summary(med_cfr_SEA$case_fatality)

med_cfr_SEA <- c(0.198, 0.191, 0.188, 0.170, 0.156, 0.155, 0.152, 0.160, 0.156, 0.144,
                 0.136, 0.137, 0.143, 0.147, 0.140, 0.140, 0.131, 0.132, 0.123, 0.113)

cfr_SEA_frame <- data.frame(year, med_cfr_SEA)

cfr_SEA_frame %>% 
  ggplot(aes(x=year, y=med_cfr_SEA))+ ggtitle("South East Asia Region Median TB CFR 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5, size=10)) +
  geom_point() + xlab("Year") + ylab ("TB Case Fatality Ratio") + geom_smooth()


med_cfr_WPR <- tbdata_WPR %>% 
  drop_na(case_fatality) %>% 
  filter(year==2019)

summary(med_cfr_WPR$case_fatality)

med_cfr_WPR <- c(0.084, 0.085, 0.083, 0.097, 0.085, 0.088, 0.086, 0.083, 0.086, 
                 0.089, 0.081, 0.079, 0.081, 0.083, 0.077, 0.079, 0.074, 0.081,
                 0.079, 0.079)
cfr_WPR_frame <- data.frame(year, med_cfr_WPR)

cfr_WPR_frame %>% 
  ggplot(aes(x=year, y=med_cfr_WPR))+ ggtitle("Western Pacific Region Median TB CFR 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5, size=10)) +
  geom_point() + xlab("Year") + ylab ("TB Case Fatality Ratio") + geom_smooth()


#Finally plotting trendline of rr-TB 

avg_rr_data <- tbdata_2019 %>% 
  drop_na(e_rr_inc_100k) %>% 
  filter(year==2019) %>% 
  mutate(avg_rr_inc_calc <- sum(e_rr_inc_100k)/114)
glimpse(avg_rr_data$`avg_rr_inc_calc <- sum(e_rr_inc_100k)/114`)

avg_rr <- c(12.7, 12.1, 11.6, 11.2, 11)

rr_year <- c(2015, 2016, 2017, 2018, 2019)

avg_rr_frame <- data.frame(rr_year, avg_rr)
#Not doing a loess line here because the sample of rr-inc isn't big enough 
avg_rr_frame %>% 
  ggplot(aes(x=rr_year, y=avg_rr))+ ggtitle("Global Average RR-TB Incidence Per 100,000 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point() + xlab("Year") + ylab ("RR-TB Inicdence per 100,000") + geom_smooth(method = lm)

rr_AFR <- tbdata_AFR %>% 
  drop_na(e_rr_inc_100k) %>% 
  filter(year==2019) %>% 
  mutate(avg_rr_inc_calc <- sum(e_rr_inc_100k)/42)
glimpse(rr_AFR$`avg_rr_inc_calc <- sum(e_rr_inc_100k)/42`)


avg_rr_AFR <- c(13.9, 13, 12.3, 11.7, 11.3)

avg_rr_frame_AFR <- data.frame(rr_year, avg_rr_AFR)

avg_rr_frame_AFR %>% 
  ggplot(aes(x=rr_year, y=avg_rr_AFR))+ ggtitle("Linearized African Region RR-TB Incidence Per 100,000 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point() + xlab("Year") + ylab ("RR-TB Inicdence per 100,000") + geom_smooth(method = lm)

rr_AMR <- tbdata_AMR %>% 
  drop_na(e_rr_inc_100k) %>% 
  filter(year==2019) %>% 
  mutate(avg_rr_inc_calc <- sum(e_rr_inc_100k)/17)
glimpse(rr_AMR$`avg_rr_inc_calc <- sum(e_rr_inc_100k)/17`)

avg_rr_AMR <- c(2.68, 2.57, 2.51, 2.41, 2.26)

avg_rr_frame_AMR <- data.frame(rr_year, avg_rr_AMR)

avg_rr_frame_AMR %>% 
  ggplot(aes(x=rr_year, y=avg_rr_AMR))+ ggtitle("Linearized American Region RR-TB Incidence Per 100,000 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point() + xlab("Year") + ylab ("RR-TB Inicdence per 100,000") + geom_smooth(method = lm)

rr_EMR <- tbdata_EMR %>% 
  drop_na(e_rr_inc_100k) %>% 
  filter(year==2019) %>% 
  mutate(avg_rr_inc_calc <- sum(e_rr_inc_100k)/11)
glimpse(rr_EMR$`avg_rr_inc_calc <- sum(e_rr_inc_100k)/11`)

avg_rr_EMR <- c(10.1, 9.83, 9.53, 9.69, 9.85)

avg_rr_frame_EMR <- data.frame(rr_year, avg_rr_EMR)

avg_rr_frame_EMR %>% 
  ggplot(aes(x=rr_year, y=avg_rr_EMR))+ ggtitle("Linearized Eastern Mediterranean Region RR-TB Incidence Per 100,000 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point() + xlab("Year") + ylab ("RR-TB Inicdence per 100,000") + geom_smooth(method = lm)

rr_EUR <- tbdata_EUR %>% 
  drop_na(e_rr_inc_100k) %>% 
  filter(year==2019) %>% 
  mutate(avg_rr_inc_calc <- sum(e_rr_inc_100k)/22)
glimpse(rr_EUR$`avg_rr_inc_calc <- sum(e_rr_inc_100k)/22`)

avg_rr_EUR <- c(21.4, 20.6, 19.7, 18.9, 18.4)

avg_rr_frame_EUR <- data.frame(rr_year, avg_rr_EUR)

avg_rr_frame_EUR %>% 
  ggplot(aes(x=rr_year, y=avg_rr_EUR))+ ggtitle("Linearized European Region RR-TB Incidence Per 100,000 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point() + xlab("Year") + ylab ("RR-TB Inicdence per 100,000") + geom_smooth(method = lm)

rr_SEA <- tbdata_SEA %>% 
  drop_na(e_rr_inc_100k) %>% 
  filter(year==2019) %>% 
  mutate(avg_rr_inc_calc <- sum(e_rr_inc_100k)/9)
glimpse(rr_SEA$`avg_rr_inc_calc <- sum(e_rr_inc_100k)/9`)

avg_rr_SEA <- c(13.4, 13.6, 12.3, 12.2, 12.3)

avg_rr_frame_SEA <- data.frame(rr_year, avg_rr_SEA)

avg_rr_frame_SEA %>% 
  ggplot(aes(x=rr_year, y=avg_rr_SEA))+ ggtitle("Linearized South East Asia Region RR-TB Incidence Per 100,000 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point() + xlab("Year") + ylab ("RR-TB Inicdence per 100,000") + geom_smooth(method = lm)


rr_WPR <- tbdata_WPR %>% 
  drop_na(e_rr_inc_100k) %>% 
  filter(year==2019) %>% 
  mutate(avg_rr_inc_calc <- sum(e_rr_inc_100k)/13)
glimpse(rr_WPR$`avg_rr_inc_calc <- sum(e_rr_inc_100k)/13`)

avg_rr_WPR <- c(8.54, 8.55, 8.56, 8.55, 8.54)

avg_rr_frame_WPR <- data.frame(rr_year, avg_rr_WPR)

avg_rr_frame_WPR %>% 
  ggplot(aes(x=rr_year, y=avg_rr_WPR))+ ggtitle("Linearized Western Pacific Region RR-TB Incidence Per 100,000 2000-2019") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point() + xlab("Year") + ylab ("RR-TB Inicdence per 100,000") + geom_smooth(method = lm)

install.packages("lme4")
library(lme4)

#Checking distributions of response variables and making transformations for normality before checking pearson correlations 
summary(tbdata_2019_new$e_inc_100k)
sd(tbdata_2019_new$e_inc_100k)
qqnorm(tbdata_2019_new$e_inc_100k)
qqline(tbdata_2019_new$e_inc_100k)

tbdata_new <- tb_data_fixed %>% 
  filter(e_inc_num >= 100) %>% 
  mutate(log_inc <- log(e_inc_100k))

qqnorm(tbdata_new$e_inc_100k)
qqline(tbdata_new$e_inc_100k)



summary(tbdata_2019_new$`log_inc <- log(e_inc_100k)`)
sd(tbdata_2019_new$`log_inc <- log(e_inc_100k)`)
qqnorm(tbdata_2019_new$`log_inc <- log(e_inc_100k)`)
qqline(tbdata_2019_new$`log_inc <- log(e_inc_100k)`)

tbdata_new %>% 
  ggplot(aes(x= e_inc_100k))+ ggtitle("Global TB Inicdence Per 100,000")+
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  geom_histogram(bins = 50, fill="darkblue", color="black") +
  xlab("TB Incidence Per 100000")

tbdata_new %>% 
  ggplot(aes(x= log_inc))+ ggtitle("Log Transformed Global TB Inicdence Per 100,000")+
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  geom_histogram(bins = 50, fill="darkblue", color="black") +
  xlab("Log Transformed TB Incidence Per 100000")

tbdata_new %>% 
  ggplot(aes(x= e_mort_100k))+ ggtitle("Global TB Mortality Per 100,000")+
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  geom_histogram(bins = 50, fill="salmon", color="black") +
  xlab("TB Mortality Per 100000")

tbdata_new %>% 
  ggplot(aes(x= log_mort))+ ggtitle("Log Transformed Global TB Mortality Per 100,000")+
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  geom_histogram(bins = 50, fill="salmon", color="black") +
  xlab("Log Transformed TB Mortality Per 100000")

tbdata_2019_new %>% 
  ggplot(aes(x=`log_inc <- log(e_inc_100k)`))+ ggtitle("Log Transformed Global TB Inicdence Per 100,000")+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_histogram(bins = 30, fill="darkblue", color="black") +
  xlab("Log Transformed TB Incidence Per 100000")

tbdata_2019_new %>% 
  ggplot(aes(x=e_inc_100k))+ ggtitle("Histogram of Global TB Incidence Per 100,000")+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_histogram(bins = 30, fill="darkblue", color="black") +
  xlab("TB Incidence Per 100000")

summary(tbdata_2019_new$e_mort_100k)
tbdata_2019_mort <- tbdata_2019_new %>% drop_na(e_mort_100k)

summary(tbdata_2019_mort$e_mort_100k)
sd(tbdata_2019_mort$e_mort_100k)
qqnorm(tbdata_2019_mort$e_mort_100k)
qqline(tbdata_2019_mort$e_mort_100k)

tbdata_2019_new <- tbdata_2019_mort %>% 
  mutate(mort_log <- log(e_mort_100k))

summary(tbdata_2019_new$`mort_log <- log(e_mort_100k)`)
sd(tbdata_2019_new$`mort_log <- log(e_mort_100k)`)
qqnorm(tbdata_2019_new$`mort_log <- log(e_mort_100k)`)
qqline(tbdata_2019_new$`mort_log <- log(e_mort_100k)`)

tbdata_2019_new %>% 
  ggplot(aes(x=`mort_log <- log(e_mort_100k)`))+ ggtitle("Log Transformed Global TB Mortality Per 100,000")+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_histogram(bins = 30, fill="darkblue", color="black") +
  xlab("Log Transformed TB Mortality Per 100000")

tbdata_2019_new %>% 
  ggplot(aes(x=e_mort_100k))+ ggtitle("Histogram of Global TB Mortality Per 100,000")+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_histogram(bins = 30, fill="darkblue", color="black") +
  xlab("TB Mortality Per 100000")

summary(tbdata_2019_new$case_fatality)
sd(tbdata_2019_new$case_fatality)
qqnorm(tbdata_2019_new$case_fatality)
qqline(tbdata_2019_new$case_fatality)

tbdata_2019_new %>% 
  ggplot(aes(x=case_fatality))+ ggtitle("Global Case Fatality Ratio")+
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_histogram(bins = 30, fill="darkblue", color="black") +
  xlab("Case Fatality Ratio")


tbdata_2019_new <- tbdata_2019_new %>% 
  mutate(cfr_log <- log(case_fatality))

summary(tbdata_2019_new$`cfr_log <- log(case_fatality)`)
sd(tbdata_2019_new$`cfr_log <- log(case_fatality)`)
qqnorm(tbdata_2019_new$`cfr_log <- log(case_fatality)`)
qqline(tbdata_2019_new$`cfr_log <- log(case_fatality)`)

tbdata_2019_new %>% 
  ggplot(aes(x=`cfr_log <- log(case_fatality)`))+ ggtitle("Global Case Fatality Ratio")+
  geom_histogram(bins = 10, fill="darkblue", color="black") +
  xlab("Case Fatality Ratio")

tbdata_2019_rr <- tbdata_2019_new %>% 
  drop_na(e_rr_inc_100k)

summary(tbdata_2019_rr$e_rr_inc_100k)
sd(tbdata_2019_rr$e_rr_inc_100k)
qqnorm(tbdata_2019_rr$e_rr_inc_100k)
qqline(tbdata_2019_rr$e_rr_inc_100k)

tbdata_2019_rr <- tbdata_2019_rr %>% 
  mutate(rr_log <- log(e_rr_inc_100k))

summary(tbdata_2019_rr$`rr_log <- log(e_rr_inc_100k)`)
sd(tbdata_2019_rr$`rr_log <- log(e_rr_inc_100k)`)
qqnorm(tbdata_2019_rr$`rr_log <- log(e_rr_inc_100k)`)
qqline(tbdata_2019_rr$`rr_log <- log(e_rr_inc_100k)`)

tbdata_2019_rr %>% 
  ggplot(aes(`rr_log <- log(e_rr_inc_100k)`))+ ggtitle("Log Transformed Global RR-TB Incidence Per 100000")+
  geom_histogram(bins = 10, fill="darkblue", color="black") +
  xlab("Log Rifampicin Resistant TB Incidence")

tbdata_2019_rr %>% 
  ggplot(aes(x=e_rr_inc_100k))+ ggtitle("Global RR-TB Incidence Per 100000")+
  geom_histogram(bins = 10, fill="darkblue", color="black") +
  xlab("Rifampicin Resistant TB Incidence")

summary(tbdata_2019_new$DM_prev)
sd(tbdata_2019_new$DM_prev)
qqnorm(tbdata_2019_new$DM_prev)
qqline(tbdata_2019_new$DM_prev)

tbdata_2019_new %>% 
  ggplot(aes(x=DM_prev))+ ggtitle("Diabetes Prevalence Histogram")+
  geom_histogram(bins = 10, fill="darkblue", color="black") +
  xlab("Diabetes Prevalence")

globe_DM <- tbdata_2019_new %>% 
  drop_na(DM_prev)

sd(globe_DM$DM_prev)
sd(globe_DM$`DM_log <- log(DM_prev)`)

tbdata_2019_new <- tbdata_2019_new %>% 
  mutate(DM_log <- log(DM_prev))

summary(tbdata_2019_new$`DM_log <- log(DM_prev)`)
qqnorm(tbdata_2019_new$`DM_log <- log(DM_prev)`)
qqline(tbdata_2019_new$`DM_log <- log(DM_prev)`)

tbdata_2019_new %>% 
  ggplot(aes(`DM_log <- log(DM_prev)`))+ ggtitle("Log Transformed Diabetes Prevalence Histogram")+
  geom_histogram(bins = 10, fill="darkblue", color="black") +
  xlab("Log Transformed Diabetes Prevalence")

#Making HIV incidence per 100000 pop instead of per 1000 so it's the same scale as TB incidence

tbdata_2019_new <- tbdata_2019_new %>% 
  mutate(HIV_inc = HIV_inc*10000)
#I messed up the calculations a few times which is why it is multipled by 10000 here instead of by 100 as it should've been originally
globe_HIV <- tbdata_2019_new %>% 
  drop_na(HIV_inc)

sd(globe_HIV$HIV_inc)

summary(tbdata_2019_new$HIV_inc)
qqnorm(tbdata_2019_new$HIV_inc)
qqline(tbdata_2019_new$HIV_inc)

tbdata_2019_new <- tbdata_2019_new %>% 
  mutate(log_HIV_globe = log(HIV_inc))

summary(tbdata_2019_new$log_HIV_globe)
qqnorm(tbdata_2019_new$log_HIV_globe)
qqline(tbdata_2019_new$log_HIV_globe)
sd(globe_HIV$log_HIV_globe)

tbdata_2019_new %>% 
  ggplot(aes(x=log_HIV_globe))+ ggtitle("Log Transformed HIV Incidence Histogram")+
  geom_histogram(bins = 10, fill="salmon", color="black") +
  xlab("Log Transformed HIV Incidence")

tbdata_2019_new %>% 
  ggplot(aes(x=HIV_inc))+ ggtitle("Global HIV Incidence Per 100000 Histogram")+
  geom_histogram(bins = 25, fill="salmon", color="black") +
  xlab("HIV Incidence Per 100000")

globe_income <- tbdata_2019_new %>% 
  drop_na(income_cap)

sd(globe_income$income_cap)

summary(tbdata_2019_new$income_cap)
qqnorm(tbdata_2019_new$income_cap)
qqline(tbdata_2019_new$income_cap)

tbdata_2019_new <- tbdata_2019_new %>% 
  filter(income_cap > 0) %>% 
  mutate(log_income <- log(income_cap))

summary(tbdata_2019_new$`log_income <- log(income_cap)`)
sd(tbdata_2019_new$`log_income <- log(income_cap)`)
qqnorm(tbdata_2019_new$`log_income <- log(income_cap)`)
qqline(tbdata_2019_new$`log_income <- log(income_cap)`)

tbdata_2019_new %>% 
  ggplot(aes(`log_income <- log(income_cap)`))+ ggtitle("Log Transformed Adjusted Income Per Capita")+
  geom_histogram(bins = 10, fill="darkblue", color="black") +
  xlab("Log Transformed Adjusted Income Per Capita")

tbdata_2019_new %>% 
  ggplot(aes(x=income_cap))+ ggtitle("Histogram of Adjusted Income Per Capita")+
  geom_histogram(bins = 10, fill="darkblue", color="black") +
  xlab("Adjusted Income Per Capita")

globe_malnutrition <- tbdata_2019_new %>% 
  drop_na(`Malnutrition_prev `)

sd(globe_malnutrition$`Malnutrition_prev `)
sd(globe_malnutrition$`mal_log <- log(\`Malnutrition_prev \`)`)

summary(tbdata_2019_new$`Malnutrition_prev `)
qqnorm(tbdata_2019_new$`Malnutrition_prev `)
qqline(tbdata_2019_new$`Malnutrition_prev `)

tbdata_2019_new <- tbdata_2019_new %>% 
  mutate(mal_log <- log(`Malnutrition_prev `))

summary(tbdata_2019_new$`mal_log <- log(\`Malnutrition_prev \`)`)
qqnorm(tbdata_2019_new$`mal_log <- log(\`Malnutrition_prev \`)`)
qqline(tbdata_2019_new$`mal_log <- log(\`Malnutrition_prev \`)`)

tbdata_2019_new %>% 
  ggplot(aes(x=`mal_log <- log(\`Malnutrition_prev \`)`))+ ggtitle("Log Transformed Global Malnutrition Prevalence")+
  geom_histogram(bins = 10, fill="darkblue", color="black") +
  xlab("Log Transformed Malnutrition Prevalence")

tbdata_2019_new %>% 
  ggplot(aes(x=`Malnutrition_prev `))+ ggtitle("Global Malnutrition Prevalence")+
  geom_histogram(bins = 10, fill="darkblue", color="black") +
  xlab("Malnutrition Prevalence")


summary(tbdata_2019_new$Budget)
qqnorm(tbdata_2019_new$Budget)
qqline(tbdata_2019_new$Budget)

globe_budget <- tbdata_2019_new %>% 
  drop_na(Budget)

sd(globe_budget$Budget)
sd(globe_budget$`log_budget <- log(Budget)`)

tbdata_2019_new <- tbdata_2019_new %>% 
  mutate(log_budget <- log(Budget))

summary(tbdata_2019_new$`log_budget <- log(Budget)`)  
qqnorm(tbdata_2019_new$`log_budget <- log(Budget)`) 
qqline(tbdata_2019_new$`log_budget <- log(Budget)`)


tbdata_2019_new %>% 
  ggplot(aes(x=`log_budget <- log(Budget)`))+ ggtitle("Log Transformed TB Budget")+
  geom_histogram(bins = 10, fill="darkblue", color="black") +
  xlab("Log Transformed TB Budget")

tbdata_2019_new %>% 
  ggplot(aes(x=Budget))+ ggtitle("Global TB Budget")+
  geom_histogram(bins = 10, fill="darkblue", color="black") +
  xlab("TB Activity Budget")


#Doing the same thing but regionally now 
summary(tbdata_AFR$e_inc_100k)
sd(tbdata_AFR$e_inc_100k)
qqnorm(tbdata_AFR$e_inc_100k)
qqline(tbdata_AFR$e_inc_100k)

tbdata_AFR %>% 
  ggplot(aes(x=e_inc_100k)) + ggtitle("Histogram of TB Incidence Per 100000 Population African Region") +
  geom_histogram(bins=30, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Incidence Per 100000")

tbdata_AFR <- tbdata_AFR %>% 
  filter(e_inc_num >= 100) %>% 
  mutate(log_inc_AFR <- log(e_inc_100k))

summary(tbdata_AFR$`log_inc_AFR <- log(e_inc_100k)`)
sd(tbdata_AFR$`log_inc_AFR <- log(e_inc_100k)`) 
qqnorm(tbdata_AFR$`log_inc_AFR <- log(e_inc_100k)`) 
qqline(tbdata_AFR$`log_inc_AFR <- log(e_inc_100k)`) 

tbdata_AFR %>% 
  ggplot(aes(x=`log_inc_AFR <- log(e_inc_100k)`)) + ggtitle("Log Adjusted Histogram of TB Incidence Per 100000 Population African Region") +
  geom_histogram(bins=30, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted TB Incidence Per 100000")

AFR_mort <- tbdata_AFR %>% 
  drop_na(e_mort_100k)

sd(AFR_mort$e_mort_100k)
sd(AFR_mort$`log_mort_AFR <- log(e_mort_100k)`)
summary(AFR_mort$e_mort_100k)

summary(tbdata_AFR$e_mort_100k)
qqnorm(tbdata_AFR$e_mort_100k)
qqline(tbdata_AFR$e_mort_100k)

tbdata_AFR <- tbdata_AFR %>% 
  mutate(log_mort_AFR <- log(e_mort_100k))

summary(tbdata_AFR$`log_mort_AFR <- log(e_mort_100k)`)
qqnorm(tbdata_AFR$`log_mort_AFR <- log(e_mort_100k)`)
qqline(tbdata_AFR$`log_mort_AFR <- log(e_mort_100k)`)


tbdata_AFR %>% 
  ggplot(aes(x=e_mort_100k)) + ggtitle("Histogram of TB Mortality Per 100000 Population African Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Mortality Per 100000")

tbdata_AFR %>% 
  ggplot(aes(x=`log_mort_AFR <- log(e_mort_100k)`))+ ggtitle("Log Adjusted Histogram of TB Mortality Per 100000 Population African Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted TB Mortality Per 100000")

summary(tbdata_AFR$case_fatality)
sd(tbdata_AFR$case_fatality)
qqnorm(tbdata_AFR$case_fatality)
qqline(tbdata_AFR$case_fatality)

tbdata_AFR %>% 
  ggplot(aes(x=case_fatality))+ ggtitle("TB Case Fatality Ratio Africa Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Case Fatality Ratio")

tbdata_rr_AFR <- tbdata_AFR %>% 
  drop_na(e_rr_inc_100k)

summary(tbdata_rr_AFR$e_rr_inc_100k)
sd(tbdata_rr_AFR$e_rr_inc_100k)
qqnorm(tbdata_rr_AFR$e_rr_inc_100k)
qqline(tbdata_rr_AFR$e_rr_inc_100k)

tbdata_rr_AFR %>% 
  ggplot(aes(x=e_rr_inc_100k))+ ggtitle("Rifampicin Resistant TB Incidence Per 100000 African Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Rifampicin Resistant TB Per 100000")

tbdata_rr_AFR <- tbdata_rr_AFR %>% 
  mutate(log_rr_AFR <- log(e_rr_inc_100k))

summary(tbdata_rr_AFR$`log_rr_AFR <- log(e_rr_inc_100k)`)
sd(tbdata_rr_AFR$`log_rr_AFR <- log(e_rr_inc_100k)`)
qqnorm(tbdata_rr_AFR$`log_rr_AFR <- log(e_rr_inc_100k)`)
qqline(tbdata_rr_AFR$`log_rr_AFR <- log(e_rr_inc_100k)`)

tbdata_rr_AFR %>% 
  ggplot(aes(x=`log_rr_AFR <- log(e_rr_inc_100k)`))+ ggtitle("Log Adjusted Rifampicin Resistant TB Incidence Per 100000 African Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted Rifampicin Resistant TB Per 100000")

AFR_DM <- tbdata_AFR %>% 
  drop_na(DM_prev)

summary(tbdata_AFR$DM_prev)
qqnorm(tbdata_AFR$DM_prev)
qqline(tbdata_AFR$DM_prev)
sd(AFR_DM$DM_prev)

tbdata_rr_AFR %>% 
  ggplot(aes(x=DM_prev))+ ggtitle("Diabetes Prevalence African Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Diabetes Prevalence")

tbdata_AFR <- tbdata_AFR %>% 
  mutate(log_DM_AFR <- log(DM_prev))

summary(tbdata_AFR$`log_DM_AFR <- log(DM_prev)`)
qqnorm(tbdata_AFR$`log_DM_AFR <- log(DM_prev)`)
qqline(tbdata_AFR$`log_DM_AFR <- log(DM_prev)`)
sd(AFR_DM$`log_DM_AFR <- log(DM_prev)`)

tbdata_AFR %>% 
  ggplot(aes(x=`log_DM_AFR <- log(DM_prev)`))+ ggtitle("Log Adjusted Diabetes Prevalence African Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted Diabetes Prevalence")

summary(tbdata_AFR$income_cap)
qqnorm(tbdata_AFR$income_cap)
qqline(tbdata_AFR$income_cap)

income_AFR <- tbdata_AFR %>% 
  drop_na(income_cap)
 

sd(income_AFR$income_cap)

tbdata_AFR %>% 
  ggplot(aes(x=income_cap))+ ggtitle("Adjusted Income Per Capita African Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Adjusted Income Per Capita")

tbdata_AFR <- tbdata_AFR %>% 
  filter(income_cap > 0) %>% 
  mutate(log_income_AFR <- log(income_cap))

summary(tbdata_AFR$`log_income_AFR <- log(income_cap)`)
qqnorm(tbdata_AFR$`log_income_AFR <- log(income_cap)`)
qqline(tbdata_AFR$`log_income_AFR <- log(income_cap)`)
sd(income_AFR$`log_income_AFR <- log(income_cap)`)

tbdata_AFR %>% 
  ggplot(aes(x=`log_income_AFR <- log(income_cap)`))+ ggtitle("Log Adjusted Income Per Capita African Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted Income Per Capita")

tbdata_AFR <- tbdata_AFR %>% 
  mutate(HIV_inc = HIV_inc*100)

summary(tbdata_AFR$HIV_inc)
qqnorm(tbdata_AFR$HIV_inc)
qqline(tbdata_AFR$HIV_inc)


HIV_AFR <- tbdata_AFR %>% 
  drop_na(HIV_inc)

sd(HIV_AFR$HIV_inc)

tbdata_AFR <- tbdata_AFR %>% 
  mutate(log_HIV_AFR = log(HIV_inc))

summary(tbdata_AFR$log_HIV_AFR)
qqnorm(tbdata_AFR$log_HIV_AFR)
qqline(tbdata_AFR$log_HIV_AFR)
sd(HIV_AFR$log_HIV_AFR)

tbdata_AFR %>% 
  ggplot(aes(x=HIV_inc))+ ggtitle("HIV Incidence Per 100000 African Region") +
  geom_histogram(bins=20, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("HIV Incidence Per 100000")

tbdata_AFR %>% 
  ggplot(aes(x=log_HIV_AFR))+ ggtitle("Log Adjusted HIV Incidence Per 100000 African Region") +
  geom_histogram(bins=20, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted HIV Incidence Per 100000")

malnut_AFR <- tbdata_AFR %>% 
  drop_na(`Malnutrition_prev `)

summary(tbdata_AFR$`Malnutrition_prev `)
qqnorm(tbdata_AFR$`Malnutrition_prev `)
qqline(tbdata_AFR$`Malnutrition_prev `)
sd(malnut_AFR$`Malnutrition_prev `)

tbdata_AFR %>% 
  ggplot(aes(x= `Malnutrition_prev `))+ ggtitle("Malnutrition Prevalence African Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Malnutrition Prevalence")

tbdata_AFR <- tbdata_AFR %>% 
  mutate(mal_log_AFR <- log(`Malnutrition_prev `))

summary(tbdata_AFR$`mal_log_AFR <- log(\`Malnutrition_prev \`)`)
qqnorm(tbdata_AFR$`mal_log_AFR <- log(\`Malnutrition_prev \`)`)
qqline(tbdata_AFR$`mal_log_AFR <- log(\`Malnutrition_prev \`)`)
sd(malnut_AFR$`mal_log_AFR <- log(\`Malnutrition_prev \`)`)

tbdata_AFR %>% 
  ggplot(aes(x= `mal_log_AFR <- log(\`Malnutrition_prev \`)`))+ ggtitle("Log Adjusted Malnutrition Prevalence African Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted Malnutrition Prevalence")

summary(tbdata_AFR$Budget)
qqnorm(tbdata_AFR$Budget)
qqline(tbdata_AFR$Budget)

budget_AFR <- tbdata_AFR %>% 
  drop_na(Budget)

sd(budget_AFR$Budget)

tbdata_AFR %>% 
  ggplot(aes(x= Budget))+ ggtitle("TB Activities Budget African Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Activities Budget")

tbdata_AFR <- tbdata_AFR %>% 
  mutate(log_budget_AFR <- log(Budget))

summary(tbdata_AFR$`log_budget_AFR <- log(Budget)`)
qqnorm(tbdata_AFR$`log_budget_AFR <- log(Budget)`)
qqline(tbdata_AFR$`log_budget_AFR <- log(Budget)`)

sd(budget_AFR$`log_budget_AFR <- log(Budget)`)



tbdata_AFR %>% 
  ggplot(aes(x= `log_budget_AFR <- log(Budget)`))+ ggtitle("Log Adjusted TB Activities Budget African Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted TB Activities Budget")

#American region

tbdata_AMR <- tbdata_AMR %>% 
  filter(e_inc_num >= 100)

summary(tbdata_AMR$e_inc_100k)
qqnorm(tbdata_AMR$e_inc_100k)
qqline(tbdata_AMR$e_inc_100k)
sd(tbdata_AMR$e_inc_100k)

tbdata_AMR %>% 
  ggplot(aes(x= e_inc_100k))+ ggtitle("TB Incidence Per 100000 Americas Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Incidence Per 100000")

tbdata_AMR <- tbdata_AMR %>% 
  mutate(log_inc_AMR = log(e_inc_100k))

summary(tbdata_AMR$log_inc_AMR)
sd(tbdata_AMR$log_inc_AMR)
qqnorm(tbdata_AMR$log_inc_AMR)
qqline(tbdata_AMR$log_inc_AMR)

tbdata_AMR %>% 
  ggplot(aes(x=log_inc_AMR))+ ggtitle("Log Adjusted TB Incidence Per 100000 Americas Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted TB Incidence Per 100000")

AMR_mort <- tbdata_AMR %>% 
  drop_na(e_mort_100k)

summary(tbdata_AMR$e_mort_100k)
qqnorm(tbdata_AMR$e_mort_100k)
qqline(tbdata_AMR$e_mort_100k)
sd(AMR_mort$e_mort_100k)

tbdata_AMR %>% 
  ggplot(aes(x=e_mort_100k))+ ggtitle("TB Mortality Per 100000 Americas Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Mortality Per 100000")

tbdata_AMR <- tbdata_AMR %>% 
  mutate(log_mort_AMR = log(e_mort_100k))

summary(tbdata_AMR$log_mort_AMR)
qqnorm(tbdata_AMR$log_mort_AMR)
qqline(tbdata_AMR$log_mort_AMR)
sd(AMR_mort$log_mort_AMR)

tbdata_AMR %>% 
  ggplot(aes(x=log_mort_AMR))+ ggtitle("Log Adjusted TB Mortality Per 100000 Americas Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted TB Mortality Per 100000")

cfr_AMR <- tbdata_AMR %>% 
  drop_na(case_fatality)

summary(tbdata_AMR$case_fatality)
qqnorm(tbdata_AMR$case_fatality)
qqline(tbdata_AMR$case_fatality)
sd(cfr_AMR$case_fatality)

tbdata_AMR %>% 
  ggplot(aes(x=case_fatality))+ ggtitle("TB Case Fatality Ratio Americas Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Case Fatality Ratio")


rr_AMR <- tbdata_AMR %>% 
  drop_na(e_rr_inc_100k)

summary(rr_AMR$e_rr_inc_100k)
qqnorm(rr_AMR$e_rr_inc_100k)
qqline(rr_AMR$e_rr_inc_100k)
sd(rr_AMR$e_rr_inc_100k)

rr_AMR %>% 
  ggplot(aes(x=e_rr_inc_100k))+ ggtitle("Rifampicin Resistant TB Incidence per 100000 Americas Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Rifampicin Resistant TB Incidence per 100000")

rr_AMR <- rr_AMR %>% 
  mutate(log_rr_AMR = log(e_rr_inc_100k))

summary(rr_AMR$log_rr_AMR)
qqnorm(rr_AMR$log_rr_AMR)
qqline(rr_AMR$log_rr_AMR)
sd(rr_AMR$log_rr_AMR)

rr_AMR %>% 
  ggplot(aes(x=log_rr_AMR))+ ggtitle("Log Adjusted Rifampicin Resistant TB Incidence per 100000 Americas Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted Rifampicin Resistant TB Incidence per 100000")

summary(tbdata_AMR$DM_prev)
qqnorm(tbdata_AMR$DM_prev)
qqline(tbdata_AMR$DM_prev)

DM_AMR <- tbdata_AMR %>% 
  drop_na(DM_prev)

sd(DM_AMR$DM_prev)

tbdata_AMR %>% 
  ggplot(aes(x=DM_prev))+ ggtitle("Diabetes Prevalence Americas Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Diabetes Prevalence")

tbdata_AMR <- tbdata_AMR %>% 
  mutate(log_DM_AMR = log(DM_prev))

summary(tbdata_AMR$log_DM_AMR)
qqnorm(tbdata_AMR$log_DM_AMR)
qqline(tbdata_AMR$log_DM_AMR)
sd(DM_AMR$log_DM_AMR)

tbdata_AMR %>% 
  ggplot(aes(x=log_DM_AMR))+ ggtitle("Log Adjusted Diabetes Prevalence Americas Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted Diabetes Prevalence")

income_AMR <- tbdata_AMR %>% 
  drop_na(income_cap)

summary(tbdata_AMR$income_cap)
qqnorm(tbdata_AMR$income_cap)
qqline(tbdata_AMR$income_cap)
sd(income_AMR$income_cap)

tbdata_AMR %>% 
  ggplot(aes(x=income_cap))+ ggtitle("Adjusted Income Per Capita Americas Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Adjusted Income Per Capita")

tbdata_AMR <- tbdata_AMR %>% 
  mutate(log_income_AMR = log(income_cap))

summary(tbdata_AMR$log_income_AMR)
qqnorm(tbdata_AMR$log_income_AMR)
qqline(tbdata_AMR$log_income_AMR)
sd(income_AMR$log_income_AMR)

tbdata_AMR %>% 
  ggplot(aes(x=log_income_AMR))+ ggtitle("Log Adjusted Income Per Capita Americas Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted Income Per Capita")

tbdata_AMR <- tbdata_AMR %>% 
  mutate(HIV_inc = HIV_inc*100)

hiv_AMR <- tbdata_AMR %>% 
  drop_na(HIV_inc)
summary(tbdata_AMR$HIV_inc)
qqnorm(tbdata_AMR$HIV_inc)
qqline(tbdata_AMR$HIV_inc)
sd(hiv_AMR$HIV_inc)

tbdata_AMR %>% 
  ggplot(aes(x=HIV_inc))+ ggtitle("HIV Incidence Per 100000 Americas Region") +
  geom_histogram(bins=20, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("HIV Incidence Per 100000")

tbdata_AMR <- tbdata_AMR %>% 
  mutate(log_HIV_AMR = log(HIV_inc))

summary(tbdata_AMR$log_HIV_AMR)
qqnorm(tbdata_AMR$log_HIV_AMR)
qqline(tbdata_AMR$log_HIV_AMR)
sd(hiv_AMR$log_HIV_AMR)

tbdata_AMR %>% 
  ggplot(aes(x=log_HIV_AMR))+ ggtitle("Log Adjusted HIV Incidence Per 100000 Americas Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted HIV Incidence Per 100000")

AMR_mal <- tbdata_AMR %>% 
  drop_na(`Malnutrition_prev `)

summary(tbdata_AMR$`Malnutrition_prev `)
qqnorm(tbdata_AMR$`Malnutrition_prev `)
qqline(tbdata_AMR$`Malnutrition_prev `)
sd(AMR_mal$`Malnutrition_prev `)

tbdata_AMR %>% 
  ggplot(aes(x=`Malnutrition_prev `))+ ggtitle("Malnutrition Prevalence Americas Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Malnutrition Prevalence")

tbdata_AMR <- tbdata_AMR %>% 
  mutate(log_mal_AMR = log(`Malnutrition_prev `))

summary(tbdata_AMR$log_mal_AMR)
qqnorm(tbdata_AMR$log_mal_AMR)
qqline(tbdata_AMR$log_mal_AMR)
sd(AMR_mal$log_mal_AMR)

tbdata_AMR %>% 
  ggplot(aes(x=log_mal_AMR))+ ggtitle("Log Adjusted Malnutrition Prevalence Americas Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted Malnutrition Prevalence")

budget_AMR <- tbdata_AMR %>% 
  drop_na(Budget)

summary(tbdata_AMR$Budget)
qqnorm(tbdata_AMR$Budget)
qqline(tbdata_AMR$Budget)
sd(budget_AMR$Budget)

tbdata_AMR %>% 
  ggplot(aes(x=Budget))+ ggtitle("TB Activities Budget Americas Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Activities Budget")

tbdata_AMR <- tbdata_AMR %>% 
  mutate(log_budget_AMR = log(Budget))

summary(tbdata_AMR$log_budget_AMR)
qqnorm(tbdata_AMR$log_budget_AMR)
qqline(tbdata_AMR$log_budget_AMR)
sd(budget_AMR$log_budget_AMR)

tbdata_AMR %>% 
  ggplot(aes(x=log_budget_AMR))+ ggtitle("Log Adjusted TB Activities Budget Americas Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted TB Activities Budget")

#Eastern Med Region 

tbdata_EMR <- tbdata_EMR %>% 
  filter(e_inc_num >= 100) %>% 
  mutate(HIV_inc = HIV_inc*100)

summary(tbdata_EMR$e_inc_100k)
qqnorm(tbdata_EMR$e_inc_100k)
qqline(tbdata_EMR$e_inc_100k)
sd(tbdata_EMR$e_inc_100k)

tbdata_EMR %>% 
  ggplot(aes(x=e_inc_100k))+ ggtitle("TB Incidence Per 100000 Eastern Mediterranean Region") +
  geom_histogram(bins=30, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Incidence Per 100000")

tbdata_EMR <- tbdata_EMR %>% 
  mutate(log_inc_EMR = log(e_inc_100k))

summary(tbdata_EMR$log_inc_EMR)
qqnorm(tbdata_EMR$log_inc_EMR)
qqline(tbdata_EMR$log_inc_EMR)
sd(tbdata_EMR$log_inc_EMR)

tbdata_EMR %>% 
  ggplot(aes(x=log_inc_EMR))+ ggtitle("Log Adjusted TB Incidence Per 100000 Eastern Mediterranean Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted TB Incidence Per 100000")

EMR_mort <- tbdata_EMR %>% 
  drop_na(e_mort_100k)

summary(tbdata_EMR$e_mort_100k)
qqnorm(tbdata_EMR$e_mort_100k)
qqline(tbdata_EMR$e_mort_100k)
sd(EMR_mort$e_mort_100k)

tbdata_EMR %>% 
  ggplot(aes(x=e_mort_100k))+ ggtitle("TB Mortality Per 100000 Eastern Mediterranean Region") +
  geom_histogram(bins=20, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Mortality Per 100000")

tbdata_EMR <- tbdata_EMR %>% 
  mutate(log_mort_EMR = log(e_mort_100k))

summary(tbdata_EMR$log_mort_EMR)
qqnorm(tbdata_EMR$log_mort_EMR)
qqline(tbdata_EMR$log_mort_EMR)
sd(EMR_mort$log_mort_EMR)

tbdata_EMR %>% 
  ggplot(aes(x=log_mort_EMR))+ ggtitle("Log Adjusted TB Mortality Per 100000 Eastern Mediterranean Region") +
  geom_histogram(bins=20, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted TB Mortality Per 100000")

cfr_EMR <- tbdata_EMR %>% 
  drop_na(case_fatality)

summary(tbdata_EMR$case_fatality)
qqnorm(tbdata_EMR$case_fatality)
qqline(tbdata_EMR$case_fatality)
sd(cfr_EMR$case_fatality)

tbdata_EMR %>% 
  ggplot(aes(x=case_fatality))+ ggtitle("TB Case Fatality Ratio Eastern Mediterranean Region") +
  geom_histogram(bins=20, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Case Fatality Ratio")

rr_EMR <- tbdata_EMR %>% 
  drop_na(e_rr_inc_100k)

summary(rr_EMR$e_rr_inc_100k)
qqnorm(rr_EMR$e_rr_inc_100k)
qqline(rr_EMR$e_rr_inc_100k)
sd(rr_EMR$e_rr_inc_100k)

rr_EMR %>% 
  ggplot(aes(x=e_rr_inc_100k))+ ggtitle("Rifampicin Resistant TB Incidence Per 100000 Eastern Mediterranean Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Rifampicin Resistant TB Incidence Per 100000")

rr_EMR <- rr_EMR %>% 
  mutate(log_rr_EMR = log(e_rr_inc_100k))

summary(rr_EMR$log_rr_EMR)
qqnorm(rr_EMR$log_rr_EMR)
qqline(rr_EMR$log_rr_EMR)
sd(rr_EMR$log_rr_EMR)

rr_EMR %>% 
  ggplot(aes(x=log_rr_EMR))+ ggtitle("Log Adjusted Rifampicin Resistant TB Incidence Per 100000 Eastern Mediterranean Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted Rifampicin Resistant TB Incidence Per 100000")

DM_EMR <- tbdata_EMR %>% 
  drop_na(DM_prev)

summary(tbdata_EMR$DM_prev)
qqnorm(tbdata_EMR$DM_prev)
qqline(tbdata_EMR$DM_prev)
sd(DM_EMR$DM_prev)

tbdata_EMR %>% 
  ggplot(aes(x=DM_prev))+ ggtitle("Diabetes Prevalence Eastern Mediterranean Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Diabetes Prevalence")

tbdata_EMR <- tbdata_EMR %>% 
  mutate(log_DM_EMR = log(DM_prev))

summary(tbdata_EMR$log_DM_EMR)
qqnorm(tbdata_EMR$log_DM_EMR)
qqline(tbdata_EMR$log_DM_EMR)
sd(DM_EMR$log_DM_EMR)

tbdata_EMR %>% 
  ggplot(aes(x=log_DM_EMR))+ ggtitle("Log Adjusted Diabetes Prevalence Eastern Mediterranean Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted Diabetes Prevalence")

income_EMR <- tbdata_EMR %>% 
  drop_na(income_cap)

summary(tbdata_EMR$income_cap)
qqnorm(tbdata_EMR$income_cap)
qqline(tbdata_EMR$income_cap)
sd(income_EMR$income_cap)

tbdata_EMR %>% 
  ggplot(aes(x=income_cap))+ ggtitle("Adjusted Income Per Capita Eastern Mediterranean Region") +
  geom_histogram(bins=50, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Adjust Income Per Capita")

tbdata_EMR <- tbdata_EMR %>% 
  mutate(log_income_EMR = log(income_cap))

summary(tbdata_EMR$log_income_EMR)
qqnorm(tbdata_EMR$log_income_EMR)
qqline(tbdata_EMR$log_income_EMR)
sd(income_EMR$log_income_EMR)

tbdata_EMR %>% 
  ggplot(aes(x=log_income_EMR))+ ggtitle("Log Adjusted Income Per Capita Eastern Mediterranean Region") +
  geom_histogram(bins=20, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted Income Per Capita")

HIV_EMR <- tbdata_EMR %>% 
  drop_na(HIV_inc)

summary(tbdata_EMR$HIV_inc)
qqnorm(tbdata_EMR$HIV_inc)
qqline(tbdata_EMR$HIV_inc)
sd(HIV_EMR$HIV_inc)

tbdata_EMR %>% 
  ggplot(aes(x=HIV_inc))+ ggtitle("HIV Incidence Per 100000 Eastern Mediterranean Region") +
  geom_histogram(bins=50, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("HIV Incidence Per 100000")

tbdata_EMR <- tbdata_EMR %>% 
  mutate(log_HIV_EMR = log(HIV_inc))

summary(tbdata_EMR$log_HIV_EMR)
qqnorm(tbdata_EMR$log_HIV_EMR)
qqline(tbdata_EMR$log_HIV_EMR)
sd(HIV_EMR$log_HIV_EMR)

tbdata_EMR %>% 
  ggplot(aes(x=log_HIV_EMR))+ ggtitle("Log Adjusted HIV Incidence Per 100000 Eastern Mediterranean Region") +
  geom_histogram(bins=20, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted HIV Incidence Per 100000")

mal_EMR <- tbdata_EMR %>% 
  drop_na(`Malnutrition_prev `)

summary(tbdata_EMR$`Malnutrition_prev `)
qqnorm(tbdata_EMR$`Malnutrition_prev `)
qqline(tbdata_EMR$`Malnutrition_prev `)
sd(mal_EMR$`Malnutrition_prev `)

tbdata_EMR %>% 
  ggplot(aes(x=`Malnutrition_prev `))+ ggtitle("Malnutrition Prevalence Eastern Mediterranean Region") +
  geom_histogram(bins=20, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Malnutrition Prevalence")

tbdata_EMR <- tbdata_EMR %>% 
  mutate(log_mal_EMR = log(`Malnutrition_prev `))

summary(tbdata_EMR$log_mal_EMR)
qqnorm(tbdata_EMR$log_mal_EMR)
qqline(tbdata_EMR$log_mal_EMR)
sd(mal_EMR$log_mal_EMR)

tbdata_EMR %>% 
  ggplot(aes(x=log_mal_EMR))+ ggtitle("Log Adjusted Malnutrition Prevalence Eastern Mediterranean Region") +
  geom_histogram(bins=20, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted Malnutrition Prevalence")

budget_EMR <- tbdata_EMR %>% 
  drop_na(Budget)

summary(tbdata_EMR$Budget)
qqnorm(tbdata_EMR$Budget)
qqline(tbdata_EMR$Budget)
sd(budget_EMR$Budget)

tbdata_EMR %>% 
  ggplot(aes(x=Budget))+ ggtitle("TB Activities Budget Eastern Mediterranean Region") +
  geom_histogram(bins=5, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Activities Budget")

tbdata_EMR <- tbdata_EMR %>% 
  mutate(log_budget_EMR = log(Budget))

summary(tbdata_EMR$log_budget_EMR)
qqnorm(tbdata_EMR$log_budget_EMR)
qqline(tbdata_EMR$log_budget_EMR)
sd(budget_EMR$log_budget_EMR)

tbdata_EMR %>% 
  ggplot(aes(x=log_budget_EMR))+ ggtitle("Log Adjusted TB Activities Budget Eastern Mediterranean Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted TB Activities Budget")

#South East Asia Region

tbdata_SEA <- tbdata_SEA %>% 
  filter(e_inc_num >= 100) %>% 
  mutate(HIV_inc = HIV_inc*100)

summary(tbdata_SEA$e_inc_100k)
qqnorm(tbdata_SEA$e_inc_100k)
qqline(tbdata_SEA$e_inc_100k)
sd(tbdata_SEA$e_inc_100k)

tbdata_SEA %>% 
  ggplot(aes(x=e_inc_100k))+ ggtitle("TB Incidence Per 100000 South East Asia Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Incidence Per 100000")

tbdata_SEA <- tbdata_SEA %>% 
  mutate(log_inc_SEA = log(e_inc_100k))

summary(tbdata_SEA$log_inc_SEA)
qqnorm(tbdata_SEA$log_inc_SEA)
qqline(tbdata_SEA$log_inc_SEA)
sd(tbdata_SEA$log_inc_SEA)

tbdata_SEA %>% 
  ggplot(aes(x=log_inc_SEA))+ ggtitle("Log Adjusted TB Incidence Per 100000 South East Asia Region") +
  geom_histogram(bins=20, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Log Adjusted TB Incidence Per 100000")

#Realizing that I can do the rest of my transformations after fitting my model and seeing the distribution of errors and residuals :(
# Will do box-cox transformations instead after fitting my models. Just plotting the distribution for the rest of my variables from here

mort_SEA <- tbdata_SEA %>% 
  drop_na(e_mort_100k)

summary(tbdata_SEA$e_mort_100k)
qqnorm(tbdata_SEA$e_mort_100k)
qqline(tbdata_SEA$e_mort_100k)
sd(mort_SEA$e_mort_100k)

#Code here is stuff I'm coming back to get descrpitive statistics for my report so it's technically out of order
#Look below at my model coding to see the log transformations I did
summary(tbdata_SEA$log_mort_SEA)
sd(mort_SEA$log_mort_SEA)

tbdata_SEA %>% 
  ggplot(aes(x=e_mort_100k))+ ggtitle("TB Mortality Per 100000 South East Asia Region") +
  geom_histogram(bins=20, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Mortality Per 100000")

cfr_SEA <- tbdata_SEA %>% 
  drop_na(case_fatality)

summary(tbdata_SEA$case_fatality)
qqnorm(tbdata_SEA$case_fatality)
qqline(tbdata_SEA$case_fatality)
sd(cfr_SEA$case_fatality)

tbdata_SEA %>% 
  ggplot(aes(x=case_fatality))+ ggtitle("TB Case Fatality Ratio South East Asia Region") +
  geom_histogram(bins=20, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Case Fatality Ratio")

rr_SEA <- tbdata_SEA %>% 
  drop_na(e_rr_inc_100k)

summary(rr_SEA$e_rr_inc_100k)
qqnorm(rr_SEA$e_rr_inc_100k)
qqline(rr_SEA$e_rr_inc_100k)
sd(rr_SEA$e_rr_inc_100k)

rr_SEA %>% 
  ggplot(aes(x=e_rr_inc_100k))+ ggtitle("Rifampicin Resistant TB Incidence Per 100000 South East Asia Region") +
  geom_histogram(bins=20, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Rifampicin Resistant TB Incidence Per 100000")

DM_SEA <- tbdata_SEA %>% 
  drop_na(DM_prev)

summary(tbdata_SEA$DM_prev)
qqnorm(tbdata_SEA$DM_prev)
qqline(tbdata_SEA$DM_prev)
sd(DM_SEA$DM_prev)

tbdata_SEA %>% 
  ggplot(aes(x=DM_prev))+ ggtitle("Diabetes Prevalence South East Asia Region") +
  geom_histogram(bins=20, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Diabetes Prevalence")

income_SEA <- tbdata_SEA %>% 
  drop_na(income_cap)

summary(tbdata_SEA$income_cap)
qqnorm(tbdata_SEA$income_cap)
qqline(tbdata_SEA$income_cap)
sd(income_SEA$income_cap)

summary(tbdata_SEA$log_income)
sd(income_SEA$log_income)

tbdata_SEA %>% 
  ggplot(aes(x=DM_prev))+ ggtitle("Adjusted Income Per Capita South East Asia Region") +
  geom_histogram(bins=20, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Adjusted Income Per Capita")


HIV_SEA <- tbdata_SEA %>% 
  drop_na(HIV_inc)

summary(tbdata_SEA$HIV_inc)
qqnorm(tbdata_SEA$HIV_inc)
qqline(tbdata_SEA$HIV_inc)
sd(HIV_SEA$HIV_inc)

summary(tbdata_SEA$log_HIV_SEA)
sd(HIV_SEA$log_HIV_SEA)


tbdata_SEA %>% 
  ggplot(aes(x=HIV_inc))+ ggtitle("HIV Incidence Per 100000 South East Asia Region") +
  geom_histogram(bins=20, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("HIV Incidence Per 100000")

mal_SEA <- tbdata_SEA %>% 
  drop_na(`Malnutrition_prev `)

summary(tbdata_SEA$`Malnutrition_prev `)
qqnorm(tbdata_SEA$`Malnutrition_prev `)
qqline(tbdata_SEA$`Malnutrition_prev `)
sd(mal_SEA$`Malnutrition_prev `)

tbdata_SEA %>% 
  ggplot(aes(x=`Malnutrition_prev `))+ ggtitle("Malnutrition Prevalence South East Asia Region") +
  geom_histogram(bins=20, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Malnutrition Prevalence")

budget_SEA <- tbdata_SEA %>% 
  drop_na(Budget)

summary(tbdata_SEA$Budget)
qqnorm(tbdata_SEA$Budget)
qqline(tbdata_SEA$Budget)
sd(budget_SEA$Budget)

tbdata_SEA %>% 
  ggplot(aes(x=Budget))+ ggtitle("TB Activities Budget South East Asia Region") +
  geom_histogram(bins=6, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Activities Budget")

#European Region

tbdata_EUR <- tbdata_EUR %>% 
  filter(e_inc_num >= 100) %>% 
  mutate(HIV_inc = HIV_inc*100)
  

summary(tbdata_EUR$e_inc_100k)
qqnorm(tbdata_EUR$e_inc_100k)
qqline(tbdata_EUR$e_inc_100k)
sd(tbdata_EUR$e_inc_100k)

summary(tbdata_EUR$log_inc_EUR)
sd(tbdata_EUR$log_inc_EUR)

tbdata_EUR %>% 
  ggplot(aes(x=e_inc_100k))+ ggtitle("TB Incidence Per 100000 European Region") +
  geom_histogram(bins=30, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Incidence Per 100000")

mort_EUR <- tbdata_EUR %>% 
  drop_na(e_mort_100k)

summary(tbdata_EUR$e_mort_100k)
qqnorm(tbdata_EUR$e_mort_100k)
qqline(tbdata_EUR$e_mort_100k)
sd(mort_EUR$e_mort_100k)

summary(tbdata_EUR$log_mort_EUR)
sd(mort_EUR$log_mort_EUR)

tbdata_EUR %>% 
  ggplot(aes(x=e_mort_100k))+ ggtitle("TB Mortality Per 100000 European Region") +
  geom_histogram(bins=30, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Mortality Per 100000")

cfr_EUR <- tbdata_EUR %>% 
  drop_na(case_fatality)

summary(tbdata_EUR$case_fatality)
qqnorm(tbdata_EUR$case_fatality)
qqline(tbdata_EUR$case_fatality)
sd(cfr_EUR$case_fatality)

tbdata_EUR %>% 
  ggplot(aes(x=case_fatality))+ ggtitle("TB Case Fatality Ratio European Region") +
  geom_histogram(bins=15, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Case Fatality Ratio")

rr_EUR <- tbdata_EUR %>% 
  drop_na(e_rr_inc_100k)

summary(rr_EUR$e_rr_inc_100k)
qqnorm(rr_EUR$e_rr_inc_100k)
qqline(rr_EUR$e_rr_inc_100k)
sd(rr_EUR$e_rr_inc_100k)

rr_EUR %>% 
  ggplot(aes(x=e_rr_inc_100k))+ ggtitle("Rifampicin Resistant TB Incidence Per 100000 European Region") +
  geom_histogram(bins=15, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Case Fatality Ratio")

DM_EUR <- tbdata_EUR %>% 
  drop_na(DM_prev)

summary(tbdata_EUR$DM_prev)
qqnorm(tbdata_EUR$DM_prev)
qqline(tbdata_EUR$DM_prev)
sd(DM_EUR$DM_prev)

tbdata_EUR %>% 
  ggplot(aes(x=DM_prev))+ ggtitle("Diabetes Prevalence European Region") +
  geom_histogram(bins=15, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Diabetes Prevalence")

income_EUR <- tbdata_EUR %>% 
  drop_na(income_cap)

summary(tbdata_EUR$income_cap)
qqnorm(tbdata_EUR$income_cap)
qqline(tbdata_EUR$income_cap)
sd(income_EUR$income_cap)

summary(tbdata_EUR$log_income)
sd(income_EUR$log_income)

tbdata_EUR %>% 
  ggplot(aes(x=income_cap))+ ggtitle("Adjusted Income per Capita European Region") +
  geom_histogram(bins=15, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Adjusted Income Per Capita")

HIV_EUR <- tbdata_EUR %>% 
  drop_na(HIV_inc)

summary(tbdata_EUR$HIV_inc)
qqnorm(tbdata_EUR$HIV_inc)
qqline(tbdata_EUR$HIV_inc)
sd(HIV_EUR$HIV_inc)

summary(tbdata_EUR$log_HIV_EUR)
sd(HIV_EUR$log_HIV_EUR)

tbdata_EUR %>% 
  ggplot(aes(x=HIV_inc))+ ggtitle("HIV Incidence per 100000 European Region") +
  geom_histogram(bins=15, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("HIV Incidence per 100000")

mal_EUR <- tbdata_EUR %>% 
  drop_na(`Malnutrition_prev `)

summary(tbdata_EUR$`Malnutrition_prev `)
qqnorm(tbdata_EUR$`Malnutrition_prev `)
qqline(tbdata_EUR$`Malnutrition_prev `)
sd(mal_EUR$`Malnutrition_prev `)

tbdata_EUR %>% 
  ggplot(aes(x=`Malnutrition_prev `))+ ggtitle("Malnutrition Prevalence European Region") +
  geom_histogram(bins=30, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Malnutrition Prevalence")

budget_EUR <- tbdata_EUR %>% 
  drop_na(Budget)

summary(tbdata_EUR$Budget)
qqnorm(tbdata_EUR$Budget)
qqline(tbdata_EUR$Budget)
sd(budget_EUR$Budget)

budget_EUR %>% 
  ggplot(aes(x=Budget))+ ggtitle("TB Activities Budget European Region") +
  geom_histogram(bins=50, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Activities Budget")

#Western Pacfic Region 

tbdata_WPR <- tbdata_WPR %>% 
  filter(e_inc_num >= 100) %>% 
  mutate(HIV_inc = HIV_inc*100)
  

summary(tbdata_WPR$e_inc_100k)
qqnorm(tbdata_WPR$e_inc_100k)
qqline(tbdata_WPR$e_inc_100k)
sd(tbdata_WPR$e_inc_100k)

summary(tbdata_WPR$log_inc_WPR)
sd(tbdata_WPR$log_inc_WPR)

tbdata_WPR %>% 
  ggplot(aes(x=e_inc_100k))+ ggtitle("TB Incidence Per 100000 Western Pacific Region") +
  geom_histogram(bins=50, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Incidence Per 100000")


mort_WPR <- tbdata_WPR %>% 
  drop_na(e_mort_100k)

summary(tbdata_WPR$e_mort_100k)
qqnorm(tbdata_WPR$e_mort_100k)
qqline(tbdata_WPR$e_mort_100k)
sd(mort_WPR$e_mort_100k)

summary(tbdata_WPR$log_mort_WPR)
sd(mort_WPR$log_mort_WPR)

tbdata_WPR %>% 
  ggplot(aes(x=e_mort_100k))+ ggtitle("TB Mortality Per 100000 Western Pacific Region") +
  geom_histogram(bins=50, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Mortality Per 100000")

cfr_WPR <- tbdata_WPR %>% 
  drop_na(case_fatality)

summary(tbdata_WPR$case_fatality)
qqnorm(tbdata_WPR$case_fatality)
qqline(tbdata_WPR$case_fatality)
sd(cfr_WPR$case_fatality)

tbdata_WPR %>% 
  ggplot(aes(x=case_fatality))+ ggtitle("TB Case Fatality Ratio Western Pacific Region") +
  geom_histogram(bins=50, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Case Fatality Ratio")

rr_WPR <- tbdata_WPR %>% 
  drop_na(e_rr_inc_100k)

summary(rr_WPR$e_rr_inc_100k)
qqnorm(rr_WPR$e_rr_inc_100k)
qqline(rr_WPR$e_rr_inc_100k)
sd(rr_WPR$e_rr_inc_100k)

rr_WPR %>% 
  ggplot(aes(x=e_rr_inc_100k))+ ggtitle("Rifampicin Resistant TB Incidence Per 100000 Western Pacific Region") +
  geom_histogram(bins=20, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Rifampicin Resistant TB Incidence Per 100000")

DM_WPR <- tbdata_WPR %>% 
  drop_na(DM_prev)

summary(tbdata_WPR$DM_prev)
qqnorm(tbdata_WPR$DM_prev)
qqline(tbdata_WPR$DM_prev)
sd(DM_WPR$DM_prev)

tbdata_WPR %>% 
  ggplot(aes(x=DM_prev))+ ggtitle("Diabetes Prevalence Western Pacific Region") +
  geom_histogram(bins=15, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Diabetes Prevalence")

income_WPR <- tbdata_WPR %>% 
  drop_na(income_cap)

summary(tbdata_WPR$income_cap)
qqnorm(tbdata_WPR$income_cap)
qqline(tbdata_WPR$income_cap)
sd(income_WPR$income_cap)

summary(tbdata_WPR$log_income)
sd(income_WPR$log_income)

tbdata_WPR %>% 
  ggplot(aes(x=income_cap))+ ggtitle("Adjusted Income Per Capita Western Pacific Region") +
  geom_histogram(bins=15, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Adjusted Income Per Capita")

HIV_WPR <- tbdata_WPR %>% 
  drop_na(HIV_inc)

summary(tbdata_WPR$HIV_inc)
qqnorm(tbdata_WPR$HIV_inc)
qqline(tbdata_WPR$HIV_inc)
sd(HIV_WPR$HIV_inc)

summary(tbdata_WPR$log_HIV_WPR)
sd(HIV_WPR$log_HIV_WPR)

tbdata_WPR %>% 
  ggplot(aes(x=HIV_inc))+ ggtitle("HIV Incidence Per 100000 Western Pacific Region") +
  geom_histogram(bins=15, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("HIV Incidence Per 100000")

mal_WPR <- tbdata_WPR %>% 
  drop_na(`Malnutrition_prev `)

summary(tbdata_WPR$`Malnutrition_prev `)
qqnorm(tbdata_WPR$`Malnutrition_prev `)
qqline(tbdata_WPR$`Malnutrition_prev `)
sd(mal_WPR$`Malnutrition_prev `)

tbdata_WPR %>% 
  ggplot(aes(x=`Malnutrition_prev `))+ ggtitle("Malnutrition Prevalence Western Pacific Region") +
  geom_histogram(bins=15, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Malnutrition Prevalence")

budget_WPR <- tbdata_WPR %>% 
  drop_na(Budget)

summary(tbdata_WPR$Budget)
qqnorm(tbdata_WPR$Budget)
qqline(tbdata_WPR$Budget)
sd(budget_WPR$Budget)

tbdata_WPR %>% 
  ggplot(aes(x=Budget))+ ggtitle("TB Activities Budget Western Pacific Region") +
  geom_histogram(bins=10, fill="salmon", color="black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("TB Activities Budget")

#Checking scatterplots to see relationships between variables of interest 
#Starting with global data 

tbdata_2019_new %>% 
  ggplot (aes(x=DM_prev, y=e_inc_100k)) + ggtitle("Scatterplot of Global TB Incidence Per 100000 and Country Level Diabetes Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method=loess) + xlab("Diabetes Prevalence") + ylab("TB Incidence Per 100000")

#Filtering Myanmar because there was about 3 years where their per capita income skyrocketed to $155,000

which.max(tbdata_2019_new$income_cap)

tbdata_2019_new_1 %>% 
  filter(!grepl('Myanmar', country)) %>% 
  ggplot (aes(x=income_cap, y=e_inc_100k)) + ggtitle("Scatterplot of Global TB Incidence Per 100000 and Adjusted Income Per Capita") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=0.5) + geom_smooth(method=loess) + xlab("Income Per Capita") + ylab("TB Incidence Per 100000")

#Log Adjusted 
tbdata_2019_new_1 %>% 
  filter(!grepl('Myanmar', country)) %>% 
  ggplot (aes(x=`log_income <- log(income_cap)`, y=e_inc_100k)) + ggtitle("Scatterplot of Global TB Incidence Per 100000 and Log Adjusted Income Per Capita") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method=loess) + xlab("Log Adjusted Income Per Capita") + ylab("Log Adjusted TB Incidence Per 100000")

tbdata_2019_new %>% 
  ggplot (aes(x=HIV_inc, y=e_inc_100k)) + ggtitle("Scatterplot of Global TB Incidence Per 100000 and HIV Incidence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("HIV Incidence Per 100000") + ylab("TB Incidence Per 100000")

tbdata_2019_new %>% 
  ggplot (aes(x=`Malnutrition_prev `, y=e_inc_100k)) + 
  ggtitle("Scatterplot of Global TB Incidence Per 100000 and Country Level Malnutrition Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Malnutrition Prevalence") + 
  ylab("TB Incidence Per 100000")

tbdata_2019_new %>% 
  ggplot (aes(x=`log_budget <- log(Budget)`, y=e_inc_100k)) + 
  ggtitle("Scatterplot of Global TB Incidence Per 100000 and Country Level TB Activities Budget") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("TB Activities Budget") + 
  ylab("TB Incidence Per 100000")

#Budget is too messy to keep including, going to exclude it from my analysis going forward. 
#Now global mortality 

tbdata_2019_new %>% 
  ggplot (aes(x= DM_prev, y=e_mort_100k)) + 
  ggtitle("Scatterplot of Global TB Mortality Per 100000 and Country Level Diabetes Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Diabetes Prevalence") + 
  ylab("TB Mortality Per 100000")

tbdata_2019_new %>% 
  filter(!grepl('Myanmar', country)) %>%
  ggplot (aes(x= income_cap, y=e_mort_100k)) + 
  ggtitle("Scatterplot of Global TB Mortality Per 100000 and Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Adjusted Per Capita Income") + 
  ylab("TB Mortality Per 100000")

tbdata_2019_new %>% 
  filter(!grepl('Myanmar', country)) %>%
  ggplot (aes(x= `log_income <- log(income_cap)`, y=e_mort_100k)) + 
  ggtitle("Log Adjusted Scatterplot of Global TB Mortality Per 100000 and Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted Per Capita Income") + 
  ylab("TB Mortality Per 100000")

tbdata_2019_new %>% 
  ggplot (aes(x= HIV_inc, y=e_mort_100k)) + 
  ggtitle("Scatterplot of Global TB Mortality Per 100000 and HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("HIV Incidence Per 100000") + 
  ylab("TB Mortality Per 100000")

tbdata_2019_new %>% 
  ggplot (aes(x= `Malnutrition_prev `, y=e_mort_100k)) + 
  ggtitle("Scatterplot of Global TB Mortality Per 100000 and Country Level Malnutrition Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Malnutrition Prevalence") + 
  ylab("TB Mortality Per 100000")

#Case fatality ratio 

tbdata_2019_new %>% 
  ggplot (aes(x= DM_prev, y=case_fatality)) + 
  ggtitle("Scatterplot of Global TB Case Fatality Ratio and Country Level Diabetes Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Diabetes Prevalence") + 
  ylab("TB Case Fatality Ratio")

tbdata_2019_new %>% 
  filter(!grepl('Myanmar', country)) %>%
  ggplot (aes(x= income_cap, y=case_fatality)) + 
  ggtitle("Scatterplot of Global TB Case Fatality Ratio and Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Adjusted Per Capita Income") + 
  ylab("TB Case Fatality Ratio")

tbdata_2019_new %>% 
  filter(!grepl('Myanmar', country)) %>%
  ggplot (aes(x= `log_income <- log(income_cap)`, y=case_fatality)) + 
  ggtitle("Log AdjustedScatterplot of Global TB Case Fatality Ratio and Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted Per Capita Income") + 
  ylab("TB Case Fatality Ratio")

tbdata_2019_new %>% 
  ggplot (aes(x= HIV_inc, y=case_fatality)) + 
  ggtitle("Scatterplot of Global TB Case Fatality Ratio and HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("HIV Incidence Per 100000") + 
  ylab("TB Case Fatality Ratio")

tbdata_2019_new %>% 
  ggplot (aes(x= HIV_inc), y=case_fatality) + 
  ggtitle("Log Adjusted Scatterplot of Global TB Case Fatality Ratio and HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted HIV Incidence Per 100000") + 
  ylab("TB Case Fatality Ratio")

tbdata_2019_new %>% 
  ggplot(aes(x=HIV_inc, y=case_fatality)) + 
  ggtitle("Scatterplot of Global TB Case Fatality Ratio and HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("HIV Incidence Per 100000") + 
  ylab("TB Case Fatality Ratio")

tbdata_2019_new %>% 
  ggplot (aes(x= `Malnutrition_prev `, y=case_fatality)) + 
  ggtitle("Scatterplot of Global TB Case Fatality Ratio and Country Level Malnutrition Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Malnutrition Prevalence") + 
  ylab("TB Case Fatality Ratio")

#Globe RR-TB 

tbdata_2019_new %>% 
  ggplot (aes(x= DM_prev, y=e_rr_inc_100k)) + 
  ggtitle("Scatterplot of Global Rifampicin Resistant TB Incidence Per 100000 and Country Level Diabetes Prevalence") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Diabetes Prevalence") + 
  ylab("RR-TB Incidence Per 100000")

tbdata_2019_new %>% 
  ggplot (aes(x= DM_prev, y=e_rr_inc_100k)) + 
  ggtitle("Scatterplot of Global Rifampicin Resistant TB Incidence Per 100000 and Country Level Diabetes Prevalence") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Diabetes Prevalence") + 
  ylab("RR-TB Incidence Per 100000")

tbdata_2019_new %>% 
  filter(!grepl('Myanmar', country)) %>%
  ggplot (aes(x= income_cap, y=e_rr_inc_100k)) + 
  ggtitle("Scatterplot of Global Rifampicin Resistant TB Incidence Per 100000 and Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Adjusted Per Capita Income") + 
  ylab("RR-TB Incidence Per 100000")

tbdata_2019_new %>% 
  filter(!grepl('Myanmar', country)) %>%
  ggplot (aes(x= `log_income <- log(income_cap)`, y=e_rr_inc_100k)) + 
  ggtitle("Log Adjusted Scatterplot of Global Rifampicin Resistant TB Incidence Per 100000 and Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted Per Capita Income") + 
  ylab("RR-TB Incidence Per 100000")

tbdata_2019_new %>% 
  ggplot (aes(x= HIV_inc, y=e_rr_inc_100k)) + 
  ggtitle("Scatterplot of Global Rifampicin Resistant TB Incidence Per 100000 and HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("HIV Incidence Per 100000") + 
  ylab("RR-TB Incidence Per 100000")

tbdata_2019_new %>% 
  ggplot (aes(x= `log_HIV <- log(HIV_inc)`, y=e_rr_inc_100k)) + 
  ggtitle("Log Adjusted Scatterplot of Global Rifampicin Resistant TB Incidence Per 100000 and HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted HIV Incidence Per 100000") + 
  ylab("RR-TB Incidence Per 100000")

tbdata_2019_new %>% 
  ggplot (aes(x= `Malnutrition_prev `, y=e_rr_inc_100k)) + 
  ggtitle("Scatterplot of Global Rifampicin Resistant TB Incidence Per 100000 and Country Malnutrition Prevalence") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Malnutrition Prevalence") + 
  ylab("RR-TB Incidence Per 100000")

#Africa Region 

tbdata_AFR %>% 
  ggplot (aes(x= DM_prev, y=e_inc_100k)) + 
  ggtitle("Scatterplot of African Region TB Incidence Per 100000 and Diabetes Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Diabetes Prevalence") + 
  ylab("TB Incidence Per 100000")

tbdata_AFR %>% 
  ggplot (aes(x= log_income, y=e_inc_100k)) + 
  ggtitle("Scatterplot of African Region TB Incidence Per 100000 and Log Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted Per Capita Income") + 
  ylab("TB Incidence Per 100000")

tbdata_AFR %>% 
  ggplot (aes(x= HIV_inc, y=e_inc_100k)) + 
  ggtitle("Scatterplot of African Region TB Incidence Per 100000 and HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("HIV Incidence Per 100000") + 
  ylab("TB Incidence Per 100000")

tbdata_AFR %>% 
  ggplot (aes(x= `Malnutrition_prev `, y=e_inc_100k)) + 
  ggtitle("Scatterplot of African Region TB Incidence Per 100000 and Country Malnutrition Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Malnutrition Prevalence") + 
  ylab("TB Incidence Per 100000")


#AFR Mortality 
tbdata_AFR %>% 
  ggplot (aes(x= DM_prev, y=e_mort_100k)) + 
  ggtitle("Scatterplot of African Region TB Mortality Per 100000 and Country Diabetes Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Diabetes Prevalence") + 
  ylab("TB Mortality Per 100000")

tbdata_AFR %>% 
  ggplot (aes(x= log_income, y=e_mort_100k)) + 
  ggtitle("Scatterplot of African Region TB Mortality Per 100000 and Log Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted Per Capita Income") + 
  ylab("TB Mortality Per 100000")

tbdata_AFR %>% 
  ggplot (aes(x= HIV_inc, y=e_mort_100k)) + 
  ggtitle("Scatterplot of African Region TB Mortality Per 100000 and HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("HIV Incidence Per 100000") + 
  ylab("TB Mortality Per 100000")

tbdata_AFR %>% 
  ggplot (aes(x= `Malnutrition_prev `, y=e_mort_100k)) + 
  ggtitle("Scatterplot of African Region TB Mortality Per 100000 and Country Malnutrition Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Malnutrition Prevalence") + 
  ylab("TB Mortality Per 100000")


tbdata_AFR %>% 
  ggplot (aes(x= DM_prev, y=case_fatality)) + 
  ggtitle("Scatterplot of African Region TB Case Fatality Ratio and Country Diabetes Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Diabetes Prevalence") + 
  ylab("TB Case Fatality Ratio")

tbdata_AFR %>% 
  ggplot (aes(x= log_income, y=case_fatality)) + 
  ggtitle("Scatterplot of African Region TB Case Fatality Ratio and Log Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted Per Capita Income") + 
  ylab("TB Case Fatality Ratio")

tbdata_AFR %>% 
  ggplot (aes(x= HIV_inc, y=case_fatality)) + 
  ggtitle("Scatterplot of African Region TB Case Fatality Ratio and HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("HIV Incidence Per 100000") + 
  ylab("TB Case Fatality Ratio")

tbdata_AFR %>% 
  ggplot (aes(x= `Malnutrition_prev `, y=case_fatality)) + 
  ggtitle("Scatterplot of African Region TB Case Fatality Ratio and Country Malnutrition Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Malnutrition Prevalence") + 
  ylab("TB Case Fatality Ratio")

tbdata_AFR %>% 
  ggplot (aes(x= DM_prev, y=e_rr_inc_100k)) + 
  ggtitle("Scatterplot of African Region Rifampicin Resistant TB Incidence Per 100000 and Country Diabetes Prevalence") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Diabetes Prevalence") + 
  ylab("Rifampicin Resistant TB Incidence Per 100000")

#Making a decision here to focus on only TB incidence, mortality, and CFR. Just looking at the data 
#I don't think any of my predictors are good predictors of RR-TB and there is just not enough robust 
#longitudinal data on RR-TB unfortunately. 

tbdata_AMR %>% 
  ggplot (aes(x= DM_prev, y=e_inc_100k)) + 
  ggtitle("Scatterplot of Americas Region TB Incidence Per 100000 and Country Diabetes Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Diabetes Prevalence") + 
  ylab("TB Incidence Per 100000")

tbdata_AMR %>% 
  ggplot (aes(x= income_cap, y=e_inc_100k)) + 
  ggtitle("Scatterplot of Americas Region TB Incidence Per 100000 and Adjusted Income per Capita") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Adjusted Income Per Capita") + 
  ylab("TB Incidence Per 100000")

tbdata_AMR %>% 
  ggplot (aes(x= log_income_AMR, y=e_inc_100k)) + 
  ggtitle("Scatterplot of Americas Region TB Incidence Per 100000 and Log Adjusted Income per Capita") +
  theme(plot.title = element_text(hjust = 0.5, size = 8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted Income Per Capita") + 
  ylab("TB Incidence Per 100000")

tbdata_AMR %>% 
  ggplot (aes(x= HIV_inc, y=e_inc_100k)) + 
  ggtitle("Scatterplot of Americas Region TB Incidence Per 100000 and HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("HIV Incidence Per 100000") + 
  ylab("TB Incidence Per 100000")

tbdata_AMR %>% 
  ggplot (aes(x= `Malnutrition_prev `, y=e_inc_100k)) + 
  ggtitle("Scatterplot of Americas Region TB Incidence Per 100000 and Country Malnutrition Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Malnutrition Prevalence") + 
  ylab("TB Incidence Per 100000")

#AMR Mortality 

tbdata_AMR %>% 
  ggplot (aes(x= DM_prev, y=e_mort_100k)) + 
  ggtitle("Scatterplot of Americas Region TB Mortality Per 100000 and Country Diabetes Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Diabetes Prevalence") + 
  ylab("TB Mortality Per 100000")

tbdata_AMR %>% 
  ggplot (aes(x= log_income_AMR, y=e_mort_100k)) + 
  ggtitle("Scatterplot of Americas Region TB Mortality Per 100000 and Log Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted Per Capita Income") + 
  ylab("TB Mortality Per 100000")

tbdata_AMR %>% 
  ggplot (aes(x= log_income_AMR, y=e_mort_100k)) + 
  ggtitle("Log Adjusted Scatterplot of Americas Region TB Mortality Per 100000 and Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted Per Capita Income") + 
  ylab("TB Mortality Per 100000")

tbdata_AMR %>% 
  ggplot (aes(x= HIV_inc, y=e_mort_100k)) + 
  ggtitle("Scatterplot of Americas Region TB Mortality Per 100000 and HIV Incidence per 100000") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("HIV Incidence per 100000") + 
  ylab("TB Mortality Per 100000")

tbdata_AMR %>% 
  ggplot (aes(x= `Malnutrition_prev `, y=e_mort_100k)) + 
  ggtitle("Scatterplot of Americas Region TB Mortality Per 100000 and Malnutrition Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Malnutrition Prevalence") + 
  ylab("TB Mortality Per 100000")

tbdata_AMR %>% 
  ggplot (aes(x= DM_prev, y=case_fatality)) + 
  ggtitle("Scatterplot of Americas Region TB Case Fatality Ratio and Country Diabetes Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Diabetes Prevalence") + 
  ylab("TB Case Fatality Ratio")

tbdata_AMR %>% 
  ggplot (aes(x= log_income_AMR, y=case_fatality)) + 
  ggtitle("Scatterplot of Americas Region TB Case Fatality Ratio and Log Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted Per Capita Income") + 
  ylab("TB Case Fatality Ratio")

tbdata_AMR %>% 
  ggplot (aes(x= log_income_AMR, y=case_fatality)) + 
  ggtitle("Log Adjusted Scatterplot of Americas Region TB Case Fatality Ratio and Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted Per Capita Income") + 
  ylab("TB Case Fatality Ratio")

tbdata_AMR %>% 
  ggplot (aes(x= HIV_inc, y=case_fatality)) + 
  ggtitle("Scatterplot of Americas Region TB Case Fatality Ratio and HIV Incidence per 100000") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("HIV Incidence per 100000") + 
  ylab("TB Case Fatality Ratio")

tbdata_AMR %>% 
  ggplot (aes(x= `Malnutrition_prev `, y=case_fatality)) + 
  ggtitle("Scatterplot of Americas Region TB Case Fatality Ratio and Malnutrition Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Malnutrition Prevalence") + 
  ylab("TB Case Fatality Ratio")

#Eastern Mediterranean

tbdata_EMR %>% 
  ggplot (aes(x= DM_prev, y=e_inc_100k)) + 
  ggtitle("Scatterplot of Eastern Mediterranean Region TB Incidence Per 100000 and Country Diabetes Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=7)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Diabetes Prevalence") + 
  ylab("TB Incidence Per 100000")

tbdata_EMR %>% 
  ggplot (aes(x= income_cap, y=e_inc_100k)) + 
  ggtitle("Scatterplot of Eastern Mediterranean Region TB Incidence Per 100000 and Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Adjusted Per Capita Income") + 
  ylab("TB Incidence Per 100000")

tbdata_EMR %>% 
  ggplot (aes(x= log_income_EMR, y=e_inc_100k)) + 
  ggtitle("Scatterplot of Eastern Mediterranean Region TB Incidence Per 100000 and Log Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5, size=7)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted Per Capita Income") + 
  ylab("TB Incidence Per 100000")

tbdata_EMR %>% 
  ggplot (aes(x= HIV_inc, y=e_inc_100k)) + 
  ggtitle("Scatterplot of Eastern Mediterranean Region TB Incidence Per 100000 and HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5, size=7)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("HIV Incidence per 100000") + 
  ylab("TB Incidence Per 100000")

tbdata_EMR %>% 
  ggplot (aes(x= log_HIV_EMR, y=e_inc_100k)) + 
  ggtitle("Scatterplot of Eastern Mediterranean Region TB Incidence Per 100000 and Log Adjusted HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5, size=7)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted HIV Incidence per 100000") + 
  ylab("TB Incidence Per 100000")

tbdata_EMR %>% 
  ggplot (aes(x= `Malnutrition_prev `, y=e_inc_100k)) + 
  ggtitle("Scatterplot of Eastern Mediterranean Region TB Incidence Per 100000 and Malnutrition Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=7)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Malnutrition Prevalence") + 
  ylab("TB Incidence Per 100000")

tbdata_EMR %>% 
  ggplot (aes(x= DM_prev, y=e_mort_100k)) + 
  ggtitle("Scatterplot of Eastern Mediterranean Region TB Mortality Per 100000 and Country Diabetes Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=7)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Diabetes Prevalence") + 
  ylab("TB Mortality Per 100000")

tbdata_EMR %>% 
  ggplot (aes(x= income_cap, y=e_mort_100k)) + 
  ggtitle("Scatterplot of Eastern Mediterranean Region TB Mortality Per 100000 and Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5, size=7)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Adjusted Per Capita Income") + 
  ylab("TB Mortality Per 100000")

tbdata_EMR %>% 
  ggplot (aes(x= log_income_EMR, y=e_mort_100k)) + 
  ggtitle("Scatterplot of Eastern Mediterranean Region TB Mortality Per 100000 and Log Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5, size=7)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted Per Capita Income") + 
  ylab("TB Mortality Per 100000")

tbdata_EMR %>% 
  ggplot (aes(x= HIV_inc, y=e_mort_100k)) + 
  ggtitle("Scatterplot of Eastern Mediterranean Region TB Mortality Per 100000 and HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5, size=7)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("HIV Incidence Per 100000") + 
  ylab("TB Mortality Per 100000")

tbdata_EMR %>% 
  ggplot (aes(x= log_HIV_EMR, y=e_mort_100k)) + 
  ggtitle("Scatterplot of Eastern Mediterranean Region TB Mortality Per 100000 and Log Adjusted HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5, size=6)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted HIV Incidence Per 100000") + 
  ylab("TB Mortality Per 100000")

tbdata_EMR %>% 
  ggplot (aes(x= `Malnutrition_prev `, y=e_mort_100k)) + 
  ggtitle("Scatterplot of Eastern Mediterranean Region TB Mortality Per 100000 and Malnutrition Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=7)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Malnutrition Prevalence") + 
  ylab("TB Mortality Per 100000")


tbdata_EMR %>% 
  ggplot (aes(x= DM_prev, y=case_fatality)) + 
  ggtitle("Scatterplot of Eastern Mediterranean Region TB Case Fatality Ratio and Diabetes Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=7)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Diabetes Prevalence") + 
  ylab("TB Case Fatality Ratio")

tbdata_EMR %>% 
  ggplot (aes(x= income_cap, y=case_fatality)) + 
  ggtitle("Scatterplot of Eastern Mediterranean Region TB Case Fatality Ratio and Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5, size=7)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Adjusted Per Capita Income") + 
  ylab("TB Case Fatality Ratio")

tbdata_EMR %>% 
  ggplot (aes(x= log_income_EMR, y=case_fatality)) + 
  ggtitle("Scatterplot of Eastern Mediterranean Region TB Case Fatality Ratio and Log Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5, size=7)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted Per Capita Income") + 
  ylab("TB Case Fatality Ratio")

tbdata_EMR %>% 
  ggplot (aes(x= HIV_inc, y=case_fatality)) + 
  ggtitle("Scatterplot of Eastern Mediterranean Region TB Case Fatality Ratio and HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("HIV Incidence Per 100000") + 
  ylab("TB Case Fatality Ratio")

tbdata_EMR %>% 
  ggplot (aes(x= log_HIV_EMR, y=case_fatality)) + 
  ggtitle("Scatterplot of Eastern Mediterranean Region TB Case Fatality Ratio and Log Adjusted HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5, size=7)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted HIV Incidence Per 100000") + 
  ylab("TB Case Fatality Ratio")

tbdata_EMR %>% 
  ggplot (aes(x= `Malnutrition_prev `, y=case_fatality)) + 
  ggtitle("Scatterplot of Eastern Mediterranean Region TB Case Fatality Ratio and Malnutrition Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=7)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Malnutrition Prevalence") + 
  ylab("TB Case Fatality Ratio")

#European Region

tbdata_EUR %>% 
  ggplot (aes(x= DM_prev, y=e_inc_100k)) + 
  ggtitle("Scatterplot of European Region TB Incidence Per 100000 and Country Diabetes Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Diabetes Prevalence") + 
  ylab("TB Incidence Per 100000")

tbdata_EUR %>% 
  ggplot (aes(x= income_cap, y=e_inc_100k)) + 
  ggtitle("Scatterplot of European Region TB Incidence Per 100000 and Adjusted Income Per Capita") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Adjusted Income Per Capita") + 
  ylab("TB Incidence Per 100000")

tbdata_EUR %>% 
  mutate(log_income_EUR = log(income_cap)) %>% 
  ggplot (aes(x= log_income_EUR, y=e_inc_100k)) + 
  ggtitle("Scatterplot of European Region TB Incidence Per 100000 and Log Adjusted Income Per Capita") +
  theme(plot.title = element_text(hjust = 0.5, size=7)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted Income Per Capita") + 
  ylab("TB Incidence Per 100000")

tbdata_EUR %>% 
  ggplot (aes(x= HIV_inc, y=e_inc_100k)) + 
  ggtitle("Scatterplot of European Region TB Incidence Per 100000 and HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5, size=7)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("HIV Incidence Per 100000") + 
  ylab("TB Incidence Per 100000")

tbdata_EUR %>% 
  ggplot (aes(x= `Malnutrition_prev `, y=e_inc_100k)) + 
  ggtitle("Scatterplot of European Region TB Incidence Per 100000 and Malnutrition Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=7)) +
  geom_point(size=0.5) + geom_smooth(method=loess) + xlab("Malnutrition Prevalence") + 
  ylab("TB Incidence Per 100000")


tbdata_EUR %>% 
  ggplot (aes(x= DM_prev, y=e_mort_100k)) + 
  ggtitle("Scatterplot of European Region TB Mortality Per 100000 and Country Diabetes Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method=loess) + xlab("Diabetes Prevalence") + 
  ylab("TB Mortality Per 100000")

tbdata_EUR %>% 
  ggplot (aes(x= log_income, y=e_mort_100k)) + 
  ggtitle("Scatterplot of European Region TB Mortality Per 100000 and Log Adjusted Income Per Capita") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method=loess) + xlab("Log Adjusted Income Per Capita") + 
  ylab("TB Mortality Per 100000")

tbdata_EUR %>% 
  ggplot (aes(x= HIV_inc, y=e_mort_100k)) + 
  ggtitle("Scatterplot of European Region TB Mortality Per 100000 and HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method=loess) + xlab("HIV Incidence Per 100000") + 
  ylab("TB Mortality Per 100000")

tbdata_EUR %>% 
  ggplot (aes(x= `Malnutrition_prev `, y=e_mort_100k)) + 
  ggtitle("Scatterplot of European Region TB Mortality Per 100000 and Malnutrition Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method=loess) + xlab("Malnutrition Prevalence") + 
  ylab("TB Mortality Per 100000")

tbdata_EUR %>% 
  ggplot (aes(x= DM_prev, y=case_fatality)) + 
  ggtitle("Scatterplot of European Region TB Case Fatality Ratio and Diabetes Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method=loess) + xlab("Diabetes Prevalence") + 
  ylab("TB Case Fatality Ratio")

tbdata_EUR %>% 
  ggplot (aes(x= log_income, y=case_fatality)) + 
  ggtitle("Scatterplot of European Region TB Case Fatality Ratio and Log Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method=loess) + xlab("Log Adjusted Per Capita Income") + 
  ylab("TB Case Fatality Ratio")

tbdata_EUR %>% 
  ggplot (aes(x= HIV_inc, y=case_fatality)) + 
  ggtitle("Scatterplot of European Region TB Case Fatality Ratio and HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method=loess) + xlab("HIV Incidence Per 100000") + 
  ylab("TB Case Fatality Ratio")

tbdata_EUR %>% 
  ggplot (aes(x= `Malnutrition_prev `, y=case_fatality)) + 
  ggtitle("Scatterplot of European Region TB Case Fatality Ratio and Malnutrition Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method=loess) + xlab("Malnutrition Prevalence") + 
  ylab("TB Case Fatality Ratio")

#South East Asia

tbdata_SEA %>% 
  ggplot (aes(x= DM_prev, y=e_inc_100k)) + 
  ggtitle("Scatterplot of South East Asian Region TB Incidence Per 100000 and Diabetes Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Diabetes Prevalence") + 
  ylab("TB Incidence Per 100000")

tbdata_SEA %>% 
  filter(!grepl('Myanmar', country)) %>%
  ggplot (aes(x= log_income, y=e_inc_100k)) + 
  ggtitle("Scatterplot of South East Asian Region TB Incidence Per 100000 and Log Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5, size=7)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted Per Capita Income") + 
  ylab("TB Incidence Per 100000")

tbdata_SEA %>% 
  ggplot (aes(x= HIV_inc, y=e_inc_100k)) + 
  ggtitle("Scatterplot of South East Asian Region TB Incidence Per 100000 and HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("HIV Incidence Per 100000") + 
  ylab("TB Incidence Per 100000")

tbdata_SEA %>% 
  ggplot (aes(x= `Malnutrition_prev `, y=e_inc_100k)) + 
  ggtitle("Scatterplot of South East Asian Region TB Incidence Per 100000 and Malnutrition Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Malnutrition Prevalence") + 
  ylab("TB Incidence Per 100000")


tbdata_SEA %>% 
  ggplot (aes(x= DM_prev, y=e_mort_100k)) + 
  ggtitle("Scatterplot of South East Asian Region TB Mortality Per 100000 and Diabetes Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Diabetes Prevalence") + 
  ylab("TB Mortality Per 100000")

tbdata_SEA %>% 
  filter(!grepl('Myanmar', country)) %>%
  ggplot (aes(x= log_income, y=e_mort_100k)) + 
  ggtitle("Scatterplot of South East Asian Region TB Mortality Per 100000 and Log Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5, size=7)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted Per Capita Income") + 
  ylab("TB Mortality Per 100000")

tbdata_SEA %>% 
  ggplot (aes(x= HIV_inc, y=e_mort_100k)) + 
  ggtitle("Scatterplot of South East Asian Region TB Mortality Per 100000 and HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("HIV Incidence Per 100000") + 
  ylab("TB Mortality Per 100000")

tbdata_SEA %>% 
  ggplot (aes(x= `Malnutrition_prev `, y=e_mort_100k)) + 
  ggtitle("Scatterplot of South East Asian Region TB Mortality Per 100000 and Malnutrition Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Malnutrition Prevalence") + 
  ylab("TB Mortality Per 100000")

tbdata_SEA %>% 
  ggplot (aes(x= DM_prev, y=case_fatality)) + 
  ggtitle("Scatterplot of South East Asian Region TB Case Fatality Ratio and Diabetes Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Diabetes Prevalence") + 
  ylab("TB Case Fatality Ratio")

tbdata_SEA %>% 
  filter(!grepl('Myanmar', country)) %>%
  ggplot (aes(x= log_income, y=case_fatality)) + 
  ggtitle("Scatterplot of South East Asian Region TB Case Fatality Ratio and Log Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted Per Capita Income") + 
  ylab("TB Case Fatality Ratio")

tbdata_SEA %>%
  ggplot (aes(x= HIV_inc, y=case_fatality)) + 
  ggtitle("Scatterplot of South East Asian Region TB Case Fatality Ratio and HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("HIV Incidence Per 100000") + 
  ylab("TB Case Fatality Ratio")

tbdata_SEA %>%
  ggplot (aes(x= `Malnutrition_prev `, y=case_fatality)) + 
  ggtitle("Scatterplot of South East Asian Region TB Case Fatality Ratio and Malnutrition Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Malnutrition Prevalence") + 
  ylab("TB Case Fatality Ratio")

#Western Pacific Region

tbdata_WPR %>% 
  ggplot (aes(x= DM_prev, y=e_inc_100k)) + 
  ggtitle("Scatterplot of Western Pacific Region TB Incidence Per 100000 and Diabetes Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Diabetes Prevalence") + 
  ylab("TB Incidence Per 100000")

tbdata_WPR %>% 
  ggplot (aes(x= log_income, y=e_inc_100k)) + 
  ggtitle("Scatterplot of Western Pacific Region TB Incidence Per 100000 and Log Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5, size=7)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted Per Capita Income") + 
  ylab("TB Incidence Per 100000")


tbdata_WPR %>% 
  ggplot (aes(x= HIV_inc, y=e_inc_100k)) + 
  ggtitle("Scatterplot of Western Pacific Region TB Incidence Per 100000 and HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("HIV Incidence Per 100000") + 
  ylab("TB Incidence Per 100000")

tbdata_WPR %>% 
  ggplot (aes(x= `Malnutrition_prev `, y=e_inc_100k)) + 
  ggtitle("Scatterplot of Western Pacific Region TB Incidence Per 100000 and Malnutrition Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Malnutrition Prevalence") + 
  ylab("TB Incidence Per 100000")

tbdata_WPR %>% 
  ggplot (aes(x= DM_prev, y=e_mort_100k)) + 
  ggtitle("Scatterplot of Western Pacific Region TB Mortality Per 100000 and Diabetes Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Diabetes Prevalence") + 
  ylab("TB Mortality  Per 100000")

tbdata_WPR %>% 
  ggplot (aes(x= log_income, y=e_mort_100k)) + 
  ggtitle("Scatterplot of Western Pacific Region TB Mortality Per 100000 and Log Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5, size=7)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted Per Capita Income") + 
  ylab("TB Mortality  Per 100000")

tbdata_WPR %>% 
  ggplot (aes(x= HIV_inc, y=e_mort_100k)) + 
  ggtitle("Scatterplot of Western Pacific Region TB Mortality Per 100000 and HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("HIV Incidence Per 100000") + 
  ylab("TB Mortality  Per 100000")


tbdata_WPR %>% 
  ggplot (aes(x= `Malnutrition_prev `, y=e_mort_100k)) + 
  ggtitle("Scatterplot of Western Pacific Region TB Mortality Per 100000 and Malnutrition Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Malnutrition Prevalence") + 
  ylab("TB Mortality  Per 100000")



tbdata_WPR %>% 
  ggplot (aes(x= DM_prev, y=case_fatality)) + 
  ggtitle("Scatterplot of Western Pacific Region TB Case Fatality and Diabetes Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Diabetes Prevalence") + 
  ylab("TB Case Fatality")

tbdata_WPR %>% 
  ggplot (aes(x= log_income, y=case_fatality)) + 
  ggtitle("Scatterplot of Western Pacific Region TB Case Fatality and Log Adjusted Per Capita Income") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Log Adjusted Per Capita Income") + 
  ylab("TB Case Fatality")

tbdata_WPR %>% 
  ggplot (aes(x= HIV_inc, y=case_fatality)) + 
  ggtitle("Scatterplot of Western Pacific Region TB Case Fatality and HIV Incidence Per 100000") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("HIV Incidence Per 100000") + 
  ylab("TB Case Fatality")

tbdata_WPR %>% 
  ggplot (aes(x= `Malnutrition_prev `, y=case_fatality)) + 
  ggtitle("Scatterplot of Western Pacific Region TB Case Fatality and Malnutrition Prevalence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Malnutrition Prevalence") + 
  ylab("TB Case Fatality")

tbdata_new %>% 
  ggplot (aes(x= mal, y=e_inc_100k)) + 
  ggtitle("Scatterplot of Malnutrition Prevlanece and TB Incidence") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Malnutrition Prevalence") + 
  ylab("TB Incidence Per 100,000")

tbdata_new %>% 
  ggplot (aes(x= DM_prev, y=e_inc_100k)) + 
  ggtitle("Scatterplot of Diabetes Prevalence and TB Incidence") +
  theme(plot.title = element_text(hjust = 0.5, size=20)) +
  geom_point(size=0.5) + geom_smooth(method = loess) + xlab("Diabetes Prevalence") + 
  ylab("TB Incidence Per 100,000")


#Starting the models. First starting w/ univariate globe models

install.packages("performance")
install.packages("lmerTest")
library(lmerTest)
library(performance)
library(MASS)
install.packages("lme4")
library(lme4)
install.packages("secr")
library(secr)
install.packages("MuMIn")
library(MuMIn)

#Doing logit transformation to CFR so its no longer bound by 0-1 and therefore won't have a beta distribution
tbdata_2019_new <- tbdata_2019_new %>% 
  mutate(log_inc = log(e_inc_100k)) %>% 
  mutate(log_DM = log(DM_prev)) %>% 
  mutate(log_mal = log(`Malnutrition_prev `)) %>% 
  mutate(log_HIV = log(HIV_inc)) %>% 
  mutate (log_mort = log(e_mort_100k)) %>% 
  mutate(logit_CFR = logit(case_fatality))

summary(tbdata_2019_new$logit_CFR)
tbdata_2019_new %>% 
  ggplot(aes(x=logit_CFR))+ ggtitle("Log Transformed Global TB Mortality Per 100,000")+
  geom_histogram(bins = 10, fill="darkblue", color="black") +
  xlab("Log Transformed TB Mortality Per 100000")


globe_inc_DM <- lmer(log_inc ~ DM_prev + (1|g_whoregion/country), data=tbdata_2019_new, REML = FALSE)
summary(globe_inc_DM)
check_model(globe_inc_DM)

tbglobe_nomyan <- tbdata_2019_new %>%  filter(!grepl('Myanmar', country))
  

globe_inc_income <- lmer(log_inc ~ log_income + (1|g_whoregion/country), data=tbglobe_nomyan, 
                         REML = FALSE)

summary(globe_inc_income)
check_model(globe_inc_income)


globe_inc_HIV <- lmer(log_inc ~ log_HIV + (1|g_whoregion/country), data=tbdata_2019_new, 
                      REML = FALSE)
summary(globe_inc_HIV)
check_model(globe_inc_HIV)



globe_inc_mal <- lmer(log_inc ~  + `Malnutrition_prev ` + (1|g_whoregion/country), data=tbdata_2019_new, 
                       REML = FALSE)
summary(globe_inc_mal)
check_model(globe_inc_mal)

#Retrying my model building with new data set with more variables. Started by including all my variables of interest and now
#I am taking non-significant variables off. 

#Global models here
summary(tbdata_new$log_in)
globe_inc_multi <- lmer(log_inc ~  alc_tot + air_pol +  
                          pop_den + Age_15_24 + Age_25_64 + Age_65_Over + HIV_inc_100k +
                          +  log_income +  c_cdr
                        + (1|g_whoregion/country), data=tbdata_new, REML = FALSE)
summary(globe_inc_multi)
check_model(globe_inc_multi)


globe_mort_multi <- lmer(log_mort ~ alc_tot + air_pol + daily_smoke + pop_den +
                          Age_15_24 + Age_25_64 + Age_65_Over +  HIV_inc_100k + log_income +
                           mal + DM_prev + c_cdr 
                         + (1|g_whoregion/country), data=tbdata_new, REML = FALSE)
summary(globe_mort_multi)
check_model(globe_mort_multi)

r2_nakagawa(globe_inc_multi)
r2_nakagawa(globe_mort_multi)

r2_nakagawa(lowincome_inc_multi)
r2_nakagawa(lowincome_mort_multi)

r2_nakagawa(highincome_inc_multi)
r2_nakagawa(highincome_mort_multi)
#African models here


AFR_inc_multi <- lmer(log_inc ~ 
                        pop_den + Age_15_24 + Age_25_64 + Age_65_Over +
                        HIV_inc_100k + log_income + log_budget + c_cdr + DM_prev + mal +
                        (1|country), data=tbdata_AFR_new, REML = FALSE)
summary(AFR_inc_multi)
check_model(AFR_inc_multi)


AFR_mort_multi <- lmer(log_mort ~ alc_tot + air_pol + 
                         daily_smoke +  Age_15_24 + Age_25_64 + Age_65_Over +
                         HIV_inc_100k +  log_budget + mal +c_cdr +
                         (1|country), data=tbdata_AFR_new, REML = FALSE)
summary(AFR_mort_multi)
check_model(AFR_mort_multi)


#American models 

AMR_inc_multi <- lmer(log_inc ~ unemploy + 
                        pop_den + Age_15_24 + Age_25_64 + Age_65_Over +
                          mal + c_cdr + log_budget + HIV_inc_100k +
                        (1|country), data=tbdata_AMR_new, REML = FALSE)

summary(AMR_inc_multi)
check_model(AMR_inc_multi)



AMR_mort_multi <- lmer(log_mort ~ 
                         daily_smoke + pop_den + Age_15_24 + Age_25_64 + Age_65_Over +
                         HIV_inc_100k + log_budget + mal + 
                         (1|country), data=tbdata_AMR_new, REML = FALSE)
summary(AMR_mort_multi)
check_model(AMR_mort_multi)



#Eastern med models 
EMR_inc_multi <- lmer(log_inc ~ unemploy + air_pol + 
                        daily_smoke +  Age_15_24 + Age_25_64 + Age_65_Over +
                        HIV_inc_100k +  log_income +
                        (1|country), data=tbdata_EMR_new, REML = FALSE)
summary(EMR_inc_multi)
check_model(EMR_inc_multi)




EMR_mort_multi <- lmer(log_mort ~  
                         daily_smoke +  Age_15_24 + Age_25_64 + Age_65_Over +
                         HIV_inc_100k + log_budget + c_cdr + mal +
                         (1|country), data=tbdata_EMR_new, REML = FALSE)
summary(EMR_mort_multi)
check_model(EMR_mort_multi)


#European Models 

EUR_inc_multi <- lmer(log_inc ~ alc_tot + 
                         pop_den + Age_15_24 + Age_25_64 + Age_65_Over +
                         log_income + c_cdr + HIV_inc_100k +
                        (1|country), data=tbdata_EUR_new, REML = FALSE)
summary(EUR_inc_multi)
check_model(EUR_inc_multi)



  EUR_mort_multi <- lmer(log_mort ~  unemploy + alc_tot + air_pol + 
                            pop_den + Age_15_24 + Age_25_64 + Age_65_Over +
                            log_budget + mal + c_cdr + DM_prev + HIV_inc_100k +
                           (1|country), data=tbdata_EUR_new, REML = FALSE)
summary(EUR_mort_multi)
check_model(EUR_mort_multi)

#South east asia models 

SEA_inc_multi <- lmer(log_inc ~ 
                         pop_den + Age_15_24 + Age_25_64 + Age_65_Over +
                        HIV_inc_100k +  log_income +  
                        (1|country), data=tbdata_SEA_new, REML = FALSE)
summary(SEA_inc_multi)
check_model(SEA_inc_multi)

summary(tbdata_SEA_new)

SEA_mort_multi <- lmer(log_mort ~  alc_tot + air_pol + 
                         daily_smoke + Age_15_24 + Age_25_64 + Age_65_Over +
                         HIV_inc_100k + log_budget +  c_cdr +
                         (1|country), data=tbdata_SEA_new, REML = FALSE)
summary(SEA_mort_multi)
check_model(SEA_mort_multi)

#WPR models 
WPR_inc_multi <- lmer(log_inc ~   alc_tot + Age_15_24 + Age_25_64 + Age_65_Over +
                         log_budget +   mal + c_cdr + HIV_inc_100k +
                        (1|country),  data=tbdata_WPR_new, REML = FALSE)
summary(WPR_inc_multi)
check_model(WPR_inc_multi)

WPR_mort_multi  <- lmer(log_mort ~ 
                          
                          daily_smoke +  Age_15_24 + Age_25_64 + Age_65_Over +
                          HIV_inc_100k +  log_budget + mal +  c_cdr +
                          (1|country), data=tbdata_WPR_new, REML = FALSE)
summary(WPR_mort_multi)
check_model(WPR_mort_multi)


lowincome_inc_multi <- lmer(log_inc ~  alc_tot + air_pol + 
                          daily_smoke +  Age_15_24 + Age_25_64 + Age_65_Over +
                          HIV_inc_100k + log_income +  DM_prev + c_cdr + 
                          (1|country), data=tbdata_lowincome, REML = FALSE)

summary(lowincome_inc_multi)
check_model(lowincome_inc_multi)

lowincome_mort_multi <- lmer(log_mort ~  alc_tot + air_pol + 
                               daily_smoke + pop_den + Age_15_24 + Age_25_64 + Age_65_Over +
                               HIV_inc_100k + log_income +  mal +  c_cdr + 
                               (1|country), data=tbdata_lowincome, REML = FALSE)

summary(lowincome_mort_multi)
check_model(lowincome_mort_multi)

highincome_mort_multi <- lmer(log_mort ~  alc_tot +  
                               daily_smoke + pop_den + Age_15_24 + Age_25_64 + Age_65_Over +
                               HIV_inc_100k +  log_budget +  DM_prev + 
                               (1|country), data=tbdata_highincome, REML = FALSE)

summary(highincome_mort_multi)
check_model(highincome_mort_multi)


highincome_inc_multi <- lmer(log_inc ~   alc_tot + 
                                 Age_15_24 + Age_25_64 + Age_65_Over +
                               HIV_inc_100k + log_budget + DM_prev + 
                                (1|country), data=tbdata_highincome, REML = FALSE)
summary(highincome_inc_multi)
check_model(highincome_inc_multi)

unemploy + alc_tot + air_pol + 
  daily_smoke + pop_den + Age_15_24 + Age_25_64 + Age_65_Over +
  HIV_inc_100k + log_income + log_budget + mal + DM_prev + c_cdr +

summary(tbdata_highincome$c_cdr)
cdr_sd <- tbdata_highincome %>% 
  drop_na(c_cdr)
sd(cdr_sd$c_cdr)
   
summary(tbdata_highincome$HIV_inc_100k)
hiv_sd <- tbdata_highincome %>% 
  drop_na(HIV_inc_100k)
sd(hiv_sd$HIV_inc_100k)
  
summary(tbdata_highincome$income_cap)
summary(tbdata_highincome$log_income)
sd(tbdata_highincome$income_cap)
sd(tbdata_highincome$log_income)
summary(tbdata_highincome$mal)
mal_sd <- tbdata_highincome %>% 
  drop_na(mal)
sd(mal_sd$mal)
  
summary(tbdata_highincome$DM_prev)
dm_sd <- tbdata_highincome %>% 
  drop_na(DM_prev)
sd(dm_sd$DM_prev)
  
summary(tbdata_highincome$budget_cap)
summary(tbdata_highincome$log_budget)
budget_sd <- tbdata_highincome %>% 
  drop_na(budget_cap) %>% 
  drop_na(log_budget)
sd(budget_sd$budget_cap)
sd(budget_sd$log_budget)
summary(tbdata_highincome$pop_den)
pop_sd <- tbdata_highincome %>% 
  drop_na(pop_den)
sd(pop_sd$pop_den)
  
summary(tbdata_highincome$daily_smoke)
smoke_sd <- tbdata_highincome %>% 
  drop_na(daily_smoke)
sd(smoke_sd$daily_smoke)
  
summary(tbdata_highincome$air_pol)
air_sd <- tbdata_highincome %>% 
  drop_na(air_pol)
sd(air_sd$air_pol)
  
summary(tbdata_highincome$alc_tot)
alc_sd <- tbdata_highincome %>% 
  drop_na(alc_tot)
sd(alc_sd$alc_tot)
  
summary(tbdata_highincome$unemploy)
unemploy_sd <- tbdata_highincome %>% 
  drop_na(unemploy)
sd(unemploy_sd$unemploy)
  
summary(tbdata_highincome$Age_65_Over)
sd(tbdata_highincome$Age_65_Over)
  
summary(tbdata_highincome$Age_25_64)
sd(tbdata_highincome$Age_25_64)
  
summary(tbdata_highincome$Age_15_24)
sd(tbdata_highincome$Age_15_24)
    
summary(tbdata_highincome$e_mort_100k)
summary(tbdata_highincome$log_mort)
sd(tbdata_highincome$e_mort_100k)
sd(tbdata_highincome$log_mort)
    
summary(tbdata_highincome$e_inc_100k)
summary(tbdata_highincome$log_inc)
sd(tbdata_highincome$e_inc_100k)
sd(tbdata_highincome$log_inc)
summary(tbdata_lowincome$c_cdr)
cdr_sd <- tbdata_lowincome %>% 
  drop_na(c_cdr)
sd(cdr_sd$c_cdr)
  
summary(tbdata_lowincome$HIV_inc_100k)
hiv_sd <- tbdata_lowincome %>% 
  drop_na(HIV_inc_100k)
sd(hiv_sd$HIV_inc_100k)
  
  
summary(tbdata_lowincome$income_cap)
summary(tbdata_lowincome$log_income)
sd(tbdata_lowincome$income_cap) 
sd(tbdata_lowincome$log_income)
summary(tbdata_lowincome$mal)
mal_sd <- tbdata_lowincome %>% 
  drop_na(mal)
sd(mal_sd$mal)
  
summary(tbdata_lowincome$DM_prev)
dm_sd <- tbdata_lowincome %>% 
  drop_na(DM_prev)
sd(dm_sd$DM_prev)
  
summary(tbdata_lowincome$budget_cap)
summary(tbdata_lowincome$log_budget)

budget_sd <- tbdata_lowincome %>% 
  drop_na(budget_cap)
sd(budget_sd$budget_cap)
sd(budget_sd$log_budget)    
summary(tbdata_lowincome$pop_den)
sd(tbdata_lowincome$pop_den)
  
summary(tbdata_lowincome$daily_smoke)
smoke_sd <- tbdata_lowincome %>% 
  drop_na(daily_smoke)
sd(smoke_sd$daily_smoke)
  
summary(tbdata_lowincome$air_pol)
airpol_sd <- tbdata_lowincome %>% 
  drop_na(air_pol)
sd(airpol_sd$air_pol)
  
summary(tbdata_lowincome$alc_tot)
alcsd <- tbdata_lowincome %>% 
  drop_na(alc_tot)
sd(alcsd$alc_tot)

summary(tbdata_lowincome$unemploy)
unemploy_sd <- tbdata_lowincome %>% 
  drop_na(unemploy)
sd(unemploy_sd$unemploy)
  
summary(tbdata_lowincome$Age_65_Over)  
sd(tbdata_lowincome$Age_65_Over)
   
summary(tbdata_lowincome$Age_25_64)
sd(tbdata_lowincome$Age_25_64)
   
summary(tbdata_lowincome$Age_15_24)
sd(tbdata_lowincome$Age_15_24)
  
  
summary(tbdata_lowincome$e_inc_100k)
summary(tbdata_lowincome$log_inc)
sd(tbdata_lowincome$e_inc_100k)
sd(tbdata_lowincome$log_inc)

summary(tbdata_lowincome$e_mort_100k)
summary(tbdata_lowincome$log_mort)

sd(tbdata_lowincome$e_mort_100k)
sd(tbdata_lowincome$log_mort)

summary(tbdata_new$c_cdr)  
cdr_sd <- tbdata_new %>% 
  drop_na(c_cdr)
sd(cdr_sd$c_cdr)
  
summary(tbdata_new$Age_15_24)
sd(tbdata_new$Age_15_24)

summary(tbdata_new$Age_25_64)
sd(tbdata_new$Age_25_64)  

summary(tbdata_new$Age_65_Over)
sd(tbdata_new$Age_65_Over) 
  
summary(tbdata_new$budget_cap)
summary(tbdata_new$log_budget)
budget_sd <- tbdata_new %>% 
  drop_na(budget_cap) %>% 
  drop_na(log_budget)
sd(budget_sd$budget_cap)
sd(budget_sd$log_budget)


summary(tbdata_new$pop_den)
popden_sd <- tbdata_new %>% 
  drop_na(pop_den)
sd(popden_sd$pop_den)
  
summary(tbdata_new$daily_smoke)
dailysmoke_sd <- tbdata_new %>% 
  drop_na(daily_smoke)
sd(dailysmoke_sd$daily_smoke)
  
summary(tbdata_new$air_pol)
airpol_sd <- tbdata_new %>% 
  drop_na(air_pol)
sd(airpol_sd$air_pol)
  
summary(tbdata_new$unemploy)
tbdata_unemploy <- tbdata_new %>% 
  drop_na(unemploy)
sd(tbdata_unemploy$unemploy)

summary(tbdata_new$alc_tot)
alc_totsd <- tbdata_new %>% 
  drop_na(alc_tot)
sd(alc_totsd$alc_tot)



#Doing predictions now 
  
summary(lowincome_inc_multi)  
  
  
pdat_Angola = data.frame(alc_tot = 5.77, air_pol = 68.4, 
                         daily_smoke = 10.54, Age_15_24 = 18.89, 
                         Age_25_64 = 33.33, Age_65_Over = 2.59, HIV_inc_100k = 86, 
                         log_income = log(1104), DM_prev = 4, c_cdr = 55,
                           country="Angola")

pdat_Bangladesh = data.frame(alc_tot = 0, air_pol = 6.3, 
                             daily_smoke = 9.57, Age_15_24 = 15.35, 
                             Age_25_64 = 54.53, Age_65_Over = 9.58, HIV_inc_100k = 1, 
                             log_income = log(1999), DM_prev = 12.5, c_cdr = 82,
                             country="Bangladesh")


pdat_Brazil = data.frame(alc_tot = 6.08,  
                          Age_15_24 = 19.47, 
                         Age_25_64 = 48.25, Age_65_Over = 5.83, HIV_inc_100k = 39, 
                         log_budget = log(700), DM_prev = 10.5,  country="Brazil")



#Using Thailand here as China couldn't be used in the model due to misssing data.
#Thaidland borders China and is in the same WHO income grouping 
#Did the same thing for other countries that were missing data as well.
#Chose a proxy country that borders the country of interest and which is in the same economic group
pdat_China = data.frame(alc_tot = 4.47,  
                         Age_15_24 = 11.27, 
                         Age_25_64 = 57.91, Age_65_Over = 13.15, HIV_inc_100k = 2.4, 
                         log_budget = log(583), DM_prev = 10.9,  country="Thailand")


pdat_DRC = data.frame(alc_tot = 0.56, air_pol = 156.9, 
                             daily_smoke = 11.13, Age_15_24 = 19.00, 
                             Age_25_64 = 31.51, Age_65_Over = 2.96, HIV_inc_100k = 37, 
                             log_income = log(417), DM_prev = 4.8, c_cdr = 70,
                              country="Democratic Republic of the Congo")

pdat_Ethiopia = data.frame(alc_tot = 0.94, air_pol = 130.0, 
                      daily_smoke = 5.97, Age_15_24 = 21.05, 
                      Age_25_64 = 38.85, Age_65_Over = 3.14, HIV_inc_100k = 19, 
                      log_income = log(792), DM_prev = 3.3, c_cdr = 87,
                      country="Ethiopia")

pdat_India = data.frame(alc_tot = 3.08, air_pol = 59.6, 
                           daily_smoke = 12.31, Age_15_24 = 18.09, 
                           Age_25_64 = 49.94, Age_65_Over = 6.80, HIV_inc_100k = 5, 
                           log_income = log(1663), DM_prev = 8.3, c_cdr = 67,
                           country="Bangladesh")

pdat_Indonesia = data.frame(alc_tot = 0.09, air_pol = 41.0, 
                        daily_smoke = 30.79, Age_15_24 = 16.12, 
                        Age_25_64 = 51.63, Age_65_Over = 6.78, HIV_inc_100k = 16, 
                        log_income = log(3111), DM_prev = 10.8, c_cdr = 45,
                        country="Indonesia")


pdat_Kenya = data.frame(alc_tot = 1.66, air_pol = 97.9, 
                            daily_smoke = 10.65, Age_15_24 = 20.78, 
                            Age_25_64 = 37.98, Age_65_Over = 2.84, HIV_inc_100k = 117, 
                            log_income = log(1594), DM_prev = 3, c_cdr = 57,
                            country="Kenya")

pdat_Mozambique = data.frame(alc_tot = 1.46, air_pol = 185.5, 
                        daily_smoke = 13.31, Age_15_24 = 20.25, 
                        Age_25_64 = 33.48, Age_65_Over = 2.59, HIV_inc_100k = 554, 
                        log_income = log(331), DM_prev = 2.4, c_cdr = 85,
                        country="Zambia")

pdat_Nigeria = data.frame(alc_tot = 1.22, air_pol = 83.2, 
                             daily_smoke = 14.45, Age_15_24 = 20.22, 
                             Age_25_64 = 38.62, Age_65_Over = 4.22, HIV_inc_100k = 44, 
                             log_income = log(1712), DM_prev = 3.7, c_cdr = 44,
                             country="Nigeria")

pdat_Pakistan = data.frame(alc_tot = 0.04, air_pol = 92.6, 
                          daily_smoke = 3.46, Age_15_24 = 19.56, 
                          Age_25_64 = 34.18, Age_65_Over = 2.98, HIV_inc_100k = 1, 
                          log_income = log(1070), DM_prev = 26.7, c_cdr = 55,
                          country="Bangladesh")

pdat_Philippines = data.frame(alc_tot = 4.84, air_pol = 60.9, 
                           daily_smoke = 24.06, Age_15_24 = 18.49, 
                           Age_25_64 = 45.55, Age_65_Over = 5.33, HIV_inc_100k = 36, 
                           log_income = log(3128), DM_prev = 6.5, c_cdr = 45,
                           country="Philippines")

pdat_SA = data.frame(alc_tot = 6.51,  
                        Age_15_24 = 15.75, 
                        Age_25_64 = 49.61, Age_65_Over = 5.97, HIV_inc_100k = 690, 
                        log_budget = log(490), DM_prev = 11.3,  country="South Africa")


pdat_Thailand = data.frame(alc_tot = 6.86,  
                     Age_15_24 = 12.40, 
                     Age_25_64 = 57.29, Age_65_Over = 14.51, HIV_inc_100k = 19, 
                     log_budget = log(305), DM_prev = 11.6,  country="Thailand")


pdat_Uganda = data.frame(alc_tot = 1.36, air_pol = 125.3, 
                              daily_smoke = 8.61, Age_15_24 = 22.08, 
                              Age_25_64 = 31.05, Age_65_Over = 1.08, HIV_inc_100k = 240, 
                              log_income = log(719), DM_prev = 3.6, c_cdr = 82,
                              country="Uganda")
pdat_Vietnam = data.frame(alc_tot = 3.41, air_pol = 40.4, 
                         daily_smoke = 24.41, Age_15_24 = 14.20, 
                         Age_25_64 = 54.51, Age_65_Over = 8.75, HIV_inc_100k = 9, 
                         log_income = log(2252), DM_prev = 6.0, c_cdr = 46,
                         country="Viet Nam")

pdat_CAF = data.frame(alc_tot = 0.61, air_pol = 251.2, 
                          daily_smoke = 8.18, Age_15_24 = 22.00, 
                          Age_25_64 = 27.32, Age_65_Over = 2.50, HIV_inc_100k = 104, 
                          log_income = log(362), DM_prev = 4.5, c_cdr = 45,
                          country="Central African Republic")

pdat_Congo = data.frame(alc_tot = 5.73, air_pol = 66.7, 
                      daily_smoke = 11.75, Age_15_24 = 18.48, 
                      Age_25_64 = 37.44, Age_65_Over = 2.68, HIV_inc_100k = 380, 
                      log_income = log(689), DM_prev = 5.6, c_cdr = 55,
                      country="Congo")

pdat_Gabon = data.frame(alc_tot = 6.43,  
                           Age_15_24 = 17.47, 
                           Age_25_64 = 42.21, Age_65_Over = 3.90, HIV_inc_100k = 115, 
                           log_budget = log(229), DM_prev = 6.5,  country="Gabon")


pdat_Lesotho = data.frame(alc_tot = 2.73, air_pol = 133.4, 
                        daily_smoke = 21.40, Age_15_24 = 19.55, 
                        Age_25_64 = 42.14, Age_65_Over = 4.21, HIV_inc_100k = 810, 
                        log_income = log(767), DM_prev = 3.9, c_cdr = 32,
                        country="Lesotho")

pdat_Liberia = data.frame(alc_tot = 3.10, air_pol = 122.1, 
                          daily_smoke = 7.44, Age_15_24 = 20.25, 
                          Age_25_64 = 30.40, Age_65_Over = 3.34, HIV_inc_100k = 37, 
                          log_income = log(317), DM_prev = 1.9, c_cdr = 46,
                          country="Sierra Leone")

pdat_Mongolia = data.frame(alc_tot = 5.46, air_pol = 49.0, 
                          daily_smoke = 29.52, Age_15_24 = 13.55, 
                          Age_25_64 = 49.60, Age_65_Over = 4.42, HIV_inc_100k = 3, 
                          log_income = log(3017), DM_prev = 7.4, c_cdr = 27,
                          country="Mongolia")

pdat_Namibia = data.frame(alc_tot = 2.06,  
                        Age_15_24 = 18.65, 
                        Age_25_64 = 41.12, Age_65_Over = 4.03, HIV_inc_100k = 516, 
                        log_budget = log(380), DM_prev = 5.5,  country="Namibia")


pdat_PNG = data.frame(alc_tot = 1.26, air_pol = 229.5, 
                           daily_smoke = 34.79, Age_15_24 = 19.49, 
                           Age_25_64 = 42.82, Age_65_Over = 3.10, HIV_inc_100k = 64, 
                           log_income = log(2102), DM_prev = 14.6, c_cdr = 68,
                           country="Papua New Guinea")

pdat_SL = data.frame(alc_tot = 0.33, air_pol = 168.3, 
                      daily_smoke = 15.80, Age_15_24 = 20.54, 
                      Age_25_64 = 36.93, Age_65_Over = 3.14, HIV_inc_100k = 54, 
                      log_income = log(421), DM_prev = 1.8, c_cdr = 72,
                      country="Sierra Leone")

pdat_Zambia = data.frame(alc_tot = 1.64, air_pol = 111.1, 
                     daily_smoke = 14.61, Age_15_24 = 20.42, 
                     Age_25_64 = 34.58, Age_65_Over = 1.74, HIV_inc_100k = 237, 
                     log_income = log(934), DM_prev = 1.5, c_cdr = 54,
                     country="Zambia")
summary(lowincome_mort_multi)



pdat_Angola_mort = data.frame(alc_tot = 5.77, air_pol = 68.4, 
                         daily_smoke = 10.54, pop_den = 27, Age_15_24 = 18.89, 
                         Age_25_64 = 33.33, Age_65_Over = 2.59, HIV_inc_100k = 86, 
                         log_income = log(1104), mal = 20.8, c_cdr = 55,
                         country="Angola")



pdat_Bangladesh_mort = data.frame(alc_tot = 0, air_pol = 6.3, 
                              daily_smoke = 9.57, pop_den = 1286, Age_15_24 = 15.35, 
                              Age_25_64 =54.53, Age_65_Over = 9.58, HIV_inc_100k = 1, 
                              log_income = log(1999), mal = 11.4, c_cdr = 82,
                              country="Bangladesh")

summary(highincome_mort_multi)

pdat_Brazil_mort = data.frame(alc_tot = 6.08, daily_smoke= 9.57, pop_den=26,  
                         Age_15_24 = 19.47, 
                         Age_25_64 = 48.25, Age_65_Over = 5.83, HIV_inc_100k = 39, 
                         log_budget = log(700), DM_prev = 10.5,  country="Brazil")


pdat_China_mort = data.frame(alc_tot = 4.47, daily_smoke= 26.74, pop_den=150,  
                              Age_15_24 = 11.27, 
                              Age_25_64 = 57.91, Age_65_Over = 13.15, HIV_inc_100k = 2.4, 
                              log_budget = log(583), DM_prev = 10.9,  country="Thailand")


pdat_DRC_mort = data.frame(alc_tot = 0.56, air_pol = 156.9, 
                                  daily_smoke = 11.13, pop_den = 41, Age_15_24 = 19.00, 
                                  Age_25_64 =31.51, Age_65_Over = 2.96, HIV_inc_100k = 37, 
                                  log_income = log(417), mal = 39.8, c_cdr = 82,
                                  country="Democratic Republic of the Congo")


pdat_Ethiopia_mort = data.frame(alc_tot = 0.94, air_pol = 130.0, 
                           daily_smoke = 5.97, pop_den = 104, Age_15_24 = 21.05, 
                           Age_25_64 =38.85, Age_65_Over = 3.14, HIV_inc_100k = 19, 
                           log_income = log(792), mal = 24.9, c_cdr = 87,
                           country="Ethiopia")


pdat_India_mort = data.frame(alc_tot = 3.08, air_pol = 59.6, 
                                daily_smoke = 12.31, pop_den = 470, Age_15_24 = 18.09, 
                                Age_25_64 =49.94, Age_65_Over = 6.80, HIV_inc_100k = 5, 
                                log_income = log(1663), mal = 16.3, c_cdr = 67,
                                country="Bangladesh")



pdat_Indonesia_mort = data.frame(alc_tot = 0.09, air_pol = 41.0, 
                             daily_smoke = 30.79, pop_den = 145, Age_15_24 = 16.12, 
                             Age_25_64 =51.63, Age_65_Over = 6.78, HIV_inc_100k =16, 
                             log_income = log(3111), mal = 7, c_cdr = 45,
                             country="Indonesia")


pdat_Kenya_mort = data.frame(alc_tot = 1.66, air_pol = 97.9, 
                                 daily_smoke = 10.65, pop_den = 91, Age_15_24 = 20.78, 
                                 Age_25_64 =37.98, Age_65_Over = 2.84, HIV_inc_100k =117, 
                                 log_income = log(1594), mal = 26.9, c_cdr = 57,
                                 country="Kenya")


pdat_Mozambique_mort = data.frame(alc_tot = 1.46, air_pol = 185.5, 
                             daily_smoke = 13.31, pop_den = 40, Age_15_24 = 20.25, 
                             Age_25_64 =33.48, Age_65_Over = 2.59, HIV_inc_100k =554, 
                             log_income = log(331), mal = 32.7, c_cdr = 85,
                             country="United Republic of Tanzania")


pdat_Nigeria_mort = data.frame(alc_tot = 1.22, air_pol = 83.2, 
                                  daily_smoke = 14.45, pop_den = 229, Age_15_24 = 20.22, 
                                  Age_25_64 =38.62, Age_65_Over = 4.22, HIV_inc_100k =44, 
                                  log_income = log(1712), mal = 12.7, c_cdr = 44,
                                  country="Nigeria")


pdat_Pakistan_mort = data.frame(alc_tot = 0.04, air_pol = 92.6, 
                               daily_smoke = 3.46, pop_den = 295, Age_15_24 = 19.56, 
                               Age_25_64 =34.18, Age_65_Over = 2.98, HIV_inc_100k =1, 
                               log_income = log(1070), mal = 16.8, c_cdr = 55,
                               country="Bangladesh")


pdat_SA_mort = data.frame(alc_tot = 6.51, daily_smoke= 21.98, pop_den=48,  
                             Age_15_24 = 15.75, 
                             Age_25_64 = 49.61, Age_65_Over = 5.97, HIV_inc_100k = 690, 
                             log_budget = log(490), DM_prev = 11.3,  country="South Africa")


pdat_Thailand_mort = data.frame(alc_tot = 6.86, daily_smoke= 21.18, pop_den=140,  
                          Age_15_24 = 12.40, 
                          Age_25_64 = 57.29, Age_65_Over = 14.51, HIV_inc_100k = 19, 
                          log_budget = log(305), DM_prev = 11.6,  country="Thailand")


pdat_Uganda_mort = data.frame(alc_tot = 1.36, air_pol = 125.3, 
                                daily_smoke = 8.61, pop_den = 221, Age_15_24 = 22.08, 
                                Age_25_64 =31.05, Age_65_Over = 1.08, HIV_inc_100k =240, 
                                log_income = log(719), mal = 39.8, c_cdr = 82,
                                country="Democratic Republic of the Congo")

pdat_Vietnam_mort = data.frame(alc_tot = 3.41, air_pol = 40.4, 
                              daily_smoke = 24.41, pop_den = 308, Age_15_24 = 14.20, 
                              Age_25_64 =54.51, Age_65_Over = 8.75, HIV_inc_100k =9, 
                              log_income = log(2252), mal = 5.7, c_cdr = 46,
                              country="Viet Nam")



pdat_CAF_mort = data.frame(alc_tot = 0.61, air_pol = 251.2, 
                               daily_smoke = 8.18, pop_den = 9, Age_15_24 = 22.00, 
                               Age_25_64 =27.32, Age_65_Over = 2.50, HIV_inc_100k =104, 
                               log_income = log(362), mal = 52.2, c_cdr = 45,
                               country="Central African Republic")

pdat_Congo_mort = data.frame(alc_tot = 5.73, air_pol = 66.7, 
                           daily_smoke = 11.75, pop_den = 17, Age_15_24 = 18.48, 
                           Age_25_64 =37.44, Age_65_Over = 2.68, HIV_inc_100k =380, 
                           log_income = log(689), mal = 31.6, c_cdr = 55,
                           country="Congo")



pdat_Gabon_mort = data.frame(alc_tot = 6.43, daily_smoke= 12.38, pop_den=9,  
                                Age_15_24 = 17.47, 
                                Age_25_64 = 42.21, Age_65_Over = 3.90, HIV_inc_100k = 115, 
                                log_budget = log(229), DM_prev = 6.5,  country="Gabon")


pdat_Lesotho_mort = data.frame(alc_tot = 2.73, air_pol = 133.4, 
                             daily_smoke = 21.40, pop_den = 74, Age_15_24 = 19.55, 
                             Age_25_64 =42.14, Age_65_Over = 4.21, HIV_inc_100k =810, 
                             log_income = log(767), mal = 34.7, c_cdr = 32,
                             country="Lesotho")

pdat_Liberia_mort = data.frame(alc_tot = 3.10, air_pol = 122.1, 
                               daily_smoke = 7.44, pop_den = 53, Age_15_24 = 20.25, 
                               Age_25_64 =30.40, Age_65_Over = 3.34, HIV_inc_100k =37, 
                               log_income = log(317), mal = 38.3, c_cdr = 46,
                               country="Sierra Leone")


pdat_Mongolia_mort = data.frame(alc_tot = 5.46, air_pol = 49.0, 
                               daily_smoke = 29.52, pop_den = 2, Age_15_24 = 13.55, 
                               Age_25_64 =49.60, Age_65_Over = 4.42, HIV_inc_100k =3, 
                               log_income = log(3017), mal = 3.6, c_cdr = 27,
                               country="Mongolia")

pdat_Namibia_mort = data.frame(alc_tot = 2.06, daily_smoke= 16.15, pop_den=3,  
                             Age_15_24 = 18.65, 
                             Age_25_64 = 41.12, Age_65_Over = 4.03, HIV_inc_100k = 516, 
                             log_budget = log(380), DM_prev = 5.5,  country="Namibia")

pdat_PNG_mort = data.frame(alc_tot = 1.26, air_pol = 229.5, 
                                daily_smoke = 34.79, pop_den = 22, Age_15_24 = 19.49, 
                                Age_25_64 =42.82, Age_65_Over = 3.10, HIV_inc_100k =64, 
                                log_income = log(2102), mal = 21.6, c_cdr = 68,
                                country="Papua New Guinea")

pdat_SL_mort = data.frame(alc_tot = 0.33, air_pol = 168.3, 
                           daily_smoke = 15.80, pop_den = 114, Age_15_24 = 20.54, 
                           Age_25_64 =36.93, Age_65_Over = 3.14, HIV_inc_100k =54, 
                           log_income = log(421), mal = 27.4, c_cdr = 72,
                           country="Sierra Leone")

pdat_Zambia = data.frame(alc_tot = 1.64, air_pol = 111.1, 
                         daily_smoke = 14.61, Age_15_24 = 20.42, 
                         Age_25_64 = 34.58, Age_65_Over = 1.74, HIV_inc_100k = 237, 
                         log_income = log(934), DM_prev = 1.5, c_cdr = 54,
                         country="Zambia")

pdat_Zambia_mort = data.frame(alc_tot = 1.64, air_pol = 111.1, 
                          daily_smoke = 14.61, pop_den = 25, Age_15_24 = 20.42, 
                          Age_25_64 =34.58, Age_65_Over = 1.74, HIV_inc_100k =237, 
                          log_income = log(934), mal = 20.8, c_cdr = 54,
                          country="Angola")


#For making predictions I switched out the country name after pdat
#Additionally would change the model to the high income model if the country was in the 
#upper middle income bracket (China, South Africa, Thailand, Gabon, and Namibia)

p0 <- predict(lowincome_mort_multi, newdata = pdat_Zambia_mort, re.form = (NULL))
exp(p0)
v <- c(VarCorr(AFR_inc_multi)$country)
exp(p0 + v/2)



inc_diff <- c(90, -81, 20, 79, 23, 88, -31, 11, 159, 64, 8, -38, -83, 241, -2, -14, 62, 61, 121, 75, -59, 2, 36, 229, 72, 91, 123)

summary(inc_diff)



mort_dff <- c(11, -14, 1.2, 12.8, 10, -14, -22, -18, 55, 117, 18, -4, -27, 20, 0, 35, 8, 89, 16, -36, 1, -26, 1, 34, 14, 0,28)
 
summary(mort_dff)


#Everything below this line is code from when I made my old models that only included mal, DM2,
#income and HIV. Keeping it for posterity 


#Trying without malnutrition since it's not a significant predictor 

globe_inc_multi_nomal <- lmer(log_inc ~ HIV_inc + log_income + year + 
                          DM_prev + (1|g_whoregion/country), data=tbglobe_nomyan, REML = FALSE)

summary(globe_inc_multi_nomal)
check_model(globe_inc_multi_nomal)

#Univariate global models for mortality 

globe_mort_DM <- lmer(log_mort ~ DM_prev + (1|g_whoregion/country), data=tbdata_2019_new, 
                      REML = FALSE)
summary(globe_mort_DM)
check_model(globe_mort_DM)


globe_mort_income <- lmer(log_mort ~ log_income + (1|g_whoregion/country), 
                          data=tbglobe_nomyan, REML = FALSE)
summary(globe_mort_income)
check_model(globe_mort_income)

globe_mort_HIV <- lmer(log_mort ~ HIV_inc + (1|g_whoregion/country), 
                       data=tbdata_2019_new, REML = FALSE)
summary(globe_mort_HIV)
check_model(globe_mort_HIV)

globe_mort_mal <- lmer(log_mort ~ `Malnutrition_prev ` + (1|g_whoregion/country), 
                        data=tbdata_2019_new, REML = FALSE)
summary(globe_mort_mal)
check_model(globe_mort_mal)

#Now global mortality multivariable model 

globe_mort_multi <- lmer(log_mort ~ `Malnutrition_prev ` + HIV_inc + log_income + year + 
                           DM_prev + (1|g_whoregion/country), data=tbglobe_nomyan, REML = FALSE)
summary(globe_mort_multi)
check_model(globe_mort_multi)

#Checking without DM 
globe_mort_multi_noDM <- lmer(log_mort ~ `Malnutrition_prev ` + HIV_inc + log_income + year +
                            (1|g_whoregion/country), data=tbglobe_nomyan, REML = FALSE)
summary(globe_mort_multi_noDM)
check_model(globe_mort_multi_noDM)

#Now univariate global CFR models 



globe_CFR_DM <- lmer(logit_CFR ~ DM_prev + (1|g_whoregion/country), data=tbdata_2019_new, 
                     REML = FALSE)
summary(globe_CFR_DM)
check_model(globe_CFR_DM)


globe_CFR_income <- lmer(logit_CFR ~ log_income + (1|g_whoregion/country), 
                         data=tbglobe_nomyan, REML = FALSE)
summary(globe_CFR_income)
check_model(globe_CFR_income)

globe_CFR_HIV <- lmer(logit_CFR ~ HIV_inc + (1|g_whoregion/country), 
                      data=tbdata_2019_new, REML = FALSE)
summary(globe_CFR_HIV)
check_model(globe_CFR_HIV)

globe_CFR_mal <- lmer(logit_CFR ~ `Malnutrition_prev ` + (1|g_whoregion/country), 
                      data=tbdata_2019_new, REML = FALSE)
summary(globe_CFR_mal)
check_model(globe_CFR_mal)

#Mutlivariate global CFR model 

globe_CFR_multi <- lmer(logit_CFR ~ `Malnutrition_prev ` + HIV_inc + log_income + year + DM_prev + 
                          year*log_income +
                            (1|g_whoregion/country), data=tbglobe_nomyan, REML = FALSE)


summary(globe_CFR_multi)
check_model(globe_CFR_multi)


#Moving on to regional models. Starting with Africa incidence 

tbdata_AFR <- tbdata_AFR %>% 
  mutate(log_inc_AFR = log(e_inc_100k)) %>% 
  mutate(log_DM_AFR = log(DM_prev)) %>% 
  mutate(log_mal_AFR = log(`Malnutrition_prev `)) %>% 
  mutate(log_HIV_AFR = log(HIV_inc)) %>% 
  mutate(log_income = log(income_cap)) %>% 
  mutate (log_mort_AFR = log(e_mort_100k)) %>% 
  mutate(logit_CFR = logit(case_fatality))

AFR_inc_DM <- lmer(log_inc_AFR ~ DM_prev + 
                     (1|country), data=tbdata_AFR, REML = FALSE)
summary(AFR_inc_DM)
check_model(AFR_inc_DM)

AFR_inc_income <- lmer(log_inc_AFR ~ log_income + 
                         (1|country), data=tbdata_AFR, REML = FALSE)
summary(AFR_inc_income)
check_model(AFR_inc_income)

AFR_inc_HIV <- lmer(log_inc_AFR ~ HIV_inc + 
                      (1|country), data=tbdata_AFR, REML = FALSE)
summary(AFR_inc_HIV)
check_model(AFR_inc_HIV)

AFR_inc_HIV_log <- lmer(log_inc_AFR ~ log_HIV_AFR + 
                      (1|country), data=tbdata_AFR, REML = FALSE)
summary(AFR_inc_HIV_log)
check_model(AFR_inc_HIV_log)

AFR_inc_mal <- lmer(log_inc_AFR ~  +  `Malnutrition_prev ` +
                      (1|country), data=tbdata_AFR, REML = FALSE)
summary(AFR_inc_mal)
check_model(AFR_inc_mal)

#Checked log here too by just replacing malnutrition w/ the log mal but diagnostics were even worse
#Doing multivariate now, trying first w/ out the log value of HIV just to see but I suspect from the 
#univariate model that I will need to use log HIV 

#Just so the timeline is clear. I am now coming back to these models to do predictions but finding
#that I need to rename the malnutrition variable. So I am renaming the malnutrition variable for
#the purposes of prediction but my ouput still have Manutrition_prev on it. 
tbdata_AFR_new <- tbdata_AFR %>% 
  rename("mal" = `Malnutrition_prev `)

AFR_inc_multi <- lmer(log_inc_AFR ~ mal + log_HIV_AFR + 
                        log_income + DM_prev + year +
                        (1|country), data=tbdata_AFR_new, REML = FALSE)
summary(AFR_inc_multi)
check_model(AFR_inc_multi)
install.packages("merTools")
library(merTools)

pdat_AFR_Ang = data.frame(mal=20.8, log_HIV_AFR = log(86),
                      log_income = log(1104), DM_prev = 4, country="Angola")
p0 <- predict(AFR_inc_multi, newdata = pdat_AFR_Ang, re.form = (NULL))
exp(p0)
v <- c(VarCorr(AFR_inc_multi)$country)
exp(p0 + v/2)

pdat_AFR_Eth = data.frame(mal=24.9, log_HIV_AFR = log(17), log_income=log(792),
                          DM_prev=3.3, country="Ethiopia")
p0 <- predict(AFR_inc_multi, newdata=pdat_AFR_Eth, re.form=(NULL))

pdat_AFR_Ken = data.frame(mal=26.9, log_HIV_AFR = log(117), log_income=log(1594),
                          DM_prev=3, year=2021, country="Kenya")
p0 <- predict(AFR_inc_multi, newdata=pdat_AFR_Ken, re.form=(NULL))

load("/Users/zanwynia/Downloads/tb.rda")

#Redoing w/ out malnutrition and DM because of non-significance 

AFR_inc_multi_noDM_mal <- lmer(log_inc_AFR ~  log_HIV_AFR + 
                                 log_income +
                                 (1|country), data=tbdata_AFR, REML = FALSE)
summary(AFR_inc_multi_noDM_mal)
check_model(AFR_inc_multi_noDM_mal)




#Now doing Africa mortality 

AFR_mort_DM <- lmer(log_mort_AFR ~ DM_prev + (1|country), data=tbdata_AFR, REML = FALSE)
summary(AFR_mort_DM)
check_model(AFR_mort_DM)

AFR_mort_income <- lmer(log_mort_AFR ~ log_income + (1|country), data=tbdata_AFR, REML = FALSE)
summary(AFR_mort_income)
check_model(AFR_mort_income)

#Checking both HIV and log HIV based on resulsts from incidence model 
AFR_mort_HIV <- lmer(log_mort_AFR ~ HIV_inc + (1|country), data=tbdata_AFR, REML = FALSE)
AFR_mort_HIV_log <- lmer(log_mort_AFR ~ log_HIV_AFR + (1|country), data=tbdata_AFR, REML = FALSE)
summary(AFR_mort_HIV_log)
check_model(AFR_mort_HIV)
check_model(AFR_mort_HIV_log)
#Log HIV w/ better diagnostics 
AFR_mort_mal <- lmer(log_mort_AFR ~ `Malnutrition_prev ` + (1|country), data=tbdata_AFR, REML = FALSE)
summary(AFR_mort_mal)
check_model(AFR_mort_mal)

#Multivariate mortality in Africa, will start w/ log HIV based on univariate model 

AFR_mort_multi <- lmer(log_mort_AFR ~ log_HIV_AFR + DM_prev +
                          year + log_income + `Malnutrition_prev ` +
                          (1|country), data=tbdata_AFR, REML = FALSE)
summary(AFR_mort_multi)
check_model(AFR_mort_multi)

#Checking a model w/ out DM and Mal again 

AFR_mort_multi_noDM_mal <- lmer(log_mort_AFR ~ log_HIV_AFR + 
                         log_income + (1|country), data=tbdata_AFR, REML = FALSE)
summary(AFR_mort_multi_noDM_mal)
check_model(AFR_mort_multi_noDM_mal)

#Doing CFR 

AFR_CFR_DM <- lmer(logit_CFR ~ DM_prev + (1|country), data=tbdata_AFR, REML = FALSE)
summary(AFR_CFR_DM)
check_model(AFR_CFR_DM)

AFR_CFR_income <- lmer(logit_CFR ~ log_income + (1|country), data=tbdata_AFR, REML = FALSE)
summary(AFR_CFR_income)
check_model(AFR_CFR_income)

AFR_CFR_logHIV <- lmer(logit_CFR ~ log_HIV_AFR + (1|country), data=tbdata_AFR, REML = FALSE)
summary(AFR_CFR_logHIV)
check_model(AFR_CFR_logHIV)

AFR_CFR_mal <- lmer(logit_CFR ~ `Malnutrition_prev ` + (1|country), data=tbdata_AFR, REML = FALSE)
summary(AFR_CFR_mal)
check_model(AFR_CFR_mal)

#Multivariate AFR CFR model 

AFR_CFR_multi <- lmer(logit_CFR ~ `Malnutrition_prev ` + log_HIV_AFR + log_income + year +
                        DM_prev + (1|country), data=tbdata_AFR, REML = FALSE)
summary(AFR_CFR_multi)
check_model(AFR_CFR_multi)
7.353e-03
1.442e-01
3.107e-04

#Checking w/ out mal and DM again 
AFR_CFR_multi_noDM_mal <- lmer(logit_CFR ~  log_HIV_AFR + log_income +
                        (1|country), data=tbdata_AFR, REML = FALSE)
summary(AFR_CFR_multi_noDM_mal)
check_model(AFR_CFR_multi_noDM_mal)

#America Models 
#Univariate Incidence Models 
tbdata_AMR <- tbdata_AMR %>% 
  mutate(log_inc_AMR = log(e_inc_100k)) %>% 
  mutate(log_DM_AMR = log(DM_prev)) %>% 
  mutate(log_mal_AMR = log(`Malnutrition_prev `)) %>% 
  mutate(log_HIV_AMR = log(HIV_inc)) %>% 
  mutate(log_income = log(income_cap)) %>% 
  mutate (log_mort_AMR = log(e_mort_100k)) %>% 
  mutate(logit_CFR = logit(case_fatality))

AMR_inc_DM <- lmer(log_inc_AMR ~ DM_prev + (1|country), data=tbdata_AMR, REML = FALSE)
summary(AMR_inc_DM)
check_model(AMR_inc_DM)

AMR_inc_income <- lmer(log_inc_AMR ~ log_income + (1|country), data=tbdata_AMR, REML = FALSE)
summary(AMR_inc_income)
check_model(AMR_inc_income)

#Better diagnostics w/ log HIV 
AMR_inc_HIV <- lmer(log_inc_AMR ~ log_HIV_AMR + (1|country), data=tbdata_AMR, REML = FALSE)
summary(AMR_inc_HIV)
check_model(AMR_inc_HIV)


AMR_inc_mal <- lmer(log_inc_AMR ~ `Malnutrition_prev ` + (1|country), data=tbdata_AMR, REML = FALSE)
summary(AMR_inc_mal)
check_model(AMR_inc_mal)


#Multivariate America Incidence
AMR_inc_multi <- lmer(log_inc_AMR ~ `Malnutrition_prev ` + DM_prev + log_HIV_AMR + log_income + year +
                        (1|country), data=tbdata_AMR, REML = FALSE)
summary(AMR_inc_multi)
check_model(AMR_inc_multi)

#Univariate Americas Mortality Models 
AMR_mort_DM <- lmer(log_mort_AMR ~ DM_prev + (1|country), data=tbdata_AMR, REML = FALSE)
summary(AMR_mort_DM)
check_model(AMR_mort_DM)

AMR_mort_income <- lmer(log_mort_AMR ~ log_income + (1|country), data=tbdata_AMR, REML = FALSE)
summary(AMR_mort_income)
check_model(AMR_mort_income)

AMR_mort_HIV <- lmer(log_mort_AMR ~ log_HIV_AMR + (1|country), data=tbdata_AMR, REML = FALSE)

summary(AMR_mort_HIV)
check_model(AMR_mort_HIV)

AMR_mort_mal <- lmer(log_mort_AMR ~ `Malnutrition_prev ` + (1|country), data=tbdata_AMR, REML = FALSE)
summary(AMR_mort_mal)
check_model(AMR_mort_mal)


#Multivariate Model
AMR_mort_multi <- lmer(log_mort_AMR ~ `Malnutrition_prev ` + DM_prev + log_HIV_AMR + log_income  + year +
                        (1|country), data=tbdata_AMR, REML = FALSE)
summary(AMR_mort_multi)
check_model(AMR_mort_multi)

AMR_mort_multi_2 <- lmer(log_mort_AMR ~ `Malnutrition_prev ` + DM_prev + log_HIV_AMR + log_income  +
                         (1|country) + (1|year), data=tbdata_AMR, REML = FALSE)
summary(AMR_mort_multi_2)
check_model(AMR_mort_multi_2)

#Americas CFR Univariate Models

AMR_CFR_DM <- lmer(logit_CFR ~ DM_prev + (1|country), data=tbdata_AMR, REML = FALSE)
summary(AMR_CFR_DM)
check_model(AMR_CFR_DM)

AMR_CFR_income <- lmer(logit_CFR ~ log_income + (1|country), data=tbdata_AMR, REML = FALSE)
summary(AMR_CFR_income)
check_model(AMR_CFR_income)


AMR_CFR_HIV <- lmer(logit_CFR ~ HIV_inc + (1|country), data=tbdata_AMR, REML = FALSE)
summary(AMR_CFR_HIV)
check_model(AMR_CFR_HIV)
#Good diagnostics w/ out log adjusted HIV 

AMR_CFR_mal <- lmer(logit_CFR ~ `Malnutrition_prev ` + (1|country), data=tbdata_AMR, REML = FALSE)
summary(AMR_CFR_mal)
check_model(AMR_CFR_mal)

#America CFR multivariate Model 
AMR_CFR_multi <- lmer(logit_CFR ~ `Malnutrition_prev ` + DM_prev + HIV_inc + log_income + year +
                         (1|country), data=tbdata_AMR, REML = FALSE)
summary(AMR_CFR_multi)
check_model(AMR_CFR_multi)


#Eastern Mediterranean Univariate Models for CFR 
tbdata_EMR <- tbdata_EMR %>% 
  mutate(log_inc_EMR = log(e_inc_100k)) %>% 
  mutate(log_DM_EMR = log(DM_prev)) %>% 
  mutate(log_mal_EMR = log(`Malnutrition_prev `)) %>% 
  mutate(log_HIV_EMR = log(HIV_inc)) %>% 
  mutate(log_income = log(income_cap)) %>% 
  mutate (log_mort_EMR = log(e_mort_100k)) %>% 
  mutate(logit_CFR = logit(case_fatality))

EMR_CFR_DM <- lmer(logit_CFR ~ DM_prev +
                     (1|country), data=tbdata_EMR, REML = FALSE)
summary(EMR_CFR_DM)
check_model(EMR_CFR_DM)

EMR_CFR_income <- lmer(logit_CFR ~ log_income +
                     (1|country), data=tbdata_EMR, REML = FALSE)
summary(EMR_CFR_income)
check_model(EMR_CFR_income)

EMR_CFR_HIV <- lmer(logit_CFR ~ log_HIV_EMR +
                         (1|country), data=tbdata_EMR, REML = FALSE)
summary(EMR_CFR_HIV)
check_model(EMR_CFR_HIV)


EMR_CFR_mal <- lmer(logit_CFR ~ `Malnutrition_prev ` +
                      (1|country), data=tbdata_EMR, REML = FALSE)
summary(EMR_CFR_mal)
check_model(EMR_CFR_mal)

#Multivariate model although I suspect from my univariate models this will be very poorly specified 

EMR_CFR_multi <- lmer(logit_CFR ~ `Malnutrition_prev ` + log_HIV_EMR + DM_prev + log_income +
                      (1|country), data=tbdata_EMR, REML = FALSE)
summary(EMR_CFR_multi)
check_model(EMR_CFR_multi)
#Actually not as badly specified as I thought it would be!
#Checking now without DM because it is so highly insignificant 

EMR_CFR_multi_noDM <- lmer(logit_CFR ~ `Malnutrition_prev ` + log_HIV_EMR + log_income +
                        (1|country), data=tbdata_EMR, REML = FALSE)
summary(EMR_CFR_multi_noDM)
check_model(EMR_CFR_multi_noDM)


#EM Incidence Univariate Models 

EMR_inc_DM <- lmer(log_inc_EMR ~ DM_prev +
                      (1|country), data=tbdata_EMR, REML = FALSE)
summary(EMR_inc_DM)
check_model(EMR_inc_DM)

EMR_inc_income <- lmer(log_inc_EMR ~ log_income +
                     (1|country), data=tbdata_EMR, REML = FALSE)
summary(EMR_inc_income)
check_model(EMR_inc_income)

EMR_inc_HIV <- lmer(log_inc_EMR ~ log_HIV_EMR +
                         (1|country), data=tbdata_EMR, REML = FALSE)
summary(EMR_inc_HIV)
check_model(EMR_inc_HIV)

EMR_inc_mal <- lmer(log_inc_EMR ~`Malnutrition_prev ` +
                      (1|country), data=tbdata_EMR, REML = FALSE)
summary(EMR_inc_mal)
check_model(EMR_inc_mal)

#Multivariate EMR Incidence Model

EMR_inc_multi <- lmer(log_inc_EMR ~ `Malnutrition_prev ` + log_HIV_EMR + DM_prev + log_income +
                        (1|country), data=tbdata_EMR, REML = FALSE)
summary(EMR_inc_multi)
check_model(EMR_inc_multi)


#Univariate EMR Mortality Models

EMR_mort_DM <- lmer(log_mort_EMR ~ DM_prev +
                     (1|country), data=tbdata_EMR, REML = FALSE)
summary(EMR_mort_DM)
check_model(EMR_mort_DM)

EMR_mort_income <- lmer(log_mort_EMR ~ log_income +
                      (1|country), data=tbdata_EMR, REML = FALSE)
summary(EMR_mort_income)
check_model(EMR_mort_income)

EMR_mort_HIV <- lmer(log_mort_EMR ~ log_HIV_EMR +
                          (1|country), data=tbdata_EMR, REML = FALSE)
summary(EMR_mort_HIV)
check_model(EMR_mort_HIV)

EMR_mort_mal <- lmer(log_mort_EMR ~ `Malnutrition_prev ` +
                       (1|country), data=tbdata_EMR, REML = FALSE)
summary(EMR_mort_mal)
check_model(EMR_mort_mal)


#Multivariate EMR Mort Model

EMR_mort_multi <- lmer(log_mort_EMR ~ `Malnutrition_prev ` + log_HIV_EMR + DM_prev + log_income +
                        (1|country), data=tbdata_EMR, REML = FALSE)
summary(EMR_mort_multi)
check_model(EMR_mort_multi)


#European Region 
#Univariate Incidence Models 
tbdata_EUR <- tbdata_EUR %>% 
  mutate(log_inc_EUR = log(e_inc_100k)) %>% 
  mutate(log_DM_EUR = log(DM_prev)) %>% 
  mutate(log_mal_EUR = log(`Malnutrition_prev `)) %>% 
  mutate(log_HIV_EUR = log(HIV_inc)) %>% 
  mutate(log_income = log(income_cap)) %>% 
  mutate (log_mort_EUR = log(e_mort_100k)) %>% 
  mutate(logit_CFR = logit(case_fatality))


EUR_inc_DM <- lmer(log_inc_EUR ~ DM_prev +
                     (1|country), data=tbdata_EUR, REML = FALSE)
summary(EUR_inc_DM)
check_model(EUR_inc_DM)

EUR_inc_income <- lmer(log_inc_EUR ~ log_income +
                     (1|country), data=tbdata_EUR, REML = FALSE)
summary(EUR_inc_income)
check_model(EUR_inc_income)

EUR_inc_HIV <- lmer(log_inc_EUR ~ HIV_inc +
                         (1|country), data=tbdata_EUR, REML = FALSE)
summary(EUR_inc_HIV)
check_model(EUR_inc_HIV)



EUR_inc_Mal <- lmer(log_inc_EUR ~  + `Malnutrition_prev ` +
                      (1|country), data=tbdata_EUR, REML = FALSE)
summary(EUR_inc_Mal)
check_model(EUR_inc_Mal)

#European Multivariate Incidence Model 

EUR_inc_multi <- lmer(log_inc_EUR ~  + `Malnutrition_prev ` + DM_prev + log_HIV_EUR +
                      log_income +
                      (1|country), data=tbdata_EUR, REML = FALSE)
summary(EUR_inc_multi)
check_model(EUR_inc_multi)

anova(EUR_inc_multi)

#European Univariate Mortality Models 

EUR_mort_DM <- lmer(log_mort_EUR ~ DM_prev +
                     (1|country), data=tbdata_EUR, REML = FALSE)
summary(EUR_mort_DM)
check_model(EUR_mort_DM)

EUR_mort_income <- lmer(log_mort_EUR ~ log_income +
                      (1|country), data=tbdata_EUR, REML = FALSE)
summary(EUR_mort_income)
check_model(EUR_mort_income)


EUR_mort_HIV <- lmer(log_mort_EUR ~ HIV_inc +
                          (1|country), data=tbdata_EUR, REML = FALSE)
summary(EUR_mort_HIV)
check_model(EUR_mort_HIV)

EUR_mort_mal <- lmer(log_mort_EUR ~  + `Malnutrition_prev `+
                       (1|country), data=tbdata_EUR, REML = FALSE)
summary(EUR_mort_mal)
check_model(EUR_mort_mal)


#Multivariate European Mortality Model 

EUR_mort_multi <- lmer(log_mort_EUR ~  + `Malnutrition_prev `+ DM_prev + HIV_inc + log_income +
                       (1|country), data=tbdata_EUR, REML = FALSE)
summary(EUR_mort_multi)
check_model(EUR_mort_multi)

#Univariate European CFR Models 
EUR_CFR_DM <- lmer(logit_CFR ~ DM_prev +
                      (1|country), data=tbdata_EUR, REML = FALSE)
summary(EUR_CFR_DM)
check_model(EUR_CFR_DM)

EUR_CFR_income <- lmer(logit_CFR ~ log_income +
                     (1|country), data=tbdata_EUR, REML = FALSE)
summary(EUR_CFR_income)
check_model(EUR_CFR_income)

EUR_CFR_HIV <- lmer(logit_CFR ~ HIV_inc +
                         (1|country), data=tbdata_EUR, REML = FALSE)
summary(EUR_CFR_HIV)
check_model(EUR_CFR_HIV)

#Checked log HIV and model is still poorly specified so sticking w/ regular HIV

EUR_CFR_mal <- lmer(logit_CFR ~  + `Malnutrition_prev `+
                      (1|country), data=tbdata_EUR, REML = FALSE)
summary(EUR_CFR_mal)
check_model(EUR_CFR_mal)

#European CFR multivariable model

EUR_CFR_multi <- lmer(logit_CFR ~  + `Malnutrition_prev `+ DM_prev + HIV_inc + log_income +
                      (1|country), data=tbdata_EUR, REML = FALSE)
summary(EUR_CFR_multi)
check_model(EUR_CFR_multi)

EUR_CFR_multi_noDM  <- lmer(logit_CFR ~  + `Malnutrition_prev `+ HIV_inc + log_income +
                        (1|country), data=tbdata_EUR, REML = FALSE)
summary(EUR_CFR_multi_noDM)
check_model(EUR_CFR_multi_noDM)

#South East Asia Incidence Univariate Models 

tbdata_SEA <- tbdata_SEA %>% 
  mutate(log_inc_SEA = log(e_inc_100k)) %>% 
  mutate(log_DM_SEA = log(DM_prev)) %>% 
  mutate(log_mal_SEA = log(`Malnutrition_prev `)) %>% 
  mutate(log_HIV_SEA = log(HIV_inc)) %>% 
  mutate(log_income = log(income_cap)) %>% 
  mutate (log_mort_SEA = log(e_mort_100k)) %>% 
  mutate(logit_CFR = logit(case_fatality))

tbdata_SEA_nomyan <- tbdata_SEA %>% 
  filter(!grepl('Myanmar', country))

SEA_inc_DM <- lmer(log_inc_SEA ~ DM_prev +
                     (1|country), data=tbdata_SEA, REML = FALSE)
summary(SEA_inc_DM)
check_model(SEA_inc_DM)

SEA_inc_income <- lmer(log_inc_SEA ~ log_income +
                     (1|country), data=tbdata_SEA_nomyan, REML = FALSE)
summary(SEA_inc_income)
check_model(SEA_inc_income)

SEA_inc_HIV <- lmer(log_inc_SEA ~ HIV_inc +
                     (1|country), data=tbdata_SEA, REML = FALSE)
summary(SEA_inc_HIV)
check_model(SEA_inc_HIV)
#Even worse diagnostics with log(HIV), will stick w/ normal HIV

SEA_inc_mal <- lmer(log_inc_SEA ~ `Malnutrition_prev ` +
                      (1|country), data=tbdata_SEA, REML = FALSE)
summary(SEA_inc_mal)
check_model(SEA_inc_mal)

#Multvariate SEA model for incidence 

SEA_inc_multi <- lmer(log_inc_SEA ~ log_income + DM_prev + `Malnutrition_prev ` + HIV_inc +
                         (1|country), data=tbdata_SEA_nomyan, REML = FALSE)
summary(SEA_inc_multi)
check_model(SEA_inc_multi)

#Checking w/ out mal and DM to see if diagnostics improve, although diagnostics are pleasantly surpising here
SEA_inc_multi_noDM_mal <- lmer(log_inc_SEA ~ log_income + HIV_inc +
                        (1|country), data=tbdata_SEA_nomyan, REML = FALSE)
check_model(SEA_inc_multi_noDM_mal)
#Diagnostics are worse w/ out DM and Mal so keeping them
#Univariate SEA models for mortality 
SEA_mort_DM <- lmer(log_mort_SEA ~ DM_prev +
                     (1|country), data=tbdata_SEA, REML = FALSE)
summary(SEA_mort_DM)
check_model(SEA_mort_DM)

SEA_mort_income <- lmer(log_mort_SEA ~ log_income +
                         (1|country), data=tbdata_SEA_nomyan, REML = FALSE)
summary(SEA_mort_income)
check_model(SEA_mort_income)

SEA_mort_HIV <- lmer(log_mort_SEA ~ HIV_inc +
                      (1|country), data=tbdata_SEA, REML = FALSE)
summary(SEA_mort_HIV)
check_model(SEA_mort_HIV)

SEA_mort_mal <- lmer(log_mort_SEA ~ `Malnutrition_prev ` +
                       (1|country), data=tbdata_SEA, REML = FALSE)
summary(SEA_mort_mal)
check_model(SEA_mort_mal)

#Multivariate SEA Model for Mortality 
SEA_mort_multi <- lmer(log_mort_SEA ~ log_income + DM_prev + `Malnutrition_prev ` + HIV_inc +
                        (1|country), data=tbdata_SEA_nomyan, REML = FALSE)
summary(SEA_mort_multi)
check_model(SEA_mort_multi)

#Univariate SEA models for CFR
SEA_CFR_DM <- lmer(logit_CFR ~ DM_prev +
                      (1|country), data=tbdata_SEA, REML = FALSE)
summary(SEA_CFR_DM)
check_model(SEA_CFR_DM)

SEA_CFR_income <- lmer(logit_CFR~ log_income +
                          (1|country), data=tbdata_SEA_nomyan, REML = FALSE)
summary(SEA_CFR_income)
check_model(SEA_CFR_income)

SEA_CFR_HIV <- lmer(logit_CFR ~ HIV_inc +
                     (1|country), data=tbdata_SEA, REML = FALSE)
summary(SEA_CFR_HIV)
check_model(SEA_CFR_HIV)

SEA_CFR_mal <- lmer(logit_CFR ~ `Malnutrition_prev ` +
                      (1|country), data=tbdata_SEA, REML = FALSE)
summary(SEA_CFR_mal)
check_model(SEA_CFR_mal)

#Multivariate SEA CFR model

SEA_CFR_multi <- lmer(logit_CFR~ log_income + DM_prev + log_income + HIV_inc + `Malnutrition_prev ` +
                         (1|country), data=tbdata_SEA_nomyan, REML = FALSE)
summary(SEA_CFR_multi)
check_model(SEA_CFR_multi)


#Western Pacific Region Models 
#WPR Univariate Incidence Models
tbdata_WPR <- tbdata_WPR %>% 
  mutate(log_inc_WPR = log(e_inc_100k)) %>% 
  mutate(log_DM_WPR = log(DM_prev)) %>% 
  mutate(log_mal_WPR = log(`Malnutrition_prev `)) %>% 
  mutate(log_HIV_WPR = log(HIV_inc)) %>% 
  mutate(log_income = log(income_cap)) %>% 
  mutate (log_mort_WPR = log(e_mort_100k)) %>% 
  mutate(logit_CFR = logit(case_fatality))

WPR_inc_DM <- lmer(log_inc_WPR ~ DM_prev +
                     (1|country), data=tbdata_WPR, REML = FALSE)
summary(WPR_inc_DM)
check_model(WPR_inc_DM)

WPR_inc_income <- lmer(log_inc_WPR ~ log_income +
                     (1|country), data=tbdata_WPR, REML = FALSE)
summary(WPR_inc_income)
check_model(WPR_inc_income)

WPR_inc_HIV <- lmer(log_inc_WPR ~ HIV_inc +
                         (1|country), data=tbdata_WPR, REML = FALSE)
summary(WPR_inc_HIV)
check_model(WPR_inc_HIV)

WPR_inc_mal <- lmer(log_inc_WPR ~ `Malnutrition_prev ` +
                      (1|country), data=tbdata_WPR, REML = FALSE)
summary(WPR_inc_mal)
check_model(WPR_inc_mal)

#WPR Multivariate Model for Incidence 

WPR_inc_multi <- lmer(log_inc_WPR ~ `Malnutrition_prev ` + DM_prev + log_income + HIV_inc +
                      (1|country) + (1+year), data=tbdata_WPR, REML = FALSE)
summary(WPR_inc_multi)
check_model(WPR_inc_multi)

#WPR Univariate models for mortality 
WPR_mort_DM <- lmer(log_mort_WPR ~ DM_prev +
                     (1|country), data=tbdata_WPR, REML = FALSE)
summary(WPR_mort_DM)
check_model(WPR_mort_DM)

WPR_mort_income <- lmer(log_mort_WPR ~ log_income +
                      (1|country), data=tbdata_WPR, REML = FALSE)
summary(WPR_mort_income)
check_model(WPR_mort_income)

WPR_mort_HIV <- lmer(log_mort_WPR ~ HIV_inc +
                          (1|country), data=tbdata_WPR, REML = FALSE)
summary(WPR_mort_HIV)
check_model(WPR_mort_HIV)

WPR_mort_mal <- lmer(log_mort_WPR ~ `Malnutrition_prev ` +
                       (1|country), data=tbdata_WPR, REML = FALSE)
summary(WPR_mort_mal)
check_model(WPR_mort_mal)

#WPR multivariate mortality model
WPR_mort_multi  <- lmer(log_mort_WPR ~ `Malnutrition_prev ` + DM_prev + HIV_inc + log_income +
                       (1|country), data=tbdata_WPR, REML = FALSE)
summary(WPR_mort_multi)
check_model(WPR_mort_multi)

#WPR Univariate CFR models
WPR_CFR_DM <- lmer(logit_CFR ~ DM_prev +
                      (1|country), data=tbdata_WPR, REML = FALSE)
summary(WPR_CFR_DM)
check_model(WPR_CFR_DM)

WPR_CFR_income <- lmer(logit_CFR ~ log_income +
                     (1|country), data=tbdata_WPR, REML = FALSE)
summary(WPR_CFR_income)
check_model(WPR_CFR_income)

WPR_CFR_HIV <- lmer(logit_CFR ~ HIV_inc +
                         (1|country), data=tbdata_WPR, REML = FALSE)
summary(WPR_CFR_HIV)
check_model(WPR_CFR_HIV)

WPR_CFR_mal <- lmer(logit_CFR ~  +`Malnutrition_prev ` +
                      (1|country), data=tbdata_WPR, REML = FALSE)
summary(WPR_CFR_mal)
check_model(WPR_CFR_mal)

#Multivariate WPR CFR Model


WPR_CFR_multi <- lmer(logit_CFR ~  +`Malnutrition_prev ` + DM_prev + HIV_inc + income_cap +
                      (1|country), data=tbdata_WPR, REML = FALSE)
summary(WPR_CFR_multi)
check_model(WPR_CFR_multi)

citation("tidyverse")
citation("ggplot2")
citation("readxl")
citation("lcsm")
citation("lme4")
citation("lmerTest")
citation("performance")
citation("secr")
citation("MASS")

WPR_inc_multi <- lmer(log_inc_WPR ~ `Malnutrition_prev ` + DM_prev + log_income + HIV_inc +
                        (1|country), data=tbdata_WPR, REML = FALSE)
log(2102)

pred_em <- predict(WPR_inc_multi, newdata=data.frame(HIV_inc = 9, 
                                                     log_income=7.65, DM_prev=15.4))

pred_em <- predict(WPR_inc_multi, newdata=data.frame(HIV_inc = 9, log_income=7.65, DM_prev=15.4), 
                   `Malnutrition_prev ` = 21.6, interval='confidence', level=0.95)
predict(WPR_inc_multi)
pred_ex <- predict(WPR_mort_multi, newdata=data.frame(HIV_inc = 9, log_income=7.65, DM_prev=15.4), 
                    interval='confidence', level=0.95)
pred_ex <- predict(AFR_inc_multi, newdata=data.frame(HIV_inc=9))

exp(5.0125)
pred_AFR <- predict(AFR_inc_multi, level=0, AFR_data=data.frame(log_HIV_AFR = 4.45, log_income = 7.01, 
                                                       DM_prev = 4, `Malnutrition_prev ` = 20.8,
                                                       re.form=~(1|country)))

predict.

summary(AFR_inc_multi)

lme4:::predict.merMod(pred_AFR <- predict(AFR_inc_multi, level=0, AFR_data=data.frame(log_HIV_AFR = 4.45, log_income = 7.01, 
                                                                                      DM_prev = 4, `Malnutrition_prev ` = 20.8,
                                                                                      re.form=~0)))

log(86)
log (1104)
