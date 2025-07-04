rm(list= ls())

directory <- "C:\\Users\\Asus\\Documents\\Surge 2024 summer\\1_2025" 
setwd(directory)
getwd()


install.packages("tidyverse")
install.packages("magrittr")
install.packages("stargazer")
install.packages("readxl")
install.packages("foreign")
install.packages("memisc")
install.packages("dplyr")
install.packages("labelled")
install.packages("expss")
install.packages("Hmisc")
install.packages("haven")
install.packages("rio")
install.packages("ggplot2")
install.packages("lessR")
install.packages("tvReg")
install.packages("plm")
install.packages("car")
install.packages("gplots")
install.packages("stargazer.rtf")
install.packages("gridExtra")


library(tidyverse)
library(magrittr)
library(stargazer)
library(readxl)
library(foreign)
library(memisc)
library(dplyr)
library(labelled)
library(expss)
library(Hmisc)
library(haven)
library(rio)
library(ggplot2)
library(lessR)
library(tvReg)
library(plm)
library(car)
library(gplots)
library(gridExtra)


mydata<-rio::import("C:\\Users\\Asus\\Documents\\Surge 2024 summer\\1_2025\\balanced_panel_data.xlsx")
attach(mydata)
names(mydata)


pdata<-pdata.frame(mydata,index=c("ifscode","year"))
is.pbalanced(pdata)

#vulnerability v/s time
summary_df <- mydata %>%
  group_by(year) %>%  
  summarise(
    median_vulnerability = median(vulnerability100, na.rm = TRUE),  
    lower_quantile = quantile(vulnerability100, 0.25, na.rm = TRUE),  
    upper_quantile = quantile(vulnerability100, 0.75, na.rm = TRUE)
  )


ggplot(summary_df, aes(x = year)) +
  geom_ribbon(aes(ymin = lower_quantile, ymax = upper_quantile), fill = "blue", alpha = 0.2) +  
  geom_line(aes(y = median_vulnerability), color = "red", size = 1) +  
  labs(title = "Vulnerability",
       x = "",
       y = "") +
  theme_minimal()

#resilience v/s time
summary_dfres <- mydata %>%
  group_by(year) %>%  
  summarise(
    median_res = median(readiness100, na.rm = TRUE),  
    lower_quantileres = quantile(readiness100, 0.25, na.rm = TRUE),  
    upper_quantileres = quantile(readiness100, 0.75, na.rm = TRUE)   
  )

ggplot(summary_dfres, aes(x = year)) +
  geom_ribbon(aes(ymin = lower_quantileres, ymax = upper_quantileres), fill = "blue", alpha = 0.2) +  
  geom_line(aes(y = median_res), color = "red", size = 1) +  
  labs(title = "Resilience",
       x = "",
       y = "") +
  theme_minimal()


pdf("intrographs.pdf")

print(resplot)
print(vulplot)

dev.off()

# Scatter plot of vulnerability100 and bond spreads
ggplot(mydata, aes(x = vulnerability100, y = bond_spreads)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Vulnerability vs Bond Spreads", x = "Vulnerability (100)", y = "Bond Spreads")

# Scatter plot of resilience100 and bond spreads
ggplot(mydata, aes(x = readiness100, y = bond_spreads)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Resilience vs Bond Spreads", x = "Resilience (100)", y = "Bond Spreads")

#Fitting the tvPLM model
tv_model <- tvPLM(bond_spreads ~ vulnerability100 + readiness100 +growth + inflation_cpi
                  + debt_gdp 
                  + ob_gdp+ reserves,
                  data = pdata, index = c("ifscode", "year"))

tv_model<-confint(tv_model)

#Extracting coefficients
tv_coefficients <- coef(tv_model)

# Extract fixed coefficients from plm
plm_coefficients <- coef(plm_model)


tv_coeff_df <- data.frame( tv_coefficients)
years<-1995:2015
tv_coeff_df$years<-years

library(ggplot2)

#plot of effect of vulnerability
ggplot(tv_coeff_df, aes(x = years, y = vulnerability100)) +
  geom_line() +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#fff5f2")) +
  labs(x = "Years", y = "Effect of Vulnerability")

#plot of effect of resilience
ggplot(tv_coeff_df, aes(x = years, y = readiness100)) +
  geom_line() +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#fff5f2")) +
  labs(x = "Years", y = "Effect of Reilience")


#plotting the temporal effect of each independent variable
plot1<-ggplot(tv_coeff_df, aes(x = years, y = vulnerability100)) +
  geom_line() +
  theme_minimal()

plot2<-ggplot(tv_coeff_df, aes(x = years, y = readiness100)) +
  geom_line() +
  theme_minimal()

plot3<-ggplot(tv_coeff_df, aes(x = years, y = growth)) +
  geom_line() +
    theme_minimal()

plot4<-ggplot(tv_coeff_df, aes(x = years, y = inflation_cpi)) +
  geom_line() +
  theme_minimal()

plot5<-ggplot(tv_coeff_df, aes(x = years, y = debt_gdp)) +
  geom_line() +
  theme_minimal()

plot6<-ggplot(tv_coeff_df, aes(x = years, y = ob_gdp)) +
  geom_line() +
  theme_minimal()

plot7<-ggplot(tv_coeff_df, aes(x = years, y = reserves)) +
  geom_line() +
  theme_minimal()


pdf("tvPLM_results_graphical.pdf")

print(plot1)
print(plot2)
print(plot3)
print(plot4)
print(plot5)
print(plot6)
print(plot7)

dev.off()

