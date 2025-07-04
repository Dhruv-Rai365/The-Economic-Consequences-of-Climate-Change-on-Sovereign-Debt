rm(list= ls())

directory <- "C:\\Users\\Asus\\Documents\\Surge 2024 summer\\1_2025" 
setwd(directory)
getwd()

needed <- c("readxl", "lmtest", "sandwich", "stargazer")
for(pkg in needed) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg)
  library(pkg, character.only = TRUE)
}
df <- readxl::read_excel("C:\\Users\\Asus\\Documents\\Surge 2024 summer\\1_2025\\panel_data.xlsx")

# because we use USA as a benchmarking country
df <- subset(df, ifscode != 111)


df$ifscode_f <- factor(df$ifscode)
df$year_f    <- factor(df$year)

fmla1 <- bond_spreads ~
  lnrgdp + growth + inflation_cpi + debt_gdp +
  OB_gdp  + reserves  + gee  + rqe +
  ifscode_f + year_f


fit1 <- lm(fmla1, data = df)

summary(fit1)
stargazer(fit1, type = "text", 
          title = "Bond Spreads Regression",
          dep.var.labels = "Bond Spreads",
          covariate.labels=c(),
          omit = c("ifscode", "year"), 
          digits = 4,
          star.cutoffs = c(0.11, 0.05, 0.01))
capture.output(
  stargazer::stargazer(fit1,
                       type = "text",
                       dep.var.labels = "Bond Spreads",
                       covariate.labels = c(
                         "ln(rGDP)", "Growth (%)", "Inflation (CPI)",
                         "Debt/GDP", "Budget Bal./GDP", "Reserves/GDP",
                         " Government effectiveness", "Bureaucratic quality"
                       ),
                       omit = c("ifscode", "year"),
                       digits = 4,
                       star.cutoffs = c(0.11, 0.05, 0.01)
  ),
  file = "model1.rtf"
)

fmla2 <- bond_spreads ~
  vulnerability100 + ifscode_f + year_f


fit2 <- lm(fmla2, data = df)

summary(fit2)
stargazer(fit2, type = "text", 
          title = "Bond Spreads Regression",
          dep.var.labels = "Bond Spreads",
          covariate.labels=c(),
          omit = c("ifscode", "year"), 
          digits = 4,
          star.cutoffs = c(0.11, 0.05, 0.01))
capture.output(
  stargazer::stargazer(fit2,
                       type = "text",
                       dep.var.labels = "Bond Spreads",
                       covariate.labels = c(
                         "Vulnerability (×100)", "Readiness (×100)",
                         "ln(rGDP)", "Growth (%)", "Inflation (CPI)",
                         "Debt/GDP", "Budget Bal./GDP", "Reserves/GDP",
                         " Government effectiveness", "Bureaucratic quality"
                       ),
                       omit = c("ifscode", "year"),
                       digits = 4,
                       star.cutoffs = c(0.11, 0.05, 0.01)
  ),
  file = "model2.rtf"
)

fmla3 <- bond_spreads ~
  readiness100 + ifscode_f + year_f


fit3 <- lm(fmla3, data = df)

summary(fit3)
stargazer(fit3, type = "text", 
          title = "Bond Spreads Regression",
          dep.var.labels = "Bond Spreads",
          covariate.labels=c(),
          omit = c("ifscode", "year"), 
          digits = 4,
          star.cutoffs = c(0.11, 0.05, 0.01))
capture.output(
  stargazer::stargazer(fit3,
                       type = "text",
                       dep.var.labels = "Bond Spreads",
                       covariate.labels = c(
                         "Vulnerability (×100)", "Readiness (×100)",
                         "ln(rGDP)", "Growth (%)", "Inflation (CPI)",
                         "Debt/GDP", "Budget Bal./GDP", "Reserves/GDP",
                         " Government effectiveness", "Bureaucratic quality"
                       ),
                       omit = c("ifscode", "year"),
                       digits = 4,
                       star.cutoffs = c(0.11, 0.05, 0.01)
  ),
  file = "model3.rtf"
)

fmla4 <- bond_spreads ~
  vulnerability100 + readiness100 +
  ifscode_f + year_f

fit4 <- lm(fmla4, data = df)

summary(fit4)
stargazer(fit4, type = "text", 
          title = "Bond Spreads Regression",
          dep.var.labels = "Bond Spreads",
          covariate.labels=c(),
          omit = c("ifscode", "year"), 
          digits = 4,
          star.cutoffs = c(0.11, 0.05, 0.01))

capture.output(
  stargazer::stargazer(fit4,
                       type = "text",
                       dep.var.labels = "Bond Spreads",
                       covariate.labels = c(
                         "Vulnerability (×100)", "Readiness (×100)",
                         "ln(rGDP)", "Growth (%)", "Inflation (CPI)",
                         "Debt/GDP", "Budget Bal./GDP", "Reserves/GDP",
                         " Government effectiveness", "Bureaucratic quality"
                       ),
                       omit = c("ifscode", "year"),
                       digits = 4,
                       star.cutoffs = c(0.11, 0.05, 0.01)
  ),
  file = "model4.rtf"
)

fmla5 <- bond_spreads ~
  vulnerability100 + lnrgdp + growth + inflation_cpi + debt_gdp +
  OB_gdp  + reserves  + gee  + rqe +
  ifscode_f + year_f

fit5 <- lm(fmla5, data = df)

summary(fit5)
stargazer(fit5, type = "text", 
          dep.var.labels = "Bond Spreads",
          covariate.labels=c(),
          omit = c("ifscode", "year"), 
          digits = 4,
          star.cutoffs = c(0.11, 0.05, 0.01))
capture.output(
  stargazer::stargazer(fit5,
                       type = "text",
                       dep.var.labels = "Bond Spreads",
                       covariate.labels = c(
                         "Vulnerability (×100)", "Readiness (×100)",
                         "ln(rGDP)", "Growth (%)", "Inflation (CPI)",
                         "Debt/GDP", "Budget Bal./GDP", "Reserves/GDP",
                         " Government effectiveness", "Bureaucratic quality"
                       ),
                       omit = c("ifscode", "year"),
                       digits = 4,
                       star.cutoffs = c(0.11, 0.05, 0.01)
  ),
  file = "model5.rtf"
)


fmla6 <- bond_spreads ~
  readiness100 + lnrgdp + growth + inflation_cpi + debt_gdp +
  OB_gdp  + reserves  + gee  + rqe +
  ifscode_f + year_f

fit6 <- lm(fmla6, data = df)

summary(fit6)
stargazer(fit6, type = "text", 
          title = "Bond Spreads Regression",
          dep.var.labels = "Bond Spreads",
          covariate.labels=c(),
          omit = c("ifscode", "year"), 
          digits = 4,
          star.cutoffs = c(0.11, 0.05, 0.01))
capture.output(
  stargazer::stargazer(fit6,
                       type = "text",
                       dep.var.labels = "Bond Spreads",
                       covariate.labels = c(
                         "Vulnerability (×100)", "Readiness (×100)",
                         "ln(rGDP)", "Growth (%)", "Inflation (CPI)",
                         "Debt/GDP", "Budget Bal./GDP", "Reserves/GDP",
                         " Government effectiveness", "Bureaucratic quality"
                       ),
                       omit = c("ifscode", "year"),
                       digits = 4,
                       star.cutoffs = c(0.11, 0.05, 0.01)
  ),
  file = "model6.rtf"
)

fmla7 <- bond_spreads ~
  vulnerability100 + readiness100 + lnrgdp + growth + inflation_cpi + debt_gdp +
  OB_gdp  + reserves  + gee  + rqe +
  ifscode_f + year_f

fit7 <- lm(fmla7, data = df)

summary(fit7)
stargazer(fit7, type = "text", 
          title = "Bond Spreads Regression",
          dep.var.labels = "Bond Spreads",
          covariate.labels=c(),
          omit = c("ifscode", "year"), 
          digits = 4,
          star.cutoffs = c(0.11, 0.05, 0.01))

capture.output(
  stargazer::stargazer(fit7,
                       type = "text",
                       dep.var.labels = "Bond Spreads",
                       covariate.labels = c(
                         "Vulnerability (×100)", "Readiness (×100)",
                         "ln(rGDP)", "Growth (%)", "Inflation (CPI)",
                         "Debt/GDP", "Budget Bal./GDP", "Reserves/GDP",
                         " Government effectiveness", "Bureaucratic quality"
                       ),
                       omit = c("ifscode", "year"),
                       digits = 4,
                       star.cutoffs = c(0.11, 0.05, 0.01)
  ),
  file = "model7.rtf"
)
