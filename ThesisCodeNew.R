#####################

getwd()
setwd("/Users/user/Desktop/Thesis")
library(readxl)
original <- read_excel("BDD_all_NEW.xlsx")


##########################################################
#####################    Packages    #####################
##########################################################

library(GGally) # visualization packages
library(dplyr)
library(gridExtra)
library(ggpubr)
library(lpirfs) # Local projection of impulse response function
library(robustbase) # This is needed for lmrob, robust regression
library(TSA)
library(strucchange)
library(corrplot)

##########################################################
#####################    Data Frames    ##################
##########################################################

new <- original
new <- cbind(new, data.frame(counties = rep(c(
  67, 29, 15, 75, 58, 64, 8, 1, 3, 67, 159, 1, 5, 44, 102, 92, 99, 105, 120, 64,
  16, 24, 14, 83, 87, 82, 114, 56, 93, 17, 10, 21, 33, 62, 100, 53, 88, 77, 36, 67,
  78, 5, 46, 66, 95, 254, 29, 14, 3, 95, 39, 55, 72, 23
), each = 360)))
new$counties <- new$counties*new$disaster_pchit_NEW
new$disastears <- ifelse(new$dumdeaths !=0 | new$dumcost_NEW !=0, 1,
                         ifelse(new$counties != 0,1,0))

aggdata <- aggregate(cbind(dumdeaths, dumcost_NEW, disastears, counties,disaster_pchit_NEW) ~ date,
                     data = new, FUN = sum, na.rm = TRUE)
colnames(aggdata) <- c("Date","Casualties","Financial_losses","Disasters","Affected_Counties")
aggdata_wo_counties <- aggregate(cbind(dumdeaths, dumcost_NEW, disasters, disaster_pchit_NEW) ~ date,
                                 data = new, FUN = sum, na.rm = TRUE)
stateagg <- aggregate(cbind(dumdeaths, dumcost_NEW, disasters, counties) ~ state,
                      data = new, FUN = sum, na.rm = TRUE)

aggdata2 <- cbind(aggdata, IPI) 

IPI <- read_excel("INDPRO.xlsx",sheet = 2)
aggdata2 <- aggdata2[,-7]

###############################################################
#####################    Preliminary analysis  ################
###############################################################

cor(aggdata) # High correlation between costs and deaths, moderate correlation 
             # between disastears deaths and costs, similar with counties.
             # Almost perfect correlation with disastears due to Ifelse statment
M = cor(aggdata)
corrplot(M, method = 'color') # colorful number

?lm # Disastears count proves to be significant regressors in both cases,
#albite explaining more variability in the case of costs 11% compared to 5% in deaths
model <- lm(Casualties~Affected_Counties+Disasters,data = aggdata)
model$coefficients
summary(model)

model2 <- lm(Financial_losses~Affected_Counties+Disasters,data = aggdata)
model2$coefficients
summary(model2)

model3 <- lm(Casualties~Affected_Counties,data = aggdata)
model3$coefficients
summary(model3)

model4 <- lm(Financial_losses~Affected_Counties,data = aggdata) 
model4$coefficients
summary(model4)

model5 <- lm(Casualties~Disasters,data = aggdata)
model5$coefficients
summary(model5)

model6 <- lm(Financial_losses~Disasters,data = aggdata) 
model6$coefficients
summary(model6)

model7 <- lm(INDPRO~Disasters,data = aggdata2 )
model7$coefficients
summary(model7)

par(mfrow = c(3,2))  # Set 2x2 plotting layout
y_s <- colnames(aggdata2)[-c(1,4,5,6)]  # Response variables
y_s
for(i in seq_along(y_s)) { # We see similiar patterns in all four graphs 
 # with couple of possible outliears. Their significance will be seen on other graps.
                           # Choice of outlier bounds is based on the rule of thumb

  # Identify outliers for y_s[i]
  mean_val <- mean(aggdata2[[y_s[i]]], na.rm = TRUE)
  sd_val <- sd(aggdata2[[y_s[i]]], na.rm = TRUE)
  outliers <- aggdata2[[y_s[i]]] > mean_val + 6 * sd_val | aggdata2[[y_s[i]]] < mean_val - 3 * sd_val
  
  # FIRST PLOT: Disasters vs. y_s[i]
  plot(aggdata2$Disasters, aggdata2[[y_s[i]]], main = paste(y_s[i], "vs. Disasters"), 
       xlab = "Disasters", ylab = y_s[i], col = ifelse(outliers, "red", "black"), pch = 19)
  
  model <- lm(as.formula(paste(y_s[i], "~ Disasters")), data = aggdata2)
  
  newx <- data.frame(Disasters = seq(min(aggdata2$Disasters), max(aggdata2$Disasters),
                                      length.out = 100))
  preds <- predict(model, newdata = newx, interval = "confidence")
  
  lines(newx$Disasters, preds[,1], col = "black")  # Regression line
  lines(newx$Disasters, preds[,3], lty = "dashed", col = "blue")  # Upper CI
  lines(newx$Disasters, preds[,2], lty = "dashed", col = "blue")  # Lower CI
  
  # SECOND PLOT: counties vs. y_s[i]
  plot(aggdata2$Affected_Counties, aggdata2[[y_s[i]]], main = paste(y_s[i], "vs. Affected Counties"), 
       xlab = "Affected_Counties", ylab = y_s[i], col = ifelse(outliers, "red", "black"), pch = 19)
  
  model1 <- lm(as.formula(paste(y_s[i], "~ Affected_Counties")), data = aggdata2)
  
  newx1 <- data.frame(Affected_Counties = seq(min(aggdata2$Affected_Counties),
                                              max(aggdata2$Affected_Counties), length.out = 100))
  preds1 <- predict(model1, newdata = newx1, interval = "confidence")
  
  lines(newx1$Affected_Counties, preds1[,1], col = "black")  # Regression line
  lines(newx1$Affected_Counties, preds1[,3], lty = "dashed", col = "red")  # Upper CI
  lines(newx1$Affected_Counties, preds1[,2], lty = "dashed", col = "red")  # Lower CI
  
}


## We see outliers in the data
par(mfrow=c(2,2)) # We spot the observation 188, hurricane Katrina which is influential
#in all scenarios, we should deal with it through robust regression
plot(model)
plot(model2)
plot(model3)
plot(model4)
plot(model5)
plot(model6)

aggdata[188,]


# We do robust regression and apply MM estimator, we decide to focus only on
#disastears as counties seems to be closly releted and present similiar results
lm3mm <- lmrob(aggdata2[[y_s[1]]]~ Disasters, data = aggdata2)
lm3mm$coefficients
summary(lm3mm)
lm3mm2 <- lmrob(aggdata2[[y_s[2]]]~ Disasters, data = aggdata2)
lm3mm2$coefficients
summary(lm3mm2)
lm3mm3 <- lmrob(aggdata2[[y_s[1]]]~ Disasters+Affected_Counties, data = aggdata2)
lm3mm3$coefficients
summary(lm3mm3)
lm3mm4 <- lmrob(aggdata2[[y_s[2]]]~ Disasters+Affected_Counties, data = aggdata2)
lm3mm4$coefficients
summary(lm3mm4)

# Running the LOO-CV, to compare our regular lm model with robus model based on error
# We decide to stick with robust regression, based on the l1 absolute loss,
#which is less sensitive to outliers.
n <- 360
prediction_matrix1 <- matrix(data =NA,nrow = 360, ncol = 3,
                             dimnames=list(c(),c("Prediction of Costs","Prediction of Deaths", "Prediction of Industrial Production Index")))
yhat <- numeric(n)
for(j in seq_along(y_s)) {
  for (i in 1:n){
    lm3mm <- lmrob(as.formula(paste(y_s[j], "~ Disasters")),
                   data = aggdata2[-i,],control = lmrob.control(init = "lts")) # Fit without point i
    yhat[i] <- predict(lm3mm,aggdata2[i,, drop = F]) # Prediction of y_i
    prediction_matrix1[i,j] <- yhat[i]
  }
  sqloss1 <- sqrt(mean((yhat-aggdata2[[y_s[j]]])^2))
  l1loss1 <- mean(abs(yhat-aggdata2[[y_s[j]]]))
  print(paste("For", y_s[j], "SQLoss:", sqloss1, "L1Loss:", l1loss1))
}
x <- seq(1,360,1)
n <- 360 # Number of observations
prediction_matrix2 <- matrix(data =NA,nrow = 360, ncol = 3, dimnames=list(c(),
                                                  c("Prediction of Costs","Prediction of Deaths","Prediction of Industrial Production Index")))
yhat <- numeric(n)
for(j in seq_along(y_s)) {
  for (i in 1:n){
    fmi <- lm(as.formula(paste(y_s[j], "~ Disasters")), data = aggdata2[-i,]) # Fit without point i
    yhat[i] <- predict(fmi,aggdata2[i,, drop = F]) # Prediction of y_i
    prediction_matrix2[i,j] <- yhat[i]
  }
  sqloss <- sqrt(mean((yhat-aggdata2[[y_s[j]]])^2))
  l1loss <- mean(abs(yhat-aggdata2[[y_s[j]]]))
  print(paste("For", y_s[j], "SQLoss:", sqloss, "L1Loss:", l1loss))
}


#Load required package
library(xtable)
# Compute SQLoss and L1Loss and store them in a dataframe

loss_results <- data.frame(
  Metric = c("SQLoss␣(Robust)", "L1Loss␣(Robust)", "SQLoss␣(OLS)", "L1Loss␣(OLS)"), Prediction_of_Costs = c(
    sqrt(mean((prediction_matrix1[,1] - aggdata[[y_s[1]]])^2)), mean(abs(prediction_matrix1[,1] - aggdata[[y_s[1]]])), sqrt(mean((prediction_matrix2[,1] - aggdata[[y_s[1]]])^2)), mean(abs(prediction_matrix2[,1] - aggdata[[y_s[1]]]))
  ),
  Prediction_of_Deaths = c(
    sqrt(mean((prediction_matrix1[,2] - aggdata[[y_s[2]]])^2)), mean(abs(prediction_matrix1[,2] - aggdata[[y_s[2]]])), sqrt(mean((prediction_matrix2[,2] - aggdata[[y_s[2]]])^2)), mean(abs(prediction_matrix2[,2] - aggdata[[y_s[2]]]))
  ) )
#
#Convert dataframe to LaTeX table
latex_table <- xtable(loss_results, caption = "Cross-Validation␣Loss␣Metrics") # Save to a .tex file
print(latex_table, file = "cross_validation_results.tex", include.rownames = FALSE)

#Prediction
par(mfrow =c(1,3))
plot(aggdata2$Date, prediction_matrix1[,1], ylab =y_s[1] ,
     main = "Comparison of model predictions for deaths", col = "green")
points(aggdata2$Date,aggdata2$Casualties , col = "red")
points(aggdata2$Date,prediction_matrix2[,1], col = "purple")
legend("topright", legend = c("True values", "Robust model", "Linear model"),
       col = c("red", "green", "purple"), pch = c(1, 1, 1))

plot(aggdata2$Date, prediction_matrix1[,2], ylab =y_s[2] ,
     main = "Comparison of model predictions for costs", col = "green")
points(aggdata2$Date,aggdata2$Financial_losses, col = "red")
points(aggdata2$Date,prediction_matrix2[,2], col = "purple")
legend("topright", legend = c("True values", "Robust model", "Linear model"),
       col = c("red", "green", "purple"), pch = c(1, 1, 1))

plot(aggdata2$Date, prediction_matrix1[,3], ylab =y_s[3] ,
     main = "Comparison of model predictions for IPI", col = "green")
points(aggdata2$Date,aggdata2$INDPRO , col = "red")
points(aggdata2$Date,prediction_matrix2[,3], col = "purple")
legend("topright", legend = c("True values", "Robust model", "Linear model"),
       col = c("red", "green", "purple"), pch = c(1, 1, 1))

# We conclude that the better models of two are robust models


################################################################################
##############  Preliminary Time Series Analysis ###############################
################################################################################

ts.plot(aggdata$Financial_losses, gpars = list(xlab = "Time", ylab = " Costs"))
ts.plot(aggdata$Casualties, gpars = list(xlab = "Time", ylab = " Deaths"))
acf(aggdata$Financial_losses)
# We observe no trands in our data


################################################################################
##############   Local Projections & Impulse Response Functions ################
################################################################################

####################################################
endog_data <- aggdata2[, c( "Disastears","Casualties", "Financial_losses","INDPRO")]
switching_data <- as.data.frame(read_excel("UNRATE.xlsx", sheet = 2)[, -1])
switching_data <- ifelse(switching_data$UNRATE > 6,1,0)
table(switching_data)
# Estimate linear model

# Create an empty vector to store BIC values
bic_values <- numeric(12)

# Loop over 12 lags and compute BIC for each
for (i in 1:12) {
  results_lin <- lp_lin(endog_data = endog_data,
                        lags_endog_lin = i,
                        trend = 0,
                        shock_type = 1,
                        confint = 1.96,
                        hor = 12)
  
  # Compute BIC
  ssr <- sum(results_lin$irf_lin_mean^2)  # Sum of Squared Residuals
  n <- length(results_lin$irf_lin_mean)   # Number of Observations
  k <- i + 1  # Number of parameters (lags + intercept)
  
  bic_values[i] <- log(ssr/n) + k * log(n) / n
}

# Print BIC values for each lag
print(bic_values)
min(bic_values)

# Find the best lag (minimum BIC)
best_lag <- which.min(bic_values)
print(paste("Best lag based on BIC:", best_lag))


bic_values2 <- numeric(12)

# Loop over 12 lags and compute BIC for each
for (i in 1:12) {
  results_nl <- lp_nl(endog_data,
                      switching = switching_data,
                      lags_endog_lin = i,
                      lags_endog_nl = i,
                      trend = 0,
                      confint = 1.67,
                      hor = 12,
                      use_logistic = FALSE,
                      lag_switching = FALSE,
                      shock_type = 1  # Apply a unit shock to Disastears
  )
  # Compute BIC
  ssr <- sum(results_lin$irf_lin_mean^2)  # Sum of Squared Residuals
  n <- length(results_lin$irf_lin_mean)   # Number of Observations
  k <- i + 1  # Number of parameters (lags + intercept)
  
  bic_values2[i] <- log(ssr/n) + k * log(n) / n
}

# Print BIC values for each lag
print(bic_values2)
min(bic_values2)

# Find the best lag (minimum BIC)
best_lag <- which.min(bic_values2)
print(paste("Best lag based on BIC:", best_lag))


bic_values3 <- numeric(12)

# Loop over 12 lags and compute BIC for each
for (i in 1:12) {
  endog_data <- aggdata[,c(2,3,4), drop = F]
  shock <- aggdata[,4, drop = F]
  results_lin_iv <- lp_lin_iv(endog_data = endog_data, lags_endog_lin = i,
                              shock = shock, trend = 0,
                              confint = 1.96, hor = 20)
  
  # Compute BIC
  ssr <- sum(results_lin$irf_lin_mean^2)  # Sum of Squared Residuals
  n <- length(results_lin$irf_lin_mean)   # Number of Observations
  k <- i + 1  # Number of parameters (lags + intercept)
  
  bic_values3[i] <- log(ssr/n) + k * log(n) / n
}

# Print BIC values for each lag
print(bic_values3)
min(bic_values3)

# Find the best lag (minimum BIC)
best_lag <- which.min(bic_values3)

print(paste("Best lag based on BIC:", best_lag))


#installed.packages(install.packages("lpirfs", lib="/Library/Frameworks/R.framework/Versions/4.2/Resources/library"))
# Due to non existance of cycles in disastears costs and deaths we use linear local projections

# --- Code to replicate Figure 5 in Jordá (2005, p. 176)

# Estimate linear model
results_lin <- lp_lin(endog_data = endog_data,
                      lags_endog_lin = 9,
                      trend = 0,
                      shock_type = 1,
                      confint = 1.96,
                      hor = 13)
# Show impulse responses
plot(results_lin)
# Show OLS diagnostics for the first shock of the first horizon
summary(results_lin)
summary(results_lin)
# --- End example code

# According to these tests there is non-linear effect and we should model for it
sctest(aggdata2$Financial_losses ~ aggdata2$Disastears, type = "Chow")
sctest(aggdata2$Casualties ~ aggdata2$Disastears, type = "Chow")
sctest(aggdata2$INDPRO ~ aggdata2$Disastears, type = "Chow")


# The example above only estimates impulse responses for the linear case, 
#and my miss out on the non linear effect disastears have on costs and deaths
# --- Code for nonlinear effects of disastears.


endog_data <- aggdata2[, c( "Disasters","Casualties", "Financial_losses","INDPRO")]
switching_data <- as.data.frame(read_excel("UNRATE.xlsx", sheet = 2)[, -1])

library(mFilter)
unemp_gap <- hpfilter(switching_data, freq = 10000)$cycle
switching <- ifelse(unemp_gap > 0, 1, 0)

 
switching <- ifelse(unemp_gap >average_12_months, 1,0)
library(zoo)
unemp_rate <- as.data.frame(read_excel("UNRATE.xlsx", sheet = 2)[, -1])

unrate_3mo_avg <- rollmean(unemp_rate, k = 3, align = "right", fill = NA)
# Step 2: Compute rolling 12-month minimum of 3-month averages
min_12mo <- rollapply(unrate_3mo_avg, width = 12, FUN = min, align = "right", fill = NA)
# Step 3: Calculate the Sahm Rule signal
recession_signal <- ifelse(unrate_3mo_avg - min_12mo >= 0.5,1,0)

table(recession_signal)
# We made sure that there is enough observations
# Estimate nonlinear model
results_nl <- lp_nl(endog_data,
  switching = switching_data,
  lags_endog_lin = 1,
  lags_endog_nl = 1,
  trend = 1,
  confint = 1.67,
  hor = 13,
  use_logistic = TRUE,
  gamma = 15,
  lag_switching = TRUE,
  shock_type = 1,
  use_hp          = TRUE,
  lambda = 10000# Apply a unit shock to Disastears
)
plot(results_nl)


results_nl <- lp_nl(endog_data,
                    switching = recession_signal,
                    lags_endog_lin = 1,
                    lags_endog_nl = 1,
                    trend = 0,
                    confint = 1.67,
                    hor = 13,
                    use_logistic = F,
                    lag_switching = F,
                    shock_type = 1,
                    use_hp          = F# Apply a unit shock to Disastears
)

nl_plots <- plot_nl(results_nl)
# Combine and show plots using 'ggpubr' and 'gridExtra'
single_plots      <- nl_plots$gg_s1[c(13, 14, 15)]
single_plots[4:6] <- nl_plots$gg_s2[c(13, 14, 15)]
all_plots         <- sapply(single_plots, ggplotGrob)
marrangeGrob(all_plots, nrow = 3, ncol = 2, top = NULL)   
# Assuming "Disastears" is variable 1 in endog_data
num_vars <- ncol(endog_data)
shock_pos <- 1  # position of "Disastears"

# Assuming you've already run your nonlinear estimation and gotten nl_plots from plot_nl():
nl_plots <- plot_nl(results_nl)

# Assuming you've already run your nonlinear estimation and gotten nl_plots from plot_nl():
nl_plots <- plot_nl(results_nl)

# Now, extract the INDPRO response for each shock in Regime 1 and Regime 2.
# For Regime 1:
single_plots <- nl_plots$gg_s1[c(13, 14, 15)]  
# For Regime 2, append them to positions 4-6 of single_plots:
single_plots[3:5] <- nl_plots$gg_s2[c(13, 14, 15)]

# Convert plots to grobs and arrange them in a grid:
library(ggplot2)
library(gridExtra)
all_plots <- sapply(single_plots, ggplotGrob)
marrangeGrob(all_plots, nrow = 3, ncol = 2, top = NULL)



cor(endog_data, shock, use = "complete.obs")
# Estimating linear model through instrumental variables, as our endogenous variable
#and shock variable is highly correleted it should work fine
# Endogenous data
endog_data <- aggdata2[,c(2,3,4,7), drop = F]
# Shock variable
shock <- aggdata2[,4, drop = F]
# Estimate linear model
results_lin_iv <- lp_lin_iv(endog_data = endog_data, lags_endog_lin = 1,
                            shock = shock, trend = 0,
                            confint = 1.96, hor = 20)
# Make and save linear plots
plot_lin(results_lin_iv)


#TS
install.packages("stargazer" )
library(stargazer)
stargazer(aggdata2, type = "latex", summary.stat = c("mean", "sd", "min", "median", "max"))


library(ggplot2)
ggplot(aggdata2, aes(x = Date)) +
  geom_line(aes(y = Disasters, color = "Disasteters")) +
  geom_line(aes(y = INDPRO, color = "Industrial Production")) +
  labs(title = "Time Series of Key Variables", y = "", x = "")

library(tseries)
library(xtable)
install.packages("remotes")
library(remotes)
remotes::install_github("gitronald/htester")
library(htester)
data(htests)
htest_data_frame(htests[["cor.test.pearson"]])
adf <- htest_data_frame(adf)

adf <- (adf.test(aggdata2$INDPRO))
latex_table <- xtable(adf, caption = "Augmented Dickey-Fuller test") # Save to a .tex file
print(latex_table, file = "Augmented Dickey-Fuller test", include.rownames = FALSE)

par(mfrow=c(2,1))
acf(aggdata2$INDPRO, main = "ACF - Industrial Production")
pacf(aggdata2$INDPRO, main = "PACF - Industrial Production")


# Calculate rolling 12-month standard deviation of INDPRO
rolling_sd <- rollapply(aggdata2$INDPRO, width = 12, FUN = sd, align = "right", fill = NA)

# Plot it
plot(rolling_sd, type = "l", main = "Rolling 12-Month SD of Industrial Production", ylab = "SD", xlab = "Time")

aggdata2 <- cbind(aggdata2, switching_data)
corrplot(aggdata2[, c("Financial_losses", "INDPRO", "Disastears", "Casualties","UNRATE")], use = "complete.obs")
library(psych)

corPlot(aggdata2[, c("Financial_losses", "INDPRO", "Disasters", "Casualties","UNRATE")])
dev.off()  # closes the active graphics device
