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


IPI <- read_excel("INDPRO.xlsx",sheet = 2)
aggdata2 <- cbind(aggdata, IPI) 
aggdata2 <- aggdata2[,-7]



################################################################################
##############  Preliminary Time Series Analysis ###############################
################################################################################


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


adf <- (adf.test(aggdata2$INDPRO))
adf <- htest_data_frame(adf)
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
library(psych)

corPlot(aggdata2[, c("Financial_losses", "INDPRO", "Disasters", "Casualties","UNRATE")])
dev.off()  # closes the active graphics device



################################################################################
##############   Local Projections & Impulse Response Functions ################
################################################################################

####################################################
endog_data <- aggdata2[, c( "Disasters","Casualties", "Financial_losses", "Affected_Counties","INDPRO")]
switching_data <- as.data.frame(read_excel("UNRATE.xlsx", sheet = 2)[, -1])
# Estimate linear model
library(zoo)
unemp_rate <- as.data.frame(read_excel("UNRATE.xlsx", sheet = 2)[, -1])
unrate_3mo_avg <- rollmean(unemp_rate, k = 3, align = "right", fill = NA)
# Step 2: Compute rolling 12-month minimum of 3-month averages
min_12mo <- rollapply(unrate_3mo_avg, width = 12, FUN = min, align = "right", fill = NA)
# Step 3: Calculate the Sahm Rule signal
recession_signal <- ifelse(unrate_3mo_avg - min_12mo >= 0.5,1,0)

table(recession_signal)

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

summary(results_lin)
irfs <- results_lin$irf_lin_mean
var_names <- results_lin[["specs"]][["column_names"]]

# Example assumes you have: irfs, irf_lin_low, irf_lin_up
K <- dim(irfs)[1]        # Number of variables
H <- dim(irfs)[2]        # Number of horizons
par(mfrow = c(K, K), mar =c(5,2,2,0))     # Set up plotting grid



for (i in 1:K) {
  for (j in 1:K) {
    if (j >= i) {
      plot.new()
      next
    }
    
    irf <- results_lin$irf_lin_mean[i, , j]
    lower <- results_lin$irf_lin_low[i, , j]
    upper <- results_lin$irf_lin_up[i, , j]
    h <- 0:(length(irf) - 1)
    
    plot(h, irf, type = "l", ylim = range(c(lower, upper)), lwd = 0.3,
         main = paste(var_names[j], "on", var_names[i]),
         xlab = "Horizon", ylab = "Response")
    polygon(c(h, rev(h)), c(upper, rev(lower)), col = rgb(0,0,0,0.1), border = NA)
    lines(h, irf, lwd = 2)
    abline(h = 0, lty = 2)
  }
}

# --- End example code

# According to these tests there is non-linear effect and we should model for it
sctest(aggdata2$Financial_losses ~ aggdata2$Disastears, type = "Chow")
sctest(aggdata2$Casualties ~ aggdata2$Disastears, type = "Chow")
sctest(aggdata2$INDPRO ~ aggdata2$Disastears, type = "Chow")


# The example above only estimates impulse responses for the linear case, 
#and my miss out on the non linear effect disastears have on costs and deaths
# --- Code for nonlinear effects of disastears.

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
var_names <- results_nl[["specs"]][["column_names"]]
K <- length(var_names)
plot_pairs <- which(outer(1:K,1:K, function(i,j) j<i), arr.ind = T)
plot_pairs <- plot_pairs[-c(1,2,4), , drop = FALSE]


n_pairs <- nrow(plot_pairs)
par(mfrow = c(n_pairs, 2), mar = c(4, 4, 2, 1))

for (p in 1:n_pairs) {
  i <- plot_pairs[p,1]
  j <- plot_pairs[p,2]
  # --- Left plot: Scenario 1 ---
  irf1 <- results_nl$irf_s1_mean[i, , j]
  lower1 <- results_nl$irf_s1_low[i, , j]
  upper1 <- results_nl$irf_s1_up[i, , j]
  h <- 0:(length(irf1) - 1)
  
  plot(h, irf1, type = "l", ylim = range(c(lower1, upper1)), lwd = 2,
       main = paste("S1:", var_names[j], "on", var_names[i]),
       xlab = "Horizon", ylab = "Response")
  polygon(c(h, rev(h)), c(upper1, rev(lower1)), col = rgb(0,0,0,0.1), border = NA)
  abline(h = 0, lty = 2)
  
  # --- Right plot: Scenario 2 ---
  irf2 <- results_nl$irf_s2_mean[i, , j]
  lower2 <- results_nl$irf_s2_low[i, , j]
  upper2 <- results_nl$irf_s2_up[i, , j]
  
  plot(h, irf2, type = "l", ylim = range(c(lower2, upper2)), lwd = 2, col = "blue",
       main = paste("S2:", var_names[j], "on", var_names[i]),
       xlab = "Horizon", ylab = "Response")
  polygon(c(h, rev(h)), c(upper2, rev(lower2)), col = rgb(0,0,1,0.1), border = NA)
  abline(h = 0, lty = 2)
}
  
library(zoo)
unemp_rate <- as.data.frame(read_excel("UNRATE.xlsx", sheet = 2)[, -1])
unrate_3mo_avg <- rollmean(unemp_rate, k = 3, align = "right", fill = NA)
# Step 2: Compute rolling 12-month minimum of 3-month averages
min_12mo <- rollapply(unrate_3mo_avg, width = 12, FUN = min, align = "right", fill = NA)
# Step 3: Calculate the Sahm Rule signal
recession_signal <- ifelse(unrate_3mo_avg - min_12mo >= 0.5,1,0)

table(recession_signal)


results_nl2<- lp_nl(endog_data,
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

K <- length(var_names)
plot_pairs <- which(outer(1:K, 1:K, function(i, j) j < i), arr.ind = TRUE)
plot_pairs <- plot_pairs[-c(1,2,4), , drop = FALSE]

n_pairs <- nrow(plot_pairs)

par(mfrow = c(n_pairs, 2), mar = c(4, 4, 2, 1))  # 2 columns: s1 on left, s2 on right

for (p in 1:n_pairs) {
  i <- plot_pairs[p, 1]
  j <- plot_pairs[p, 2]
  
  # --- Left plot: Scenario 1 ---
  irf1 <- results_nl2$irf_s1_mean[i, , j]
  lower1 <- results_nl2$irf_s1_low[i, , j]
  upper1 <- results_nl2$irf_s1_up[i, , j]
  h <- 0:(length(irf1) - 1)
  
  plot(h, irf1, type = "l", ylim = range(c(lower1, upper1)), lwd = 2,
       main = paste("S1:", var_names[j], "on", var_names[i]),
       xlab = "Horizon", ylab = "Response")
  polygon(c(h, rev(h)), c(upper1, rev(lower1)), col = rgb(0,0,0,0.1), border = NA)
  abline(h = 0, lty = 2)
  
  # --- Right plot: Scenario 2 ---
  irf2 <- results_nl2$irf_s2_mean[i, , j]
  lower2 <- results_nl2$irf_s2_low[i, , j]
  upper2 <- results_nl2$irf_s2_up[i, , j]
  
  plot(h, irf2, type = "l", ylim = range(c(lower2, upper2)), lwd = 2, col = "blue",
       main = paste("S2:", var_names[j], "on", var_names[i]),
       xlab = "Horizon", ylab = "Response")
  polygon(c(h, rev(h)), c(upper2, rev(lower2)), col = rgb(0,0,1,0.1), border = NA)
  abline(h = 0, lty = 2)
}

