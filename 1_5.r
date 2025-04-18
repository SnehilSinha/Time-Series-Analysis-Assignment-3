# -----------------------------
# Q1.2 - ACF Plot for AR(2)
# phi1 = -0.7, phi2 = -0.3 (Madsen notation, same in R simulation here)
# -----------------------------

library(forecast)

# Set seed for reproducibility
set.seed(123)

# Parameters
n <- 200
num_series <- 5
max_lag <- 30

phi1 <- -0.7
phi2 <- -0.3

# Step 1: Simulate 5 realizations of AR(2)
simulations <- replicate(num_series, arima.sim(model = list(ar = c(phi1, phi2)), n = n))

# Step 2: Empirical ACFs for each realization
acf_list <- apply(simulations, 2, function(x) acf(x, plot = FALSE, lag.max = max_lag)$acf)

# Step 3: Average ACF across simulations
avg_empirical_acf <- rowMeans(acf_list)
lags <- 0:max_lag

# Step 4: Theoretical ACF from textbook formula
theoretical_acf <- numeric(max_lag + 1)
theoretical_acf[1] <- 1
theoretical_acf[2] <- -phi1 / (1 + phi2)
theoretical_acf[3] <- -phi2 + phi1 * theoretical_acf[2]

for (k in 4:(max_lag + 1)) {
  theoretical_acf[k] <- -phi1 * theoretical_acf[k - 1] - phi2 * theoretical_acf[k - 2]
}

# Step 5: Plot
plot(lags, theoretical_acf, type = "l", col = "blue", lwd = 2,
     xlab = "Lag", ylab = "ACF", ylim = c(-1, 1),
     main = "Empirical vs Theoretical ACF for AR(2)")
lines(lags, avg_empirical_acf, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Theoretical ACF", "Average Empirical ACF"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)
