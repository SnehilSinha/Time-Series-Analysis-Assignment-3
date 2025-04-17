# -----------------------------
# AR(2) Simulation + ACF Plot
# For phi1 = -0.6, phi2 = 0.5 (Madsen notation)
# -----------------------------

library(forecast)

# Set parameters
set.seed(123)
n <- 200              # Number of observations
num_series <- 5       # Number of realizations
max_lag <- 30         # Max lag for ACF

# Flip signs for R's convention (since R uses: X_t = phi1 * X_{t-1} + phi2 * X_{t-2} + e_t)
phi1 <- 0.6           # Flipped from -0.6
phi2 <- -0.5          # Flipped from +0.5

# Step 1: Simulate 5 AR(2) realizations
simulations <- replicate(num_series, arima.sim(model = list(ar = c(phi1, phi2)), n = n, sd = 1))

# Step 2: Compute empirical ACF for each realization
acf_list <- apply(simulations, 2, function(x) acf(x, plot = FALSE, lag.max = max_lag)$acf)

# Step 3: Average empirical ACF across realizations
avg_empirical_acf <- rowMeans(acf_list)
lags <- 0:max_lag


# Step 4: Compute theoretical ACF using textbook values directly
phi1_textbook <- -0.6   # No sign flip here
phi2_textbook <- 0.5

theoretical_acf <- numeric(max_lag + 1)
theoretical_acf[1] <- 1
theoretical_acf[2] <- -phi1_textbook / (1 + phi2_textbook)
theoretical_acf[3] <- -phi2_textbook + phi1_textbook * theoretical_acf[2]

for (k in 4:(max_lag + 1)) {
  theoretical_acf[k] <- -phi1_textbook * theoretical_acf[k - 1] - phi2_textbook * theoretical_acf[k - 2]
}


# Step 5: Plot empirical vs theoretical ACF
plot(lags, theoretical_acf, type = "l", col = "blue", lwd = 2,
     xlab = "Lag", ylab = "ACF", ylim = c(-1, 1),
     main = "Empirical vs Theoretical ACF for AR(2)")
lines(lags, avg_empirical_acf, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Theoretical ACF", "Average Empirical ACF"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)
