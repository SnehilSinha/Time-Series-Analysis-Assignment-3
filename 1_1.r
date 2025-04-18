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


# Step 5: Plot theoretical ACF
plot(lags, theoretical_acf, type = "l", col = "blue", lwd = 2,
     xlab = "Lag", ylab = "ACF", ylim = c(-1, 1),
     main = "Theoretical ACF for AR(2)")
legend("topright", legend = c("Theoretical ACF"),
       col = c("blue"), lty = c(1, 2), lwd = 2)
