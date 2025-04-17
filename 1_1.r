# --- Setup ---
set.seed(123)
total_n <- 200
phi_flipped <- c(0.6, -0.5)  # R-style signs for φ₁ = -0.6, φ₂ = 0.5

# --- Simulate 5 realizations ---
sim_data <- matrix(NA, nrow = 5, ncol = n)

for (i in 1:5) {
  e <- rnorm(total_n)
  x <- filter(e, filter = phi_flipped, method = "recursive")
  sim_data[i, ] <- x[(burn_in + 1):total_n]  # remove burn-in
}

# --- Plot all 5 in one plot ---
plot(1:n, sim_data[1, ], type = "l", ylim = range(sim_data),
     xlab = "Time", ylab = expression(X[t]),
     main = "5 Realizations of AR(2): phi1 = -0.6, phi2 = 0.5", col = 1)

for (i in 2:5) {
  lines(1:n, sim_data[i, ], col = i)
}

legend("topright", legend = paste("Realization", 1:5),
       col = 1:5, lty = 1)
