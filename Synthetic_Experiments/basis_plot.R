xs = seq(0,1,length.out = 10000)
library(splines)
m = 10
phi <- bs(xs, df=m, intercept = F) # to check?
phi2 = matrix(NA, nrow = nrow(phi), ncol=ncol(phi))
phi_bar = NULL;
for(i in 1:ncol(phi)){
  phi2[,i] = phi[,i] - mean(phi[,i])
  phi_bar[i] = mean(phi[,i])
}

matplot(phi, type='l')
matplot(phi2, type='l')

lines(xs,phi[10,], col="red",lwd=2)


library(splines)
library(ggplot2)
library(tidyr)
library(dplyr)

# Create sequence
xs <- seq(0, 1, length.out = 10000)

# Create B-spline basis matrix
m <- 10
phi <- bs(xs, df = m, intercept = FALSE)

# Center the columns of phi
phi2 <- phi
phi_bar <- colMeans(phi)
for (i in 1:ncol(phi)) {
  phi2[, i] <- phi[, i] - phi_bar[i]
}

# Convert matrices to data frames for ggplot
phi_df <- as.data.frame(phi)
phi2_df <- as.data.frame(phi2)
phi_df$xs <- xs
phi2_df$xs <- xs

# Gather data for ggplot
phi_long <- phi_df %>% gather(key = "basis", value = "value", -xs)
phi2_long <- phi2_df %>% gather(key = "basis", value = "value", -xs)

# Plot original basis functions
p1 <- ggplot(phi_long, aes(x = xs, y = value, color = basis)) +
  geom_line() +
   theme_minimal()+
  ggtitle("B-spline Basis Functions")
p1
# Plot centered basis functions
p2 <- ggplot(phi2_long, aes(x = xs, y = value, color = basis)) +
  geom_line() +
  theme_minimal() +
  ggtitle("Centered B-spline Basis Functions")

# Print plots
print(p1)
print(p2)
