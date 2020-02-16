rm(list = ls())

library(reshape2)
library(ggplot2)

readdata.function <- function(p) {
  library("readxl")
  all_data <- readxl::read_excel(p)
  airpol <- all_data[which(all_data[,1] == "AIRPOL"),2]
  airpol <- airpol[[1]]

  return(list(all_data = all_data, airpol = airpol))
}

airpol <- readdata.function(p = "data\\env_air_emis.xls")

PCA_dataprep.function <- function(AIRPOL, all_data) {
  selected_data_00 <- all_data[which(all_data[, 2] == AIRPOL):nrow(all_data), ]
  if (length(which(is.na(selected_data_00[, 1]))) == 1) {
    selected_data_01 <- t(selected_data_00[
      (which(is.na(selected_data_00[, 1])) + 1):nrow(selected_data_00), ])
  } else {
    selected_data_01 <- t(selected_data_00[
      (which(is.na(selected_data_00[, 1]))[1] + 1):(which(
        is.na(selected_data_00[, 1]))[2] - 1), ])
  }

  selected_data_02 <- selected_data_01[2:nrow(selected_data_01),
                                       2:ncol(selected_data_01)]
  rownames(selected_data_02) <- selected_data_01[2:nrow(selected_data_01), 1]
  colnames(selected_data_02) <- selected_data_01[1, 2:ncol(selected_data_01)]
  class(selected_data_02) <- 'numeric'
  scaled_data <- scale(selected_data_02)

  return(scaled_data)
}

# PCA plots (Q1 a and b):
data_sul <- PCA_dataprep.function(AIRPOL = airpol$airpol[5],
                                  all_data = airpol$all_data)


PCA_sul <- prcomp(data_sul)
load_PC1PC2 <- PCA_sul$rotation[, 1:2]
biplot(PCA_sul, xlabs = rep(".", nrow(PCA_sul$x)), col = c("blue", "black"))
cum_var_per <- cumsum(PCA_sul$sdev^2) / sum(PCA_sul$sdev^2)
var_per <- PCA_sul$sdev / sum(PCA_sul$sdev)

par(mfrow = c(1, 2))
plot(var_per, type = 'b', xlab = 'Principal Component',
     ylab = 'Prop. Variance Explained')
plot(cum_var_per, type = 'b', xlab = 'Principal Component',
     ylab = 'Cumulative Prop. Variance Explained')
par(mfrow = c(1, 1))

# BIC Criteria (Q1c)
if (nrow(data_sul) == ncol(data_sul)) {
  Cnp <- nrow(data_sul)*ncol(data_sul)
} else {
  Cnp <- min(nrow(data_sul), ncol(data_sul))
}

k_max = ncol(PCA_sul$rotation) - 1

BIC <- rep(0, k_max)

for (k in 1:k_max) {
  epsilon2 <- matrix(0, nrow(PCA_sul$x), nrow(PCA_sul$rotation))

  for (i in 1:nrow(PCA_sul$x)) {
    for (j in 1:nrow(PCA_sul$rotation)) {
      epsilon2[i, j] <- (data_sul[i, j] - PCA_sul$rotation[j, 1:k]
                         %*% t(PCA_sul$x)[1:k, i])^2
    }
  }

  SSR <- sum(epsilon2) / (nrow(PCA_sul$x) * nrow(PCA_sul$rotation))
  penalty <- k*log(Cnp) / Cnp
  BIC[k] <- penalty + log(SSR)
}

plot(BIC, type = "b", xlab = "Number of PCs")
axis(1, seq(0, k_max, 1), labels = TRUE)

# Ex D - Recode 28
PC1 <- matrix(0, 28, length(airpol$airpol))
PC2 <- matrix(0, 28, length(airpol$airpol))

for (i in 1:length(airpol$airpol)) {
  data <- PCA_dataprep.function(AIRPOL = airpol$airpol[i],
                                all_data = airpol$all_data)
  PCA <- prcomp(data)
  PC1[, i] <- PCA$x[, 1]
  PC2[, i] <- PCA$x[, 2]
}

rownames(PC1) <- rownames(PCA$x)
rownames(PC2) <- rownames(PCA$x)
colnames(PC1) <- airpol$airpol
colnames(PC2) <- airpol$airpol

time <- unname(rownames(PCA$x))

#Principal Component 1 (Q1d)
data_PC1 <- data.frame(PC1,time)
data_PC1 <- melt(data_PC1, id.vars = 'time')
ggplot(data_PC1, aes(time, value, col = variable)) +
  geom_point() + geom_smooth(se = F)

#Principal Compent 2 (Q1d)
data_PC2 <- data.frame(PC2,time)
data_PC2 <- melt(data_PC2, id.vars = 'time')
ggplot(data_PC2, aes(time, value, col = variable)) +
  geom_point() + geom_smooth(se = F)
