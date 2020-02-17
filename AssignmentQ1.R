rm(list = ls())

library(reshape2)
library(ggplot2)

readdata.function <- function(p) {
  library("readxl")
  all_data <- readxl::read_excel(p)
  airpol <- all_data[which(all_data[,1] == "AIRPOL"), 2]
  airpol <- airpol[[1]]

  return(list(all_data = all_data, airpol = airpol))
}

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

  return(list(scaled_data = scaled_data, selected_data_02 = selected_data_02))
}

# Get data
airpol <- readdata.function(p = "data\\env_air_emis.xls")
data_sul <- PCA_dataprep.function(airpol$airpol[5], airpol$all_data)

# PCA plots (Q1 - a and b):
PCA_sul <- prcomp(data_sul$scaled_data)
load_PC1PC2 <- PCA_sul$rotation[, 1:2]

biplot(PCA_sul, xlabs = rep(".", nrow(PCA_sul$x)), col = c("blue", "black"))

cum_var_per <- cumsum(PCA_sul$sdev^2) / sum(PCA_sul$sdev^2)
var_per <- PCA_sul$sdev^2 / sum(PCA_sul$sdev^2)

par(mfrow = c(1, 2))
plot(var_per, type = 'b', xlab = 'Principal Component',
     ylab = 'Prop. Variance Explained')
plot(cum_var_per, type = 'b', xlab = 'Principal Component',
     ylab = 'Cumulative Prop. Variance Explained')
par(mfrow = c(1, 1))

# BIC Criteria (Q1c)
if (nrow(data_sul$scaled_data) == ncol(data_sul$scaled_data)) {
  Cnp <- nrow(data_sul$scaled_data)*ncol(data_sul$scaled_data)
} else {
  Cnp <- min(nrow(data_sul$scaled_data), ncol(data_sul$scaled_data))
}

k_max <- ncol(PCA_sul$rotation) - 1

SSR <- rep(0, k_max)
penalty <- rep(0, k_max)
BIC <- rep(0, k_max)

for (k in 1:k_max) {
  epsilon2 <- matrix(0, nrow(PCA_sul$x), nrow(PCA_sul$rotation))

  for (i in 1:nrow(PCA_sul$x)) {
    for (j in 1:nrow(PCA_sul$rotation)) {
      epsilon2[i,j] <- (data_sul$scaled_data[i, j] - PCA_sul$rotation[j, 1:k]
                        %*% t(PCA_sul$x)[1:k, i])^2
    }
  }

  SSR[k] <- sum(epsilon2) / (nrow(PCA_sul$x)*nrow(PCA_sul$rotation))
  penalty[k] <- k*log(Cnp) / Cnp

  BIC[k] <- penalty[k] + log(SSR[k])
}

par(mfrow = c(1, 2))
plot(penalty, type = "o", col = "green", ann = FALSE)
title(xlab = 'Number of PC', ylab = 'Penalty')
plot(log(SSR), type = "o", col = "red", ann = FALSE)
title(xlab = 'Number of PC', ylab = 'log(SSR)')

par(mfrow = c(1, 1))
plot(log(SSR), type = "o", pch = 22, lty = 2, col = "red",  ann = FALSE)
lines(penalty, type = "o", pch = 22, lty = 2, col = "green")
lines(BIC, type = "o", pch = 22, lty = 2, col = "blue")
title(main = 'Number of PC Selection', xlab = 'Number of PC', ylab = 'BIC')
legend("bottomleft", c("log(SSR)", "Penalty", "BIC"), cex = 0.8,
       col = c("red", "green", "blue"), lty = 1:2)

# BIC Criteria (Q1c) - unscaled data
PCA_sul <- prcomp(data_sul$selected_data_02)

if (nrow(data_sul$selected_data_02) == ncol(data_sul$selected_data_02)) {
  Cnp <- nrow(data_sul$selected_data_02)*ncol(data_sul$selected_data_02)
} else {
  Cnp <- min(nrow(data_sul$selected_data_02), ncol(data_sul$selected_data_02))
}

k_max <- ncol(PCA_sul$rotation) - 1

BIC <- rep(0, k_max)

for (k in 1:k_max) {
  epsilon2 <- matrix(0, nrow(PCA_sul$x), nrow(PCA_sul$rotation))

  for (i in 1:nrow(PCA_sul$x)) {
    for (j in 1:nrow(PCA_sul$rotation)) {
      epsilon2[i,j] <- (data_sul$scaled_data[i, j] - PCA_sul$rotation[j, 1:k]
                        %*% t(PCA_sul$x)[1:k, i])^2
    }
  }

  SSR <- sum(epsilon2) / (nrow(PCA_sul$x)*nrow(PCA_sul$rotation))
  penalty <- k*log(Cnp) / Cnp

  BIC[k] <- log(SSR) + penalty
}

par(mfrow = c(1, 1))
plot(BIC, type = "o", col = "blue", ann = FALSE)
title(xlab = 'Number of PCs', ylab = 'BIC')

# Principal components over time (Q1d)
PC1 <- matrix(0, 28, length(airpol$airpol))
PC2 <- matrix(0, 28, length(airpol$airpol))

for (i in 1:length(airpol$airpol)) {
  data <- PCA_dataprep.function(AIRPOL = airpol$airpol[i],
                                all_data = airpol$all_data)
  PCA <- prcomp(data$scaled_data)
  PC1[, i] <- PCA$x[, 1]
  PC2[, i] <- PCA$x[, 2]
}

years <- unname(rownames(PCA$x))

#Principal Component 1 (Q1d)
rownames(PC1) <- rownames(PCA$x)
colnames(PC1) <- airpol$airpol
data_PC1 <- data.frame(PC1,years)
data_PC1 <- reshape2::melt(data_PC1, id.vars = 'years')
data_PC1$years <- as.numeric(as.character(data_PC1$years))
colnames(data_PC1)[3] <- 'pollution'
colnames(data_PC1)[2] <- 'pollutant'

ggplot(data_PC1, aes(years, pollution, col = pollutant)) +
  geom_point() + geom_line() + ylab('1st principal component') +
  ggtitle("1st principal component per pollutant over the years" ) +
  theme_bw() + theme(legend.position = c(0.86, 0.15),
                     legend.background = element_blank())

#Principal Compent 2 (Q1d)
rownames(PC2) <- rownames(PCA$x)
colnames(PC2) <- airpol$airpol
data_PC2 <- data.frame(PC2,years)
data_PC2 <- melt(data_PC2, id.vars = 'years')
data_PC2$time <- as.numeric(as.character(data_PC2$years))
colnames(data_PC2)[3] <- 'pollution'
colnames(data_PC2)[2] <- 'pollutant'

ggplot(data_PC2, aes(time, pollution, col = pollutant)) +
 geom_line() + geom_point() + ylab('2nd principal component') +
  ggtitle("2nd principal component per pollutant over the years" ) +
  theme_bw() + theme(legend.position = c(0.86, 0.15),
                     legend.background = element_blank())
