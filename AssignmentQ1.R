rm(list = ls())

readdata.function <- function(p) {
  library("readxl")
  all_data <- read_excel(p)
  airpol <- all_data[which(all_data[,1] == "AIRPOL"),2]
  airpol <- airpol[[1]]
  return(list(all_data=all_data, airpol=airpol))
}

airpol <- readdata.function(p = "Assignment 1\\env_air_emis.xls")

PCA_dataprep.function <- function(AIRPOL, all_data) {
  selected_data_00 <- all_data[which(all_data[,2] == AIRPOL):nrow(all_data),]
  if (length(which(is.na(selected_data_00[,1])))==1) {
    selected_data_01 <- t(selected_data_00[(which(is.na(selected_data_00[,1]))+1):nrow(selected_data_00),])
  }
  else {
    selected_data_01 <- t(selected_data_00[(which(is.na(selected_data_00[,1]))[1]+1):(which(is.na(selected_data_00[,1]))[2]-1),])
  }
  selected_data_02<-selected_data_01[2:nrow(selected_data_01),2:ncol(selected_data_01)]
  rownames(selected_data_02)<-selected_data_01[2:nrow(selected_data_01),1]
  colnames(selected_data_02)<-selected_data_01[1,2:ncol(selected_data_01)]
  class(selected_data_02)<-'numeric'
  scaled_data <- scale(selected_data_02)
  return(scaled_data)
}
# Recode 28
PC1 <- matrix(0,28,length(airpol$airpol))
PC2 <- matrix(0,28,length(airpol$airpol))

for (i in 1:length(airpol$airpol)) {
data <- PCA_dataprep.function(AIRPOL = airpol$airpol[i], all_data = airpol$all_data)
PCA <- prcomp(data)
PC1[,i] <- PCA$rotation[,1]
PC2[,i] <- PCA$rotation[,2]
}

#matplot(M, type = c("b"),pch=1,col = 1:28) #plot

# PCA plots (Q1 a and b):
data_sul <- PCA_dataprep.function(AIRPOL = airpol$airpol[5], all_data = airpol$all_data)
PCA<-prcomp(data_sul)
load_PC1PC2 <- PCA$rotation[,1:2]
biplot(PCA, xlabs=rep(".",nrow(PCA$x)), col=c("blue","black"))
screeplot(PCA, type = "lines")
cum_var_per <- cumsum(PCA$sdev)/sum(PCA$sdev)
var_per <- PCA$sdev/sum(PCA$sdev)
plot(var_per, type= 'b', xlab = 'Principal Component', ylab = 'Prop. Variance Explained')
plot(cum_var_per, type= 'b', xlab = 'Principal Component', ylab = 'Cumulative Prop. Variance Explained')

