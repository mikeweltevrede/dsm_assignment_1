rm(list = ls())

PCA_dataprep.function <- function(p, AIRPOL) {
  library("readxl")
  all_data <- read_excel(p)
  selected_data_00 <- all_data[which(all_data[,2] == AIRPOL):nrow(all_data),]
  if (length(which(is.na(selected_data_00[,1])))==1) {
    selected_data_01 <- t(selected_data_00[(which(is.na(selected_data_00[,1]))+1):nrow(selected_data_00),])
  }
  # Add else if AIRPOL is not last one
  selected_data_02<-selected_data_01[2:nrow(selected_data_01),2:ncol(selected_data_01)]
  rownames(selected_data_02)<-selected_data_01[2:nrow(selected_data_01),1]
  colnames(selected_data_02)<-selected_data_01[1,2:ncol(selected_data_01)]
  class(selected_data_02)<-'numeric'
  scaled_data <- scale(selected_data_02)
  return(scaled_data)
}

data <- PCA_dataprep.function(p = "Assignment 1\\env_air_emis.xls", AIRPOL = 'Sulphur oxides')

# PCA plots:
PCA<-prcomp(data)
screeplot(PCA, type = "lines")
cum_var_per <- cumsum(PCA$sdev)/sum(PCA$sdev)
var_per <- PCA$sdev/sum(PCA$sdev)
plot(var_per, type= 'b', xlab = 'Principal Component', ylab = 'Prop. Variance Explained')
plot(cum_var_per, type= 'b', xlab = 'Principal Component', ylab = 'Cumulative Prop. Variance Explained')

