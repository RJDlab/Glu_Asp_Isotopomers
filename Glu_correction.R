setwd('C:/Data2/R')
library(readxl); library(xlsx); library(glmnet);
#read corrected matrix of Glu
gm_cor<- read_excel("Supplementary Data II.xlsx", 
                    sheet = "S3", range = "B4:AG91", col_names = FALSE)
gm_cor <- as.matrix(gm_cor)
#read data
rawdata<- read_excel("Input Data Glu Asp.xlsx", sheet = "H460 Data", range = "B99:B186", col_names = FALSE)
View(rawdata)
rawdata <- as.matrix(rawdata)
# number of samples in input file
nt=1
result <- matrix(nrow=32, ncol=nt, 0)
for(i in 1:ncol(rawdata)) {Glu <- rawdata[,c(i)] 
cal.iso <-coef(glmnet(gm_cor,Glu, lambda = 0, lower.limits = 0, intercept = FALSE, thresh = 1e-30,upper.limits = 1))[-1]

temp_res<-cal.iso
result[,i]<-temp_res}

write.xlsx(result, "GluCorrection.xlsx",  sheetName = "Sheet1", col.names = TRUE,  row.names = TRUE, append =FALSE)
