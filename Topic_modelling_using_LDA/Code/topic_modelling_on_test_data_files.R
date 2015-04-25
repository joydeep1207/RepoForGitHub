##
# name : Joydeep
# date : 22/4/2015
# topic: Topic modelling with LDA on given blog in txt format.
##

####################################################
# for documents in txt format.
library("lda")

# locating the working directory
setwd("C:/Users/deepj/Desktop/predict_exectime/POC")

#installing packages 
install.packages("RTextTools")
installed.packages("topicmodels")

#loading the packages in memory
library(RTextTools)
library(topicmodels)

file_name = c("abc.txt","def.txt")
file_path = "C:/Users/deepj/Desktop/predict_exectime/POC/LDA/"


total_no_of_files = 2
for (i in 1:total_no_of_files ){
  
  file = paste(file_path , file_name[i], sep = "")
  
  #reading the data and converting it into dataframes
  myData1 = read.delim(file, header = F, sep= "+")
  
  # making a list of documents
  mydata = concatenate.documents(as.vector(myData1$V1))
  
  matrix <- create_matrix(mydata, 
                          language="english", removeNumbers=TRUE, stemWords=TRUE,
                          weighting=tm::weightTf)
  
  k = 5
  lda <- LDA(matrix, k)
  
  print(paste('Top 5 topics for filename ' , file_name[i]))
  print (terms(lda))
  ## topics(lda)
  
}
