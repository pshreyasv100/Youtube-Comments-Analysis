The following readme contains instructions on how to run the code in this project
and necessary libraries required.
_____________________________________________________
Requirements - Rstudio with R 3.4 and above
               and following libraries  
library(readr)
library(stringr)
library(quanteda)
library(dplyr)
library(stringr)
library(dummies)
library(ggplot2)
library(reshape2)
library(corrplot)
library(nnet)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)

Datasets used 
______________________________________________________
UScomments.csv
USvideos.csv
badwords.txt

Below dataset(twitter) was used to train a hate speech recognizer written in python3 and later used to classify a comment in UScomments.csv as hate speech or not

twitter-hate-speech-classifier-DFE-a845520.csv

