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

UScomments.df <- read_csv(file = "UScomments.csv")[,1:4]
USvideos.df <- read_csv("USvideos.csv")[,1:9]

#colnames(UScomments.df) <- c("video_id","comment_text","likes","replies")

UScomments.df<- UScomments.df[!(UScomments.df$video_id == "" | is.na(UScomments.df$video_id)), ]

# Swear Words & Curse Words
# Source: http://www.noswearing.com/dictionary
badWords <- readLines("badwords.txt")
badWords.df <- as.data.frame(badWords)

UScomments.df$video_id = factor(UScomments.df$video_id)

# There are many packages in the R  for performing text
# analytics. One of them is quanteda. The quanteda
# package has many useful functions for quickly and easily working
# with text data.


# Tokenizing the comments
comments.tokens <- tokens(UScomments.df$comment_text, what = "word",
                   remove_numbers = TRUE,
                   remove_punct = TRUE,
                   remove_separators = TRUE,
                   remove_symbols = TRUE,
                   remove_hyphens = TRUE,
                   remove_url = TRUE)




# We take a look at a specific SMS message and see how it transforms.
comments.tokens[[1]]

# Lower casing the tokens.
comments.tokens <- tokens_tolower(comments.tokens)
comments.tokens[[357]]


# Using quanteda's built-in stopword list for English.
comments.tokens <- tokens_select(comments.tokens, stopwords(), selection = "remove")
comments.tokens[[357]]


#Function slang_freq counts the frequency of slang words in a document in UScomments.df
slang_freq <- function(x){
  freq = 0
  for(i in x){
    if (i %in% badWords){  
      freq = freq + 1
    }   
  }
  return (freq)
}



UScomments.df$slang_freq<- sapply(comments.tokens,FUN = slang_freq)

# Perform stemming on the tokens.
#comments.tokens <- tokens_wordstem(comments.tokens, language = "english")
comments.tokens[[357]]


#unique(UScomments.df$video_id)

#Since table UScomments.df does not contain comments for all videos or valid video_id in USvideos.df 
# we include onnly those documents/comments for which we have a valid corresponding video_id 
UScomments_1_5.df  <- UScomments.df[,c(1,5)]
valid_UScomments.df <- semi_join(x = UScomments_1_5.df, y = USvideos.df)


#Creating a count of slang words for every video
slang_freq.df <- valid_UScomments.df %>%
                    select(video_id,slang_freq) %>% 
                    group_by(video_id) %>%
                    summarise(slang_freq = sum(slang_freq))


#Now since not all videos have comments corresponding to their video_id's we include only those that have 
# comments

USvideos_1.df <- semi_join(x = USvideos.df, y = slang_freq.df)

#Counting number of analyzed comments per video
#Finding rows with unique video_id in USvideos.df

USvideos_1.df <- USvideos.df %>% group_by(video_id) %>% top_n(1, views)

commentnums <- data.frame(table(valid_UScomments.df$video_id))
colnames(commentnums) <- c("video_id", "comment_count")
commentnums <- semi_join(x=commentnums,y= USvideos_1.df)
USvideos_1.df <- semi_join(x=USvideos_1.df, y=commentnums)


USvideos_1.df$comment_total <- as.numeric(USvideos_1.df$comment_total)


USvideos_1.df <- merge(x = USvideos_1.df, y = slang_freq.df, by="video_id",all=TRUE)
USvideos_1.df <- merge(x = USvideos_1.df, y = commentnums, by="video_id",all=TRUE)


str(USvideos_1.df$tags)
#Adding a genre to every video using the tags 

write.csv(USvideos_1.df,"USvideos_1.csv",row.names = F)

#------Ommitting all rows with NA in them-------#
USvideos_1.df  <- na.omit(USvideos_1.df)


#Adding a general category tag using the category_id from json file
rw_meta <- (c("Film & Animation","Autos & Vehicles","","","","","","","","Music","","","","","Pets & Animals"
                              ,"Sports","Short Movies","Travel & Events","Gaming","Videoblogging","People & Blogs",
                              "Comedy","Entertainment", "News & Politics","Howto & Style","Education","Science & Technology",
                              "Nonprofits & Activism","Anime/Animation","Action/Adventure","Classics",
                              "Comedy","Documentary","Drama","Family","Foreign","Horror","Thriller","Shorts","Shows",
                              "Trailers"))

genres <- sapply(USvideos_1.df$category_id ,FUN = function(x){return(rw_meta[as.numeric(x)])})

#-----Spotting genres with too few videos-------#
USvideos_1.df$genre <- genres
USvideos_1.df %>% 
       group_by(genre) %>%
       summarise(number = n())
USvideos_1.df  <- na.omit(USvideos_1.df)


#-------Since some categories have too few inputs, we will merge some of them. combining anime/animation into film and animation, gaming into entertainment, Videoblogging in entertainment, short movies in film and animation, autos and vehicles in science and technology, Pets & animation in entertainment----#
USvideos_1.df[USvideos_1.df$genre == "Autos & Vehicles",]$genre <- "Science & Technology"
USvideos_1.df[USvideos_1.df$genre == "Anime/Animation",]$genre <- "Film & Animation"
USvideos_1.df[USvideos_1.df$genre == "Gaming",]$genre <- "Entertainment"
USvideos_1.df[USvideos_1.df$genre == "Videoblogging",]$genre <- "Entertainment"
USvideos_1.df[USvideos_1.df$genre == "Short Movies",]$genre <- "Film & Animation"
USvideos_1.df[USvideos_1.df$genre == "Pets & Animals",]$genre <- "Entertainment"


#----normalizing the data: Take slang frequency/number of comments we possess as opposed to just slang frequency-------#
USvideos_1.df$abuseratio <- USvideos_1.df$slang_freq / USvideos_1.df$comment_count


#-----Adding a new column for polarisation (dislike/totallikes)----#
USvideos_1.df$polarization <- USvideos_1.df$dislikes / (USvideos_1.df$likes + USvideos_1.df$dislikes)

#--------Removing NaN values from polarization (0 likes and 0 dislikes results in 0/0)------------#
USvideos_1.df$polarization[is.nan(USvideos_1.df$polarization)] <- 0.00


################################################
#########Visualization begins here##############
################################################
                    
#Creating a dataframe containing stats by genre
genre_stats <- USvideos_1.df %>% select(genre, views,likes,dislikes,slang_freq,comment_count,polarization,abuseratio) %>% 
                                          group_by(genre) %>%
                                          summarise(views = sum(views),
                                                    likes = sum(likes),
                                                    dislikes = sum(dislikes),
                                                    comment_count = sum(comment_count),
                                                    slang_freq = sum(slang_freq),
                                                    abuseratio = mean(abuseratio),
                                                    polarization = mean(polarization))


#plot of likes per genre
likes.plot <-ggplot(genre_stats, aes(genre, likes))
likes.plot +geom_bar(stat = "identity")+ theme(axis.text.x=element_text(angle=60, hjust=1)) + ggtitle("Plot of likes by genre")

#plot of views per genre
views.plot <-ggplot(genre_stats, aes(genre, views))
views.plot +geom_bar(stat = "identity")+ theme(axis.text.x=element_text(angle=60, hjust=1)) + ggtitle("Plot of views by genre")

#plot of slang frequency per genre
slang.plot <-ggplot(genre_stats, aes(genre, slang_freq))
slang.plot +geom_bar(stat = "identity")+ theme(axis.text.x=element_text(angle=60, hjust=1)) + ggtitle("Plot of slang frequency by genre")

#plot of dislikes per genre
dislikes.plot <-ggplot(genre_stats, aes(genre, dislikes))
dislikes.plot +geom_bar(stat = "identity")+ theme(axis.text.x=element_text(angle=60, hjust=1)) + ggtitle("Plot of dislikes by genre")


#---------abuseratio per genre------------#
abuseratio.plot <-ggplot(genre_stats, aes(genre, abuseratio))
abuseratio.plot +geom_bar(stat = "identity")+ theme(axis.text.x=element_text(angle=60, hjust=1)) + ggtitle("Plot of abuseratio by genre")


 #plot of slang frequency per comments
df1 <- genre_stats[,c(1,5,6)]
df2 <- melt(df1, id.vars = 'genre')
head(df2)
comment_slang <- ggplot(df2, aes(x = genre,y = value, fill = variable)) + geom_bar(stat = "identity",position = 'dodge')
comment_slang + theme(axis.text.x=element_text(angle=60, hjust=1)) + ggtitle("Plot of comment count and slang frequency by genre")


#----Python hate speech detection results----#

#Creating a new dataframe which counts the number of hate speech instances spotted per video. This csv was created using attached python code, and analyzed using an naive bayes text analysis algorthm, trained on a twitter hate comment data set
commentshs.df <- read_csv(file = "UScommentsHS.csv")
commentshs.df <- commentshs.df[commentshs.df$hate_speech==1,]
hate_vids.df <- commentshs.df %>%
  select(video_id,hate_speech) %>% 
  group_by(video_id) %>%
  summarise(hate_freq = sum(hate_speech))

videoshs.df <- merge(x = USvideos_1.df, y = hate_vids.df, by="video_id",all=TRUE)
videoshs.df <- videoshs.df[!(is.na(videoshs.df$views)),]
videoshs.df[is.na(videoshs.df$hate_freq),] <- 0
videoshs.df$hate_ratio <- videoshs.df$hate_freq/videoshs.df$comment_count
videoshs.df$hate_ratio[is.nan(videoshs.df$hate_ratio)] <- 0
#Required dataframe generated, by the name videoshs.df

genre_stats_hs <- videoshs.df %>% select(genre, views,likes,dislikes,slang_freq,comment_count,polarization,abuseratio, hate_ratio) %>% 
  group_by(genre) %>%
  summarise(views = sum(views),
            likes = sum(likes),
            dislikes = sum(dislikes),
            comment_count = sum(comment_count),
            slang_freq = sum(slang_freq),
            abuseratio = mean(abuseratio),
            polarization = mean(polarization),
            hate_ratio = mean(hate_ratio))

#plot of polarization per genre
genrepolarity.plot <-ggplot(genre_stats_hs, aes(genre, polarization))
genrepolarity.plot +geom_bar(stat = "identity")+ theme(axis.text.x=element_text(angle=60, hjust=1)) + ggtitle("Plot of genre by polarization")


#plot of hate frequency per genre
hate.plot <-ggplot(genre_stats_hs, aes(genre, hate_ratio))
hate.plot +geom_bar(stat = "identity")+ theme(axis.text.x=element_text(angle=60, hjust=1)) + ggtitle("Plot of hate comment ratio by genre")

#Checking our two evaluation criteria to see similarities
hate_abuse.plot <-ggplot(genre_stats_hs, aes(abuseratio, hate_ratio))
hate_abuse.plot +geom_line(stat = "identity")+ theme(axis.text.x=element_text(angle=60, hjust=1)) + ggtitle("Plot of hate comment ratio by abuseratio")
###################################################################
######Classification based on abuse ratio##########################
###################################################################

#-----Creating dummy variables for genres--------#
USvideos_1.df <-(dummy.data.frame(USvideos_1.df, "genre"))
names(USvideos_1.df)[12:20] <- c("comedy", "education", "entertainment", "film_animation", "howto_style", "music", "news_politics", "nonprofits_activism", "science_technology")
summary(USvideos_1.df$abuseratio)
#-------Creating 4 bins based on quartiles-----#
USvideos_1.df$abuse_level <- 4
USvideos_1.df$abuse_level[USvideos_1.df$abuseratio <0.12500 ] <- 3
USvideos_1.df$abuse_level[USvideos_1.df$abuseratio <0.05500 ] <- 2
USvideos_1.df$abuse_level[USvideos_1.df$abuseratio <0.02000 ] <- 1

#---------Splitting the data into training and testing ---------#

set.seed(120)
## 70% of the sample size for training data
numeric_columns.df <- USvideos_1.df[,c(6:23)]
numeric_predictors.df <- numeric_columns.df[,c(1:4,6:15,17,18)]

intermediate <- createDataPartition(y = numeric_predictors.df$abuse_level, p=0.7, list=FALSE)
train.df <- numeric_predictors.df[intermediate,]
test.df <- numeric_predictors.df[-intermediate,]

#-----Function for predictions in multinomial regressions-----#


#formula for model 1 
mod_f1 <- abuse_level ~ .
model1 <- multinom(mod_f1, train.df)
summary(model1)

prediction1 <- predict(model1, test.df, "probs")
prediction1 <- as.data.frame(prediction1)
names(prediction1)[1:4] <- c("Passive", "Low", "Medium", "High")
prediction1[, "predicted_level"] <- apply(prediction1[, 1:4], 1, function(x) which(x==max(x)))
prediction1$actual_level <- test.df$abuse_level

#Checking for accuracy as follows: We wish to classify the the likelihood of bullying, 
#and identify likely videos as well as possible.
#We will do so by allowing a classification error of +-1 ie. if actual level is 2,
#a prediction is correct so long as it predicts a class of 1, 2 or 3 and so on
#This ensures abuse gets identified, even if some extra videos get classified alongwith.
prediction1$deviation <- abs(prediction1$predicted_level - prediction1$actual_level)
prediction1$correct=0
prediction1$correct[prediction1$deviation == 0] = 1
prediction1$correct[prediction1$deviation == 1] = 1
accuracy1 = sum(prediction1$correct)/nrow(prediction1)
accuracy1


#Calculating Haccuracy, or sensitivity of the model. This follows the same principal as above, we allow for an error of upto one class.#
prediction1$highlevelaccuracy = 0
prediction1$highlevelaccuracy[prediction1$actual_level == 4] = 0
prediction1$highlevelaccuracy[prediction1$highlevelaccuracy == 0 & prediction1$predicted_level == 4] = 1
prediction1$highlevelaccuracy[prediction1$highlevelaccuracy == 0 & prediction1$predicted_level == 3] = 1
haccuracy = sum(prediction1$highlevelaccuracy == 1) / (sum(prediction1$highlevelaccuracy == 0) +sum(prediction1$highlevelaccuracy == 1))
haccuracy

#------------------Naive bayes---------#

test_nb <- test.df
nb_model <- naiveBayes(abuse_level ~., data = train.df)
test_nb <- cbind(test_nb,nb_cls = predict(nb_model,test.df,type = "raw"))


#From given columns, finding out which class has highest probability ie is most likely to be the correct class#
test_nb[, "predicted_level"] <- apply(test_nb[, 17:20], 1, function(x) which(x==max(x)))


#Calculating accuracy of the model#
test_nb$deviation <- abs(test_nb$predicted_level - test_nb$abuse_level)
test_nb$correct=0
test_nb$correct[test_nb$deviation == 0] = 1
test_nb$correct[test_nb$deviation == 1] = 1
accuracy2 = sum(test_nb$correct)/nrow(test_nb)
accuracy2

#Calculating Haccuracy, or sensitivity of the model#
test_nb$highlevelaccuracy = 0
test_nb$highlevelaccuracy[test_nb$abuse_level == 4] = 0
test_nb$highlevelaccuracy[test_nb$highlevelaccuracy == 0 & test_nb$predicted_level == 4] = 1
test_nb$highlevelaccuracy[test_nb$highlevelaccuracy == 0 & test_nb$predicted_level == 3] = 1
haccuracy2 = sum(test_nb$highlevelaccuracy == 1) / (sum(test_nb$highlevelaccuracy == 0) +sum(test_nb$highlevelaccuracy == 1))
haccuracy2


#----------------Decision trees-----------#
test_dec <- test.df
fit <- rpart(abuse_level~.,data=train.df,method='class')
cat("Decision Trees -  Variable Importance \n",fit$variable.importance,"\n")
plot(fit, main = "Decision Tree for classification of abuse levels")
text(fit, cex=0.8)

#converting the prediction from factors to numeric#
temp <-predict(fit,test_dec,type='class')
temp <- as.numeric(temp)
test_dec$predicted_level <- temp

#Calculating accuracy of the model#
test_dec$deviation <- abs(test_dec$predicted_level - test_dec$abuse_level)
test_dec$correct=0
test_dec$correct[test_dec$deviation == 0] = 1
test_dec$correct[test_dec$deviation == 1] = 1
accuracy3 = sum(test_dec$correct)/nrow(test_dec)
accuracy3

#Calculating Haccuracy, or sensitivity of the model#
test_dec$highlevelaccuracy = 0
test_dec$highlevelaccuracy[test_dec$abuse_level == 4] = 0
test_dec$highlevelaccuracy[test_dec$highlevelaccuracy == 0 & test_dec$predicted_level == 4] = 1
test_dec$highlevelaccuracy[test_dec$highlevelaccuracy == 0 & test_dec$predicted_level == 3] = 1
haccuracy3 = sum(test_dec$highlevelaccuracy == 1) / (sum(test_dec$highlevelaccuracy == 0) +sum(test_dec$highlevelaccuracy == 1))
haccuracy3


#------------SVM---------#

test_svm <- test.df
svm_model <- svm(abuse_level~.,data=train.df,kernel="polynomial",type="C-classification")

#converting the prediction from factors to numeric#
temp <-predict(svm_model,test_svm)
temp <- as.numeric(temp)
test_svm$predicted_level <- temp

#Calculating accuracy of the model#
test_svm$deviation <- abs(test_svm$predicted_level - test_svm$abuse_level)
test_svm$correct=0
test_svm$correct[test_svm$deviation == 0] = 1
test_svm$correct[test_svm$deviation == 1] = 1
accuracy4 = sum(test_svm$correct)/nrow(test_svm)
accuracy4

#Calculating Haccuracy, or sensitivity of the model#
test_svm$highlevelaccuracy = 0
test_svm$highlevelaccuracy[test_svm$abuse_level == 4] = 0
test_svm$highlevelaccuracy[test_svm$highlevelaccuracy == 0 & test_svm$predicted_level == 4] = 1
test_svm$highlevelaccuracy[test_svm$highlevelaccuracy == 0 & test_svm$predicted_level == 3] = 1
haccuracy4 = sum(test_svm$highlevelaccuracy == 1) / (sum(test_svm$highlevelaccuracy == 0) +sum(test_svm$highlevelaccuracy == 1))
haccuracy4



###########Highest accuracy achieved by Decision tree, highest Haccuracy (or true positive rate ie sensitivity) for classes 3 and 4 was obtained in the svm.
####While dec_trees provide a better performance overall, we recommend the use of the svm classifier since high levels of abuse are captured much better####






##############################################################################
#####Begin classification based on hate speech classifier built on python#####
##############################################################################
#------Creating dummy variables---------#
videoshs.df <-(dummy.data.frame(videoshs.df, "genre"))
names(videoshs.df)[12:21] <- c("Other", "comedy", "education", "entertainment", "film_animation", "howto_style", "music", "news_politics", "nonprofits_activism", "science_technology")


#-------Creating 4 bins based on quartiles-----#
summary(videoshs.df$hate_ratio)
videoshs.df$hate_level <- 4
videoshs.df$hate_level[videoshs.df$hate_ratio <0.0175 ] <- 3
videoshs.df$hate_level[videoshs.df$hate_ratio <0.0080 ] <- 2
videoshs.df$hate_level[videoshs.df$hate_ratio <=0.00 ] <- 1

#---------Splitting the data into training and testing ---------#

set.seed(120)
## 70% of the sample size for training data
numeric_columns.df <- videoshs.df[,c(6:26)]
numeric_predictors.df <- numeric_columns.df[,c(1:4,6:16,18,21)]


intermediate <- createDataPartition(y = numeric_predictors.df$hate_level, p=0.7, list=FALSE)
train.df <- numeric_predictors.df[intermediate,]
test.df <- numeric_predictors.df[-intermediate,]

#-----Function for predictions in multinomial regressions-----#


#formula for model 1 
mod_f1 <- hate_level ~ .
model1 <- multinom(mod_f1, train.df)
summary(model1)

prediction1 <- predict(model1, test.df, "probs")
prediction1 <- as.data.frame(prediction1)
names(prediction1)[1:4] <- c("Passive", "Low", "Medium", "High")
prediction1[, "predicted_level"] <- apply(prediction1[, 1:4], 1, function(x) which(x==max(x)))
prediction1$actual_level <- test.df$hate_level

#Checking for accuracy as follows: We wish to classify the the likelihood of bullying, 
#and identify likely videos as well as possible.
#We will do so by allowing a classification error of +-1 ie. if actual level is 2,
#a prediction is correct so long as it predicts a class of 1, 2 or 3 and so on
#This ensures abuse gets identified, even if some extra videos get classified alongwith.
prediction1$deviation <- abs(prediction1$predicted_level - prediction1$actual_level)
prediction1$correct=0
prediction1$correct[prediction1$deviation == 0] = 1
prediction1$correct[prediction1$deviation == 1] = 1
accuracy1_2 = sum(prediction1$correct)/nrow(prediction1)
accuracy1_2


#Calculating Haccuracy, or sensitivity of the model. This follows the same principal as above, we allow for an error of upto one class.#
prediction1$highlevelaccuracy = 0
prediction1$highlevelaccuracy[prediction1$actual_level == 4] = 0
prediction1$highlevelaccuracy[prediction1$highlevelaccuracy == 0 & prediction1$predicted_level == 4] = 1
prediction1$highlevelaccuracy[prediction1$highlevelaccuracy == 0 & prediction1$predicted_level == 3] = 1
haccuracy1_2 = sum(prediction1$highlevelaccuracy == 1) / (sum(prediction1$highlevelaccuracy == 0) +sum(prediction1$highlevelaccuracy == 1))
haccuracy1_2

#------------------Naive bayes---------#

test_nb <- test.df
nb_model <- naiveBayes(hate_level ~., data = train.df)
test_nb <- cbind(test_nb,nb_cls = predict(nb_model,test.df,type = "raw"))


#From given columns, finding out which class has highest probability ie is most likely to be the correct class#
test_nb[, "predicted_level"] <- apply(test_nb[, 18:21], 1, function(x) which(x==max(x)))


#Calculating accuracy of the model#
test_nb$deviation <- abs(test_nb$predicted_level - test_nb$hate_level)
test_nb$correct=0
test_nb$correct[test_nb$deviation == 0] = 1
test_nb$correct[test_nb$deviation == 1] = 1
accuracy2_2 = sum(test_nb$correct)/nrow(test_nb)
accuracy2_2

#Calculating Haccuracy, or sensitivity of the model#
test_nb$highlevelaccuracy = 0
test_nb$highlevelaccuracy[test_nb$hate_level == 4] = 0
test_nb$highlevelaccuracy[test_nb$highlevelaccuracy == 0 & test_nb$predicted_level == 4] = 1
test_nb$highlevelaccuracy[test_nb$highlevelaccuracy == 0 & test_nb$predicted_level == 3] = 1
haccuracy2_2 = sum(test_nb$highlevelaccuracy == 1) / (sum(test_nb$highlevelaccuracy == 0) +sum(test_nb$highlevelaccuracy == 1))
haccuracy2_2


#----------------Decision trees-----------#
test_dec <- test.df
fit <- rpart(hate_level~.,data=train.df,method='class')
cat("Decision Trees -  Variable Importance \n",fit$variable.importance,"\n")
plot(fit, main = "Decision Tree for classification of hate levels")
text(fit, cex=0.8)

#converting the prediction from factors to numeric#
temp <-predict(fit,test_dec,type='class')
temp <- as.numeric(temp)
test_dec$predicted_level <- temp

#Calculating accuracy of the model#
test_dec$deviation <- abs(test_dec$predicted_level - test_dec$hate_level)
test_dec$correct=0
test_dec$correct[test_dec$deviation == 0] = 1
test_dec$correct[test_dec$deviation == 1] = 1
accuracy3_2 = sum(test_dec$correct)/nrow(test_dec)
accuracy3_2

#Calculating Haccuracy, or sensitivity of the model#
test_dec$highlevelaccuracy = 0
test_dec$highlevelaccuracy[test_dec$hate_level == 4] = 0
test_dec$highlevelaccuracy[test_dec$highlevelaccuracy == 0 & test_dec$predicted_level == 4] = 1
test_dec$highlevelaccuracy[test_dec$highlevelaccuracy == 0 & test_dec$predicted_level == 3] = 1
haccuracy3_2 = sum(test_dec$highlevelaccuracy == 1) / (sum(test_dec$highlevelaccuracy == 0) +sum(test_dec$highlevelaccuracy == 1))
haccuracy3_2


#------------SVM---------#

test_svm <- test.df
svm_model <- svm(hate_level~.,data=train.df,kernel="polynomial",type="C-classification")

#converting the prediction from factors to numeric#
temp <-predict(svm_model,test_svm)
temp <- as.numeric(temp)
test_svm$predicted_level <- temp

#Calculating accuracy of the model#
test_svm$deviation <- abs(test_svm$predicted_level - test_svm$hate_level)
test_svm$correct=0
test_svm$correct[test_svm$deviation == 0] = 1
test_svm$correct[test_svm$deviation == 1] = 1
accuracy4_2 = sum(test_svm$correct)/nrow(test_svm)
accuracy4_2

#Calculating Haccuracy, or sensitivity of the model#
test_svm$highlevelaccuracy = 0
test_svm$highlevelaccuracy[test_svm$hate_level == 4] = 0
test_svm$highlevelaccuracy[test_svm$highlevelaccuracy == 0 & test_svm$predicted_level == 4] = 1
test_svm$highlevelaccuracy[test_svm$highlevelaccuracy == 0 & test_svm$predicted_level == 3] = 1
haccuracy4_2 = sum(test_svm$highlevelaccuracy == 1) / (sum(test_svm$highlevelaccuracy == 0) +sum(test_svm$highlevelaccuracy == 1))
haccuracy4_2


#Accuracy1, 2 3 and 4 are respectively: 0.9587852 0.6225597 0.9045553 0.9305857
#Haccuracy (sensitivity for high hate levels) are respectively 0.537961 0.06724512 0.4295011 0.6138829
#For the logistic multinomial regression, NB classifier, Decision tree and SVM models respectively
#We recommend the use of the NB classifier since it provides a right balance of both accuracy and sensitivity.
#However, the NB classifier is equally usable since sensitivity is especially critical in any hate comment filter. misclassification of lower hate levels dont matter as much.



###########################################################
#######Classification Conclusions##########################
###########################################################
#SVM model did both considering both our metrics.# 