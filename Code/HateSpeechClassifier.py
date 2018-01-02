import pandas as pd
import random
import nltk
import numpy
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.naive_bayes import MultinomialNB

from sklearn.metrics import confusion_matrix
#from pandas_ml import ConfusionMatrix

from nltk import word_tokenize
from nltk.corpus import stopwords
import string

#UScomments = []
#USvideos = []

#UScomments = pd.read_csv('UScomments.csv', error_bad_lines=False)
#USvideos = pd.read_csv('USvideos.csv', error_bad_lines=False)

#bw = open('badwords.txt', 'r')
#badWords = [line.strip() for line in bw]

hs = pd.read_csv('hatespeech.csv', encoding = 'ISO-8859-1')
hate = [hs.does_this_tweet_contain_hate_speech, hs.tweet_text, []]

stop = stopwords.words('english') + list(string.punctuation)


for i in range(len(hate[0])):
    if hate[0][i] == 'The tweet contains hate speech':
        hate[2].append(1)
    else:
        hate[2].append(0)

com = []

for i in range(len(hate[1])):
    if isinstance(hate[1][i],int):
        com.append("NA")
    else:
        l = [j for j in word_tokenize(hate[1][i].lower()) if j not in stop]
        str = ' '.join(l)
        com.append(str)


test = [[],[]]
train = [[],[]]
for i in range(len(com)):
    t = random.randint(0,1)
    if(t == 0):
        test[0].append(com[i])
        test[1].append(hate[2][i])
    else:
        train[0].append(com[i])
        train[1].append(hate[2][i])


count_vectorizer = CountVectorizer()
counts = count_vectorizer.fit_transform(train[0])

classifier = MultinomialNB()
classifier.fit(counts, train[1])

testcounts = count_vectorizer.transform(test[0])
predictions = classifier.predict(testcounts)

confusion_matrix(test[1], predictions)
#cm = ConfusionMatrix(test[1], predictions)
#cm.print_stats()

youtube = pd.read_csv('UScomments.csv', error_bad_lines=False, low_memory = False)
youtube = youtube.replace(numpy.nan, '', regex = True)
yt = [youtube.comment_text, []]
yttestcounts = count_vectorizer.transform(yt[0])
ytpredictions = classifier.predict(yttestcounts)

youtube1 = youtube
youtube1['hate_speech']= pd.Series(ytpredictions)
youtube1.to_csv(path_or_buf='UScommentsHS.csv')