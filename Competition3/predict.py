"""
Code to create predicitons for the CrowdFlower Search Result Relevance Kaggle competition
	https://www.kaggle.com/c/crowdflower-search-relevance/

Uses the Python Benchmark code that they provide with some changes.

Also uses some code from:
https://www.kaggle.com/duttaroy/crowdflower-search-relevance/porter-stemmer
"""
import nltk
import numpy as np
import pandas as pd
import re
import logging
import os
import sys

from sklearn.feature_extraction import text
from sklearn.base import BaseEstimator
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.pipeline import Pipeline
from sklearn.ensemble import RandomForestClassifier
from sklearn.feature_extraction.text import TfidfVectorizer

from HTMLParser import HTMLParser

#create logger
logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)
ch = logging.StreamHandler(sys.stdout)
ch.setLevel(logging.INFO)
formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
ch.setFormatter(formatter)
logger.addHandler(ch)

#hold the locaton to the test and training sets
TRAIN_LOCATION = os.path.join('.',"train.csv")
TEST_LOCATION = os.path.join('.','test.csv')

#class to strip HTML from a string
#taken from the following StackOverflow post
#	http://stackoverflow.com/questions/753052/strip-html-from-strings-in-python
class MLStripper(HTMLParser):
    def __init__(self):
        self.reset()
        self.strict = False
        self.convert_charrefs= True
        self.fed = []
    def handle_data(self, d):
        self.fed.append(d)
    def get_data(self):
        return ''.join(self.fed)

def strip_tags(html):
    s = MLStripper()
    s.feed(html)
    return s.get_data()


class FeatureMapper:
    def __init__(self, features):
        self.features = features

    def fit(self, X, y=None):
        for feature_name, column_name, extractor in self.features:
            extractor.fit(X[column_name], y)

    def transform(self, X):
        extracted = []
        for feature_name, column_name, extractor in self.features:
            fea = extractor.transform(X[column_name])
            if hasattr(fea, "toarray"):
                extracted.append(fea.toarray())
            else:
                extracted.append(fea)
        if len(extracted) > 1:
            return np.concatenate(extracted, axis=1)
        else: 
            return extracted[0]

    def fit_transform(self, X, y=None):
        extracted = []
        for feature_name, column_name, extractor in self.features:
            fea = extractor.fit_transform(X[column_name], y)
            if hasattr(fea, "toarray"):
                extracted.append(fea.toarray())
            else:
                extracted.append(fea)
        if len(extracted) > 1:
            return np.concatenate(extracted, axis=1)
        else: 
            return extracted[0]

def identity(x):
    return x

class SimpleTransform(BaseEstimator):
    def __init__(self, transformer=identity):
        self.transformer = transformer

    def fit(self, X, y=None):
        return self

    def fit_transform(self, X, y=None):
        return self.transform(X)

    def transform(self, X, y=None):
        return np.array([self.transformer(x) for x in X], ndmin=2).T

#build a list of tuples containing a feature's name, the dataframe column that it is associated with and a function to apply to that data
#
#                          Feature Set Name            Data Frame Column              Transformer
features = FeatureMapper([('QueryBagOfWords',          'query',                       CountVectorizer(max_features=200)),
                          ('TitleBagOfWords',          'product_title',               CountVectorizer(max_features=200)),
                          ('DescriptionBagOfWords',    'product_description',         CountVectorizer(max_features=200)),
                          ('QueryTokensInTitle',       'query_tokens_in_title',       SimpleTransform()),
                          ('QueryTokensInDescription', 'query_tokens_in_description', SimpleTransform()),
                          ('StrippedQueryTokensInTitle','query_stripped_in_title',	  SimpleTransform()),
                          ('StrippedQueryTokensInDescription','query_stripped_in_description', SimpleTransform())],
                          )
"""clf = Pipeline([('v',TfidfVectorizer(min_df=5, max_df=500, max_features=None, strip_accents='unicode', analyzer='word', token_pattern=r'\w{1,}', ngram_range=(1, 2), use_idf=True, smooth_idf=True, sublinear_tf=True, stop_words = 'english')), 
                ('svd', TruncatedSVD(n_components=200, algorithm='randomized', n_iter=5, random_state=None, tol=0.0)), 
                ('scl', StandardScaler(copy=True, with_mean=True, with_std=True)), 
                ('svm', SVC(C=10, kernel='rbf', degree=3, gamma=0.0, coef0=0.0, shrinking=True, probability=False, tol=0.001, cache_size=200, class_weight=None, verbose=False, max_iter=-1, random_state=None))])
"""
#apply porter stemming and remove stop words from queries
#get the stemmer
stemmer =  nltk.stem.snowball.SnowballStemmer("english")

#list of words we want to remove from query.  These are all HTML related phrases that have no impact on the query and we should get rid of them
#add this list ot the default stop word list that comes with sklearn
stop_words = ['http', 'img', 'www', 'border', 'table', 'ul', 'ol', 
				'style', 'color', 'px', 'margin', 'left', 'right', 'font', 'solid', '0px',
				'strong', 'br', ]

stop_words = list(text.ENGLISH_STOP_WORDS.union(stop_words))

def removeStopWordsAndStem(data):
	data['query_stripped'] = ' '
	data['title_stripped'] = ' '
	data['description_stripped'] =' '

	#go through each query and product description and remove all non-alphanumeric characters and stopwords and perform stemming
	for i, row in data.iterrows():
		#strip HTML and make sure strings are all unicode
		query = strip_tags(row['query'].decode('utf-8', 'ignore'))
		title = strip_tags(row['product_title'].decode('utf-8', 'ignore'))
		descript = strip_tags(row['product_description'].decode('utf-8', 'ignore'))

		#strip stop words and stem the rest
        #the description field gives us a lot of UnicodeDecodeErrors

		query = u" ".join([stemmer.stem(word) for word in query.split() if word not in stop_words])
		title = u' '.join([stemmer.stem(word) for word in title.split() if word not in stop_words])
		descript = u' '.join([stemmer.stem(word) for word in descript.split() if descript not in stop_words])


		#add values back to data frame
		data.set_value(i,'query_stripped', query)
		data.set_value(i, 'title_stripped', title)
		data.set_value(i, 'description_stripped',descript)


def extract_features(data):
    """
    Add features to the dataframe
    :param data:	pandas DataFrame
    """

    #regex pattern to find all individual words in a string
    token_pattern = re.compile(r"(?u)\b\w\w+\b")

    #create two columns to store extra features in the dataset
    data["query_tokens_in_title"] = 0.0
    data["query_tokens_in_description"] = 0.0
    data['query_stripped_in_title'] = 0.0
    data['query_stripped_in_description'] = 0.0

    #go through each row of the dataframe and build the two features
    for i, row in data.iterrows():
        query = set(x.lower() for x in token_pattern.findall(row["query"]))
        title = set(x.lower() for x in token_pattern.findall(row["product_title"]))
        description = set(x.lower() for x in token_pattern.findall(row["product_description"]))
        if len(title) > 0:
            data.set_value(i, "query_tokens_in_title", len(query.intersection(title))/len(title))
        if len(description) > 0:
            data.set_value(i, "query_tokens_in_description", len(query.intersection(description))/len(description))

        stripped_query = set(x.lower() for x in token_pattern.findall(row["query_stripped"]))
        stripped_title = set(x.lower() for x in token_pattern.findall(row["title_stripped"]))
        stripped_description = set(x.lower() for x in token_pattern.findall(row["description_stripped"]))
        if len(stripped_title) > 0:
            data.set_value(i, "query_stripped_in_title", float(len(stripped_query.intersection(stripped_title)))/len(stripped_title))
        if len(stripped_description) > 0:
            data.set_value(i, "query_stripped_in_description", float(len(stripped_query.intersection(stripped_description)))/len(stripped_description))
        

if __name__ == "__main__":
	#load data
	logger.info("Loading train and test sets")
	train = pd.read_csv(TRAIN_LOCATION).fillna("")
	test  = pd.read_csv(TEST_LOCATION).fillna("")

	logger.info("Stripping and stemming strings in datasets")
	removeStopWordsAndStem(train)
	removeStopWordsAndStem(test)


	#add the two features to the training and test sets
	logger.info("Creating extra features")
	extract_features(train)
	extract_features(test)

	#train.to_csv("stripped_train.csv", encoding='utf-8')
	#test.to_csv('stripped_test.csv', encoding='utf-8')

	#apply a set of transforms to the training dataset
	logger.info("Training model")
	pipeline = Pipeline([("extract_features", features),
	                     ("classify", RandomForestClassifier(n_estimators=200,
	                                                         n_jobs=1,
	                                                         min_samples_split=2,
	                                                         random_state=1)),
                        #('v',TfidfVectorizer(min_df=5, 
                        #                       max_df=500, max_features=None, 
                        #                        strip_accents='unicode', analyzer='word', 
                        #                        token_pattern=r'\w{1,}', ngram_range=(1, 2), 
                        #                        use_idf=True, smooth_idf=True, sublinear_tf=True, 
                        #                        stop_words = 'english'))
    ])
	pipeline.fit(train, train["median_relevance"])

	#make predicitons on the test set
	logger.info("Making predictions")
	predictions = pipeline.predict(test)

	submission = pd.DataFrame({"id": test["id"], "prediction": predictions})
	submission.to_csv("python_benchmark.csv", index=False)