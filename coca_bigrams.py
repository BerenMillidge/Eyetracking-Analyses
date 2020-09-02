# script to add a bigram frequency column to a dataframe
# bigram data from the corpus of contemporary american english (COCA)

import numpy as np
import pandas as pd
import sys
from nltk.stem import *

def create_bigram_dict(bigram_fname):
	print("Creating bigram dict!")
	d = {}
	with open(bigram_fname, 'r', encoding = "ISO-8859-1") as f:
		lines = f.readlines()
		for line in lines:
			splits = line.strip().split("\t") # assume that's the separator
			if len(splits) != 3:
				print("Line has incorrect number of splits: " + str(line))

			freq, prev_word, cur_word = splits
			d[prev_word + bigram_sep + cur_word] = int(freq)
	return d

def get_freq(cur_word, prev_word):

	cur_word = cur_word.strip().lower()
	prev_word = prev_word.strip().lower()

	word = prev_word + bigram_sep + cur_word
	if word in freqdict.keys():
		return freqdict[word]

	porterStem = porterStemmer.stem(cur_word)
	word = prev_word + bigram_sep + porterStem
	if word in freqdict.keys():
		return freqdict[word]
	snowballStem = snowballStemmer.stem(cur_word)
	word = prev_word + bigram_sep + snowballStem
	if snowballStem in freqdict.keys():
		return freqdict[word]

	porterStem = porterStemmer.stem(prev_word)
	word = porterStem + bigram_sep + cur_word
	if word in freqdict.keys():
		return freqdict[word]
	snowballStem = snowballStemmer.stem(prev_word)
	word = snowballStem + bigram_sep + cur_word
	if snowballStem in freqdict.keys():
		return freqdict[word]

	curStem = porterStemmer.stem(cur_word)
	prevStem = porterStemmer.stem(prev_word)
	word = prevStem + bigram_sep + curStem
	if word in freqdict.keys():
		return freqdict[word]

	curStem = snowballStemmer.stem(cur_word)
	prevStem = snowballStemmer.stem(prev_word)
	word = prevStem + bigram_sep + curStem
	if snowballStem in freqdict.keys():
		return freqdict[word]

	else:
		return None 

def bigram_frequencies(wordlist):
	l = []
	prev = "qwzzq" # arbitrary... hope it doesn't exist!
	for word in wordlist:
		w = str(word)
		freq = get_freq(w, prev)
		l.append(freq)
		prev = w
	return l



df_fname = './data/data/Jun_data/augmented_English_2.csv'
bigram_fname = "./data/word_Ngrams/w2_.txt"

porterStemmer = PorterStemmer()
snowballStemmer = SnowballStemmer("english")

bigram_sep = "$$$"

if len(sys.argv) > 1:
	df_fname = sys.argv[1]

if len(sys.argv) > 2:
	bigram_fname = sys.argv[2]



freqdict = create_bigram_dict(bigram_fname)

df = pd.read_csv(df_fname, na_values=["NA"], low_memory=False)
print("Read in dataframe")
print(type(df))
leftwords = df["word_l"]
rightwords = df["word_r"]
print(type(leftwords))
print(len(leftwords))
df["bigram_frequencies_l"] = bigram_frequencies(leftwords)
df["bigram_frequencies_r"] = bigram_frequencies(rightwords)
#save_name = str(df_fname.split('.')[:-1]) + "_bigrams.csv"
#save_name= "test_bigrams.csv"
save_name = "./data/data/Jun_data/augmented_English_3_bigrams.csv"
print("Saving dataframe!")
with open(save_name, 'w+') as f:
	df.to_csv(f)
