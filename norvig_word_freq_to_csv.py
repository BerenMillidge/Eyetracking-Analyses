# quick script to add the norvig word frequency dictionary to existing dataframe

import sys
from eyetracking import *
import pickle
import pandas as pd
from nltk.stem import *

csv_sep = ","

fname = ''

freqdict_fname = 'data/norvig_word_frequency.txt'

porterStemmer = PorterStemmer()
snowballStemmer = SnowballStemmer("english")

def parse_txt_to_dict(fname):
	freqdict = {}
	input_file = open(fname)
	for line in input_file:
		splits = line.split('\t')
		if len(splits) == 2:
			key = splits[0]
			freq = splits[1]
			freq = freq.split('\n')[0]
			freq = int(freq)
			freqdict[key] = freq

	input_file.close()
	return freqdict
	


if len(sys.argv) > 1:
	fname = sys.argv[1]

if len(sys.argv) > 2:
	freqdict_fname = sys.argv[2]

if len(sys.argv) > 3:
	csv_sep = sys.argv[3]


def get_freq_dict(get_freq_dict):
	freqdict = {}
	print("In get freq dict!")
	with open(freqdict_fname, 'r') as f:
		lines =f.readlines()
		print(lines[0:5])
		for line in lines:
			line = line.strip()
			#print(line)
			splits =  line.split('\t')
			if len(splits) !=2:
				print("Line split incorrectly: length " + str(len(splits)))
				print(line)
				continue
			else:
				word, freq = splits
				freqdict[word] = freq

	return freqdict




freqdict = get_freq_dict(freqdict_fname)
#print(freqdict.keys())



def getfreq(word):
	print("In getfreq")
	print(word)
	word = word.strip().lower()
	if word in freqdict.keys():
		return freqdict[word]
	porterStem = porterStemmer.stem(word)
	if porterStem in freqdict.keys():
		return freqdict[porterStem]
	snowballStem = snowballStemmer.stem(word)
	if snowballStem in freqdict.keys():
		return freqdict[snowballStem]
	else:
		return None # this is as a missing value in pandas!

df = pd.read_csv(fname, na_values=["NA"], low_memory = False)

df["norvig_wordFreq_l"] = [getfreq(word) for word in list(df["word_l"])]
df["norvig_wordFreq_r"] = [getfreq(word) for word in list(df["word_r"])]
basefname = str(fname.split('.')[:-1][0]) + "_norvig.csv"
with open(basefname, 'w+') as f:
	df.to_csv(f)
