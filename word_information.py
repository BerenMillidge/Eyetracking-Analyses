# A script to compute the character level information in each word using the bigram probability frequencies as a crude approximation
from __future__ import division
import pickle
import collections
import numpy as np
import matplotlib.pyplot as plt
import sys
import pandas as pd

def save(obj, fname):
	pickle.dump(obj, open(fname, 'wb'))

def load(fname):
	return pickle.load(open(fname, 'rb'))

def information(p):
	return p * np.log(p + eps) # epsilon for numerical stbaility!


def sanitize_string(word):
	return str(word).strip().upper() # all keys are in upper case!

def find_fallback(wordbase, i,d):
	if i < 0:
		return 0,0 # word could not be found!
	if wordbase in d:
		return d[wordbase], i
	else:
		return find_fallback(wordbase[1:], i-1,d)

def calculate_word_info(word):
	word = sanitize_string(word)
	print(word)
	if len(word) < 1:
		return 0
	info_list = []
	wordbase = ''
	for i, char in enumerate(word):
		j = i + 1
		if i < dictlist_length:
			d = dictlist[i]
		else:
			d = dictlist[dictlist_length-1]

		if i == 0:
			p = d[char] / d["$total"]
			inf = information(p)
			info_list.append(inf)
			wordbase += char
		else:
			if i >= dictlist_length:
				dprev = dictlist[dictlist_length-1]
			else:
				dprev = dictlist[i-1]
			s = wordbase + char
			if s in d:
				if wordbase in dprev:
					p = d[s] / dprev[wordbase] 
					inf = information(p)
					info_list.append(inf)
					wordbase = s
				else:
					newbase, index = find_fallback(wordbase, i, d)
					p = d[s] / newbase
					inf = information(p)
					info_list.append(inf)
					wordbase = s
			else:
				newprob, index = find_fallback(s, i,d)
				baseprob, j = find_fallback(wordbase, i-1,d)
				p = newprob / (baseprob + eps)
				inf = information(p)
				info_list.append(inf)
				wordbase = s

	return info_list

def calculate_info_gain(word):
	# this is the mutual information
	word = sanitize_string(word)
	info_list = calculate_word_info(word)
	infogain_list = []
	wordbase = ''
	basedict = dictlist[0]
	total = basedict["$total"]
	for i, char in enumerate(word):
		j = i + 1
		if i == 0:
			infogain_list.append(-1 * info_list[0])
		else:
			prevchar = word[i-1]
			curr_p = basedict[char] / total
			curr_info = information(curr_p)
			prev_p = basedict[prevchar] / total
			infogain = curr_info - prev_p * info_list[i]
			infogain_list.append(-1 *infogain)


	return infogain_list



def plot_word_info(infolist, figsize = (30,30)):
	fig = plt.figure()
	xs = range(len(infolist))
	plt.plot(xs, l)
	plt.title('Information Gain per Letter in Word')
	plt.xlabel('Letter index')
	plt.ylabel('Information Gain')
	fig.tight_layout()
	plt.show()


df_fname = "data/data/Jun_data/augmented_English_3_bigrams.csv"
save_fname = "data/data/Jun_data/augmented_English_4_word_info.csv"

if len(sys.argv) > 1:
	df_fname = sys.argv[1]

if len(sys.argv) > 2:
	save_fname = sys.argv[2]


eps = 1e-8
ngram_dict_base = 'gram_dict'
ngram_base_base = "data/Ngrams/"

# load dictionaries into memory
dictlist = []
for i in range(9):
	j = i + 1
	d = load(ngram_base_base + str(j) + ngram_dict_base)
	dictlist.append(d)

dictlist_length = len(dictlist)


def get_word_infos():
	print("Loading dataframe")
	df = pd.read_csv(df_fname, na_values=["NA"], low_memory=False)
	print("Finished reading in dataframe")
	leftwords = df["word_l"]
	rightwords = df["word_r"]
	print("Creating word information columns")
	df["word_information_l"] = [sum(calculate_word_info(word)) for word in leftwords]
	df["word_information_r"] = [sum(calculate_word_info(word)) for word in rightwords]
	df["word_information_gain_l"] = [sum(calculate_info_gain(word)) for word in leftwords]
	df["word_information_gain_r"] = [sum(calculate_info_gain(word)) for word in rightwords]
	print("Saving dataframe")
	with open(save_fname, "w+") as f:
		df.to_csv(f)
		print("Augmented dataframe saved.")


if __name__ == '__main__':
	get_word_infos()

