#Test utility of Ngrams for predicting word saccade speeds
#This script simply parses a collection of words to create the Ngram frequency dict
import csv 
import collections
import cPickle as pickle


#utility pickle functions:
def save(obj, fname):
	pickle.dump(obj, open(fname, 'wb'))

def load(fname):
	return pickle.load(open(fname, 'rb'))

def make_ngram_total_freq_dict(fname, save_name=None):
	d = collections.OrderedDict()
	with open(fname, 'rb') as csvfile:
		reader = csv.reader(csvfile)
		for row in reader:
			d[row[0]] = row[1]

	if save_name:
		save(d, save_name)
	return d

def make_ngram_total_freq_dict_with_total(fname, save_name=None):
	d = collections.OrderedDict()
	with open(fname, 'rb') as csvfile:
		reader = csv.reader(csvfile)
		l = []
		for i, row in enumerate(reader):
			if i > 0:
				val = int(row[1])
				d[row[0]] = val
				l.append(val)
		print type(l[1])
		print l[1]
		d["$total"] = sum(l) 
	if save_name:
		save(d, save_name)
	return d

# now make the N-gram dicts
def make_ngram_dicts():
	for i in range(9):
		j = i + 1 # because 0 indexed

		print "processing ngrams: " + str(j)
		fname = 'ngrams' + str(j) +'.csv'
		save_name = str(j)+'gram_dict'
		make_ngram_total_freq_dict_with_total(fname, save_name)
	print "Done!"


make_ngram_dicts()