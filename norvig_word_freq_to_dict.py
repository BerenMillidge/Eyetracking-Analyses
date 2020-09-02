# simple script to convert norvigs word frequency data to a dictionary
import numpy as np
import cPickle as pickle 
import sys

freqdict = {}
input_file = open("norvig_word_frequency.txt")
try:
	for line in input_file:
		splits = line.split('\t')
		if len(splits) == 2:
			key = splits[0]
			freq = splits[1]
			freq = freq.split('\n')[0]
			freq = int(freq)
			freqdict[key] = freq

finally:
	input_file.close()

try: 
	f = open("norvig_frequency_dict", "w+")
	pickle.dump(freqdict, f)
except Exception as e:
	print e



print "Done!"
