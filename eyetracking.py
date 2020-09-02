import csv 
import collections
import cPickle as pickle


#utility pickle functions:
def save(obj, fname):
	pickle.dump(obj, open(fname, 'wb'))

def load(fname):
	return pickle.load(open(fname, 'rb'))

def convert_csv_to_dict(fname, save_name=None):
	with open(fname, 'rb') as csvfile:
		reader = csv.reader(csvfile)
		row_num = 0
		data = collections.OrderedDict()
		keylist=  []
		for row in reader:
			if row_num == 0:
				# setup the dict keys
				for key in row:
					data[key] = []
					keylist.append(key)
			else:
				for i in xrange(len(row)):
					elem = row[i]
					key = keylist[i]
					data[key].append(elem)
			row_num +=1

	if save_name is not None:
		save(data, save_name)
	return data


#convert_csv_to_dict('EnglishTypical.csv', 'EnglishTypical')

data = load('EnglishTypical')
print type(data)
print data.keys()
for key in data.keys():
	print len(data[key])

def means(data, keys):
	means = []
	for key in keys:
		means.append(np.mean(np.array(data[key])))
	return means

