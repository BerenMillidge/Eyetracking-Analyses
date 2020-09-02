from eyetracking import *
import numpy as np

length = convert_csv_to_dict('data/FixationLengths.csv')
print(type(length))
print(length.keys())
lens = length['Lengths']
print type(lens)
print len(lens)
lens = np.array(lens).astype(np.float)
print lens.shape
print type(lens)
print np.mean(lens)
print np.std(lens)
np.save("data/FixationLengthsNumpy", lens)