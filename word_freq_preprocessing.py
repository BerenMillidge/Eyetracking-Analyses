import numpy
import pandas as pd

fname = "./data/data/Jun_data/augmented_English_4_word_info.csv"

df = pd.read_csv(fname, na_values=["NA"], low_memory=False)

for row in df.itertuples():
	print(row["word_l"])