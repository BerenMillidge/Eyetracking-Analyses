from __future__ import division
import os
import pandas as pd
import time 
from ParserAsc import ParseEyeLinkAsc

datadir = "English/Block1/ASC/"
elfilename = "SUB01B1E.asc"
outdir = datadir
savedir = "/tests/pythonparsertests/"


# load data into pandas dataframes
os.chdir(datadir)
# load the file in!
dfTrial, dfMsg, dfFix, dfSacc, dfBlink, dfSamples = ParseEyeLinkAsc(elfilename)

savenamebase = elfilename.split('.')[0]
alldfs = [dfTrial, dfMsg, dfFix, dfSacc, dfBlink, dfSamples]
allNames = ['Trial', 'Message','Fixation','Saccade','Blink','Sample'] 
# all write to csv file

for i in range(len(alldfs)):
	outfname = savedir + savenamebase + "_" + allNames[i]
	alldfs[i].to_csv(outfname, float_format='%.1f', index=False)

