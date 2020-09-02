# some exploratory data analysis of the new dataset 
data = read.csv('data/English_Chinese_Dyslexics.csv', sep='\t')
head(data)
colnames(data)
ncol(data)

boxplot(data$Group ~ data$L_StartTime)
nrow(data)
length(data$Group[data$Group=='Chinese'])
numChinese <- length(data$Group[data$Group == 'Chinese'])
numEnglish <- length(data$Group[data$Group == 'English'])
numDyslexic <- length(data$Group[data$Group == 'Dyslexic'])
numNormal <- numChinese + numEnglish