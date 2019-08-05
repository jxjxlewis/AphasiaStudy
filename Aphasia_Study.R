#Author: Jordan Lewis
#Aphasia Study Data Analysis


setwd("C:\\Users\\jxjxl\\Documents\\Freshman S2\\ST 308 R\\project 3")
library(Rlab)

#Read in both sheets of the Aphasia Study data, did this by manually creating two csv
#through excel and reading each in
aphasic <- read.csv("aphasic.csv")
nonaphasic <- read.csv("nonaphasic.csv")

head(aphasic)
head(nonaphasic)

#Creating list of column names for each data set
colnames1 <- c("gender","group","duration","words_per_sec",
               "utter_per_sec","AQ","edu_yr","words","nonword",
               "phon_nonword","phon_word","demonstratives",
               "nouns","pronouns","verbs")

colnames2 <- c("gender","group","duration","words_per_sec","utter_per_sec","edu_yr","pronouns",
               "verbs","nouns","demonstratives")
names(aphasic) <- colnames1
names(nonaphasic) <- colnames2

#Some Exploratory Analysis
dim(aphasic) #138 rows, 15 cols
dim(nonaphasic) #143 rows, 10 cols

#Combine the two R objects by the correct varaibles
full.data <- rbind(aphasic[,colnames2], nonaphasic)


#MOre exploratory Analysis checking procedure done correctly
dim(full.data) #281 rows, 10 cols which is correct


#unique(full.data$group) finds the possible values of the group variable. 
#delete "NotAphasicByWAB". Defined a new variable to full.data called atype- 
#equal to "aphasia" if group is not equal to "control" and is equal to "control" otherwise. 
unique(full.data$group)
full.data <- full.data[full.data$group != "NotAphsicByWAB", ]
full.data$atype <- ifelse(full.data$group == "control", "control", "aphasic")


#found descriptive statistics for the variables nouns and verbs by atype. 
stats(full.data$nouns, full.data$atype)
stats(full.data$verbs, full.data$atype)

#The output::
#> stats(full.data$nouns, full.data$atype)
#aphasic   control
#N              138.000000 143.00000
#mean             9.557971  22.84615
#Std.Dev.         7.378226  13.90476
#min              0.000000   6.00000
#Q1               5.000000  14.00000
#median           8.000000  20.00000
#Q3              12.000000  25.00000
#max             55.000000 100.00000
#missing values   0.000000   0.00000
#> stats(full.data$verbs, full.data$atype)
#aphasic   control
#N              138.000000 143.00000
#mean             6.500000  15.37762
#Std.Dev.         6.875589  11.80762
#min              0.000000   4.00000
#Q1               1.250000   8.00000
#median           5.000000  12.00000
#Q3               9.000000  18.00000
#max             44.000000  73.00000
#missing values   0.000000   0.00000


#############################################################

#The data in ozone are the monthly mean concentration (in Dobson units) of the ozone layer at Arosa,
#Switzerland, from 1926 to 1971 (from Andrews and Herzberg, 1985, p. 75-76). 
#Exploratory Analysis
ozone[1:5,]

#Setting up and making boxplots
set.panel(2,1)
boxplot(ozone~month, data = ozone)
bplot(ozone$ozone, by = ozone$month)
#The first boxplot has the months in alphebetical order rather than chronological order.
#in addition, the boxs in the frist are alot wider and harder to read to notice a trend. In contrast, the bplot
#effecivly shows that there is a curve trend over time as the boxes are easier to read.


#Looked at the means and standard deviations by month.
temp <- stats(ozone$ozone, by = ozone$month)
round(temp, 1)
str(temp) #temp is a list with two objects


#Plotted the means versus month on the top figure and the standard deviations on the bottom
o.mean <- temp[2, 1:12]
o.sd <- temp[3, 1:12]
lplot(colnames(temp),o.mean)
lplot(colnames(temp),o.sd)


#Alter plot aesthetics
lplot(colnames(temp), o.mean, tcex = 2)
lplot(colnames(temp), o.sd, tcex = 2)

lplot(colnames(temp),o.mean, tcex = 1, ylim=c(250,400), ylab = ("Mean Ozone"))
lplot(colnames(temp),o.sd, tcex = 1, ylim=c(0, 35), ylab = ("Spread of Ozone"))

lplot(colnames(temp),o.mean, tcex = 1, ylim=c(250,400), ylab = ("Mean Ozone"))
title("Mean Ozone Concentration by Month")
lplot(colnames(temp),o.sd, tcex = 1, ylim=c(0, 35), ylab = ("Spread of Ozone"))
title("Spread in Ozone Concentration by Month")

#The mean ozone concentration goes from being around 350 units then dipping from sep-nov and going back up
#with a peak in march-april and then goes on the decline until june.
#The sd ozone concentration is relativly flat from july to november at around 10, then increases and peaks at 
#around 30 during febuary and then declines until june. 


###############################################################



#The data set draft contains average lottery numbers by month for the 1970 Draft Lottery (randomly drawn in Dec. 1969). 
#These numbers determined the order by which draft age youth were drafted in 1970. 
#For example, a person whose birthday received number 63 was drafted fairly early in 1970; a person with number 300 was not drafted at all.
#One of the variables, draft$order, breaks the months into two groups: "first" is for January through June and "second" is for
#July through December. If the draft numbers (1-366) were actually random, then monthly averages of draft$lottnum should be close to 183.5.

draft
draft$order
stats(draft$lottnum, by = draft$order)

# First gorup mean = 260.3333, second group mean = 160.83333
#From this information, we can come to the null hypothesis that the the draft numbers are not random
#and thus we should carry out a ttest. The null hypothesis is that the true difference in means is zero.

t.test(lottnum ~ order, data = draft, alternative = "two.sided")


# The mean of the first group is 160.8333 and the mean of the second group is 206.3333. So far, 
#we think that the draft numbers are not random.
# Carry out a t-test to be sure. True difference in means should be 0. 

t.test(lottnum ~ order, data = draft, alternative = "two.sided")

#With a p-val of .00335, which is less than our alpha level of .05 we can reject the null hypothesis
#There is sufficient evidence that the means of lottery numbers of the first and second group are different.
#Thus, the lottery numbers are not random and we can confidently say that is is more likely to get drafted
#earlier with an earlier brithday