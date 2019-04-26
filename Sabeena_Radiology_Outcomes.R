#imported the data using 3 methods, compared them all and they are all the same. Around line 70 is were I write it to csv and then set the variable I use later on

setwd("/Users/macbook/Documents/McGill School/Sabeena_Radiology_Outcomes")

#import the dta file and inspect it
#install.packages("haven")
library(haven)
radio_data <-read_dta("june9th.dta")
head(radio_data)
str(radio_data)


library("Hmisc")
describe(radio_data)
nrow(radio_data)


#convert the dta file to a csv
#install.packages("rio")
library(rio)

#this one takes a long time, so I # in front to make sure I don't accidentally run in unless I want to 
#convert("june9th.dta", "june9th.csv")

radio_data2 <- read.csv("june9th.csv")
head(radio_data2)
nrow(radio_data2)
str(radio_data2)

#to make sure the two different methods of import are the same, it looks like this gives a bunch of zeros....so yes, the same
#radio_data - radio_data2

#got sent a .csv that was exported from stata, so this should be the real deal. Take a look to compare to the other two
radio_data3 <- read.csv("marshall.csv")
head(radio_data3)
nrow(radio_data3)
str(radio_data3)

radio_data - radio_data3

all.equal(radio_data2, radio_data3)

str(as.data.frame(radio_data))

######After I got sent the stata exported csv, I organized it the way I organizd the original import and compared them

#just the left 8 columns of the df, this is the 3rd import, just using to check later. 
radio_left3 <- radio_data3[ , c(1:8)]
head(radio_left3)
#cut out the completely NA rows
comp_radio_left3 <- radio_left3[rowSums(is.na(radio_left3)) != 8, ]
nrow(comp_radio_left3)
str(radio_left3)

#just the left 8 columns, the _01 and _02 might just be nonsense that we are not looking at
radio_left <- radio_data[ , c(1:8)]
head(radio_left)
nrow(radio_left)
str(radio_left)

#need to make the _left and _left3 the same format, couldn't get the tbl into a clean dataframe, so made them both matrices. Do this so they are the same format for all.equal......and they are!
radio_left_mt <- as.matrix(radio_left)
str(radio_left_mt)
radio_left3_mt <- as.matrix(radio_left3)
str(radio_left3_mt)
all.equal(radio_left3_mt, radio_left_mt)

#all three data imports are the same, can write it to a csv. Also going to write it as a different variable as a point of control
TAT_data_all <- radio_left
head(TAT_data_all)
describe(TAT_data_all)
str(TAT_data_all)
head(TAT_data_all)
#write.csv(TAT_data_all, file = "247_short_with_all_NA.csv")
#1.4 million mdex missing

#clear out the completey missing rows and then write to .csv
TAT_data_no8na <- TAT_data_all[rowSums(is.na(TAT_data_all)) != 8, ]
head(TAT_data_no8na)
describe(TAT_data_no8na)
str(TAT_data_no8na)
#95k mdex missing
#write.csv(TAT_data_no8na, file = "247_short_with_no_completely_NA.csv")

#clear out the all missing and all-but-id missing rows. This is probably the data set to work with
TAT_data_no78na <- TAT_data_all[rowSums(is.na(TAT_data_all)) < 7, ]
head(TAT_data_no78na)
describe(TAT_data_no78na)
str(TAT_data_no78na)
#9318 missing mdex
#write.csv(TAT_data_no78na, file = "247_short_with_no_7or8_NA.csv")


#take all NA rows out and write to .csv
TAT_data_nona <- TAT_data_all[rowSums(is.na(TAT_data_all)) == 0, ]
sum(is.na(TAT_data_nona))
#write.csv(TAT_data_nona, file = "247_short_no_NA.csv")
nrow(radio_no_NA)
#no missing mdex

#make a short version of the list in order to run winbugs on my laptop....ouf, 2000 was pretty slow, 4000 didn't work
TAT_data_nona_trunk <- TAT_data_nona[1:3000, ]
nrow(TAT_data_nona_trunk)
#write.csv(TAT_data_nona_trunk, file = "247_short_no_NA_trunk.csv")

TAT_data_no78na_trunk <- TAT_data_no78na[1:3000, ]
nrow(TAT_data_no78na_trunk)
#write.csv(TAT_data_no78na_trunk, file = "247_short_no_7or8_NA_trunk.csv")



#need to set int for cats
TAT_data_no78na$ctas <- as.factor(TAT_data_no78na$ctas)
TAT_data_no78na$imaging <- as.factor(TAT_data_no78na$imaging)
TAT_data_no78na$todseen <- as.factor(TAT_data_no78na$todseen)
TAT_data_no78na$trauma <- as.factor(TAT_data_no78na$trauma)
TAT_data_no78na$group <- as.factor(TAT_data_no78na$group)
TAT_data_no78na$adm <- as.factor(TAT_data_no78na$adm)

library(ggplot2)
library(GGally)

ggplot(data = TAT_data_no78na, aes(x = mdex, color = group)) + 
  geom_histogram(binwidth = 200, alpha = 0.2, position = "identity", fill = "white")
#looks like poisson, looks like group 1 and 2 have extreme observations

#look at the 0-2000 mdex range
ggplot(data = TAT_data_no78na, aes(x = mdex, color = group)) + 
  geom_histogram(binwidth = 50, alpha = 0.2, position = "identity", fill = "white") + xlim(0, 2000) 

#the 95th quantil is this
quantile(TAT_data_no78na$mdex, 0.95, na.rm = T)

mean(TAT_data_no78na$mdex, na.rm = T)
sd(TAT_data_no78na$mdex, na.rm = T)

quantile(TAT_data_no78na$mdex, 0.995, na.rm = T)

ggplot(data = TAT_data_no78na, aes(x = group, y = mdex)) + geom_boxplot()
#looks like both have extreme values, maybe more in group 2

#mean mdex in each group
tapply(TAT_data_no78na$mdex, TAT_data_no78na$group, mean, na.rm = T)
tapply(TAT_data_no78na$mdex, TAT_data_no78na$group, median, na.rm = T)
tapply(TAT_data_no78na$mdex, TAT_data_no78na$group, sd, na.rm = T)
tapply(TAT_data_no78na$mdex, TAT_data_no78na$group, count, na.rm = T)
#####I have the same mean and sds for each group as Sabeena

length(which(TAT_data_no78na$group==1))
length(which(TAT_data_no78na$group==2))
#####I have different number of observations than her though

#check to see # of obs in each group, gorup 2 has 3033 more observation
nrow(TAT_data_no78na[TAT_data_no78na$group == 1, ]) - nrow(TAT_data_no78na[TAT_data_no78na$group == 2, ])

#check how many mdex are NA, there are 9318 NAs, that's 5.3% of them
sum(is.na(TAT_data_no78na$mdex))/nrow(TAT_data_no78na)

#check to see how many group NAs, none
sum(is.na(TAT_data_no78na$group))/nrow(TAT_data_no78na)

#to look at all relationships, this is a slow one so I've #'ed it out
#ggpairs(TAT_data_no78na)

#####missing data below AND THIS IS LINEAR, it should be poisson
#check some regressions, missing data
simple.lm <- lm(data = TAT_data_no78na, mdex ~ group)
summary(simple.lm)
confint(simple.lm)
#it says that group 2 has a redution in time from 8 to 1.5 minutes less, average for group 1 is 257

#throw all variables in there and see what we get (there is missing data)
all.multi.lm <- lm(data = TAT_data_no78na, mdex ~ . - mdex -id)
summary(all.multi.lm)
confint(all.multi.lm)
#now it's a reduction of 4 to increase of 1 minute


#check some poisson regressions, missing data
simple.glm <- glm(data = TAT_data_no78na, mdex ~ group, family = "poisson")
summary(simple.glm)
exp(confint(simple.glm))
#it has baseline time as 259 minutes and then the "rate ratio" as 0.98. In this case the rate ratio would just be the time to event ratio. TAT is 2% faster?

#throw all variables in there and see what we get (there is missing data)
all.multi.glm <- glm(data = TAT_data_no78na, mdex ~ . - mdex - id, family = "poisson")
summary(all.multi.glm)
all.multi.glm.confint <- exp(confint(all.multi.glm))
all.multi.glm.confint
#Now the effect of group is 0.995, ie: turn around times are 0.05% faster. The intercept is now way different too, which is odd....no wait, that's okay since it is now descibing something else, not just the average of group 1. The intercept is the simplest case (eg: no trauma, came in the morning, not admited to hospital)

#look at BIC model selection 
library(BMA)

#put the nbest to a high number hoping to get more models in the output, but they didn't, so maybe I didn't do it right. Can do nbest  1 million
bic_out <- bic.glm.formula(data = TAT_data_nona, f = mdex ~ group + ctas + imaging + todseen + trauma + adm, glm.family = "poisson", nbest = 10000)

summary(bic_out)
bic_out$mle

#this is just to see which models look promising
?bicreg
bic_out$mle
#it straight up cuts out group. Damn. That's cold son.

#make a summary stat table? put means, medians, IQR, and % missing data
#install.packages("sjPlot")
library("sjPlot")
# I can't seem to get sjPlot to work

#install.packages("stargazer")
library("stargazer")
stargazer(all.multi.glm, type = "text")

#stargazer wants it to be a data frame, it also doesn't do factor varaibles, I change them back down below
stargazer(TAT_data_no78na, type = "html", out = "radio.doc")

TAT_df_no78na <- as.data.frame(TAT_data_no78na)

stargazer(TAT_df_no78na, type = "text")

TAT_df_no78na_desc <- TAT_df_no78na
TAT_df_no78na_desc$ctas <- as.numeric(TAT_df_no78na_desc$ctas)
TAT_df_no78na_desc$imaging <- as.numeric(TAT_df_no78na_desc$imaging)
TAT_df_no78na_desc$todseen <- as.numeric(TAT_df_no78na_desc$todseen)
TAT_df_no78na_desc$trauma <- as.numeric(TAT_df_no78na_desc$trauma)
TAT_df_no78na_desc$group <- as.numeric(TAT_df_no78na_desc$group)
TAT_df_no78na_desc$adm <- as.numeric(TAT_df_no78na_desc$adm)

stargazer(TAT_df_no78na_desc, digits = 2, type = "text")

stargazer(subset(TAT_df_no78na_desc, group == 1), digits = 2, type = "text")
stargazer(subset(TAT_df_no78na_desc, group == 2), digits = 2, type = "text")

#maybe just make my own table? These packages aren't making life that much easier. 

#started this, but got sleepy, may come back later. This still has the old data in it. Maybe not worth getting into it....
d.variable <- c("Md Time", "", "CTAS", "", "Imaging Done", "", "Time of Day", "", "Trauma", "", "Admitted", "")
d.group <- c(rep(c("Pre 24/7", "Post 24/7"), 6))
d.means <- c(round(mean(subset(radio_descriptive_df, group ==1)[ , "mdex"], na.rm = T), 1), round(mean(subset(radio_descriptive_df, group ==2)[ , "mdex"], na.rm = T), 1), rep("", 10))
d.medians <- c(median(subset(radio_descriptive_df, group ==1)[ , "mdex"], na.rm = T), median(subset(radio_descriptive_df, group ==2)[ , "mdex"], na.rm = T), 
             median(subset(radio_descriptive_df, group ==1)[ , "ctas"], na.rm = T), median(subset(radio_descriptive_df, group ==2)[ , "ctas"], na.rm = T),
             median(subset(radio_descriptive_df, group ==1)[ , "imaging"], na.rm = T), median(subset(radio_descriptive_df, group ==2)[ , "imaging"], na.rm = T),
             median(subset(radio_descriptive_df, group ==1)[ , "todseen"], na.rm = T), median(subset(radio_descriptive_df, group ==2)[ , "todseen"], na.rm = T),
             median(subset(radio_descriptive_df, group ==1)[ , "trauma"], na.rm = T), median(subset(radio_descriptive_df, group ==2)[ , "trauma"], na.rm = T),
             median(subset(radio_descriptive_df, group ==1)[ , "adm"], na.rm = T), median(subset(radio_descriptive_df, group ==2)[ , "adm"], na.rm = T))

d.iqrs <- c(IQR(subset(radio_descriptive_df, group ==1)[ , "mdex"], na.rm = T), IQR(subset(radio_descriptive_df, group ==2)[ , "mdex"], na.rm = T), 
          IQR(subset(radio_descriptive_df, group ==1)[ , "ctas"], na.rm = T), IQR(subset(radio_descriptive_df, group ==2)[ , "ctas"], na.rm = T),
          IQR(subset(radio_descriptive_df, group ==1)[ , "imaging"], na.rm = T), IQR(subset(radio_descriptive_df, group ==2)[ , "imaging"], na.rm = T),
          IQR(subset(radio_descriptive_df, group ==1)[ , "todseen"], na.rm = T), IQR(subset(radio_descriptive_df, group ==2)[ , "todseen"], na.rm = T),
          IQR(subset(radio_descriptive_df, group ==1)[ , "trauma"], na.rm = T), IQR(subset(radio_descriptive_df, group ==2)[ , "trauma"], na.rm = T),
          IQR(subset(radio_descriptive_df, group ==1)[ , "adm"], na.rm = T), IQR(subset(radio_descriptive_df, group ==2)[ , "adm"], na.rm = T))

cbind(d.variable, d.group, d.means, d.medians, d.iqrs)


#realized there was somthing funky about the subseting, that's because there are a lot of missing "group" data
sum(is.na(subset(radio_descriptive_df, group ==1)[ , "mdex"])) + sum(is.na(subset(radio_descriptive_df, group ==2)[ , "mdex"]))
length(is.na(radio_descriptive_df$mdex))
length(is.na(subset(radio_descriptive_df, group ==1)[ , "mdex"])) + length(is.na(subset(radio_descriptive_df, group ==2)[ , "mdex"])) + sum(is.na(radio_descriptive_df$group))


#without spliting, the amount of missing data in each col
apply(TAT_df_no78na_desc, 2, function(col)sum(is.na(col))/length(col))
#so 22.5% of group is missing data? that makes sense, 22.5% of the data subset only has id so that is the baseline missing information. The above and beyond information that is missing is the mdex and todseen which are about 3% higher

#look at how often the row has mulitple NAs?
freq.missing <- apply(TAT_df_no78na_desc, 1, function(row)sum(is.na(row)))
sum(freq.missing == 7)
sum(freq.missing == 2)
sum(freq.missing == 1)
histogram(freq.missing)
sum(is.na(TAT_df_no78na_desc$id))

freq.missing.all <- apply(TAT_data_all, 1, function(row)sum(is.na(row)))
histogram(freq.missing.all)


##
##
##
##
## Stopped updating here, a little tired. Could get into it, but not going to right now.

missing.whole.row
n.tot <- nrow(radio_stacked)
n.sub <- nrow(radio_tidy)

#porportion of total
values.missing <- c(1:8)
proportion <- round(c(sum(freq.missing == 1)/n.tot, sum(freq.missing == 2)/n.tot, sum(freq.missing == 3)/n.tot, sum(freq.missing == 4)/n.tot, 
                sum(freq.missing == 5)/n.tot, sum(freq.missing == 6)/n.tot, sum(freq.missing == 7)/n.tot, missing.whole.row/n.tot), 5)

proportion.missing <- data.frame(values.missing, proportion)
proportion.missing
#soooooo 92.2% of the rows are completely blank, and 94% are either completely blank or just have the id


#proportion of those with 7 or less missing values
values.missing.sub <- c(1:7)
proportion.sub <- round(c(sum(freq.missing == 1)/n.sub, sum(freq.missing == 2)/n.sub, sum(freq.missing == 3)/n.sub, sum(freq.missing == 4)/n.sub, 
                      sum(freq.missing == 5)/n.sub, sum(freq.missing == 6)/n.sub, sum(freq.missing == 7)/n.sub), 6)

proportion.missing.sub <- data.frame(values.missing.sub, proportion.sub)
proportion.missing.sub
#soooo of the "not completely blank" rows, 22.5% only have the id and nothing else. 3% are missing 2 values. 

#proportion of those with 6 or less missing values
values.missing.sub.sub <- c(1:6)
radio_6orless_NA <- radio_stacked[rowSums(is.na(radio_stacked)) < 7, ]
n.sub.sub <- nrow(radio_6orless_NA)
proportion.sub.sub <- round(c(sum(freq.missing == 1)/n.sub.sub, sum(freq.missing == 2)/n.sub.sub, sum(freq.missing == 3)/n.sub.sub, sum(freq.missing == 4)/n.sub.sub, 
                          sum(freq.missing == 5)/n.sub.sub, sum(freq.missing == 6)/n.sub.sub), 6)

proportion.missing.sub.sub <- data.frame(values.missing.sub.sub, proportion.sub.sub)
proportion.missing.sub.sub


  
#tried to use sjPlot, but it's getting messy with all the package junk.
#install.packages("TMB")
library(TMB)
library(sjPlot)

#simple imputation? Can't really impute the group, can I 




#cut away most of the data so I can quickly check my regression setup to make sure the code works. 
work_radio_data <- radio_data[c(1:50), ]
work_radio_data

#figuring out how to cut out wholely NA rows of an 8 wide df
test <- work_radio_data[rowSums(is.na(work_radio_data)) != 8, ]
test
is.na(radio_left[ , ]) == 8



#notes on confounding potential: ctas and adm, mdex and adm, imaging and adm, 
#group doesn't seem to have a relationship with any variables.....except maybe mdex

