setwd("/Users/macbook/Documents/McGill School/Sabeena_Radiology_Outcomes")

#import the dta file and inspect it
install.packages("haven")
library(haven)
radio_data <-read_dta("june9th.dta")
head(radio_data)
str(radio_data)


library("Hmisc")
describe(radio_data)
nrow(radio_data)


#convert the dta file to a csv
install.packages("rio")
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

#take a look at the times
time_data <- read.csv("Dataspread sheet_convertminutes.csv")
head(time_data)
str(time_data)

#figuring out how many missing values there are. These are the numbers from the id columns from the describe() output
(82397+260745+38273)/(1559854+1381506+1603978+82397+260745+38273)

#stack the data, could use dplyr, but I'm being lazy.....or maybe cautious. Do this by cutting into 3, 8 varaible df, cuting out the completely NA rows, and then stacking them
radio_left <- radio_data[ , c(1:8)]
head(radio_left)
#cut out the completely NA rows
comp_radio_left <- radio_left[rowSums(is.na(radio_left)) != 8, ]
nrow(comp_radio_left)

radio_mid <- radio_data[ , c(9:16)]
head(radio_mid)
comp_radio_mid <- radio_mid[rowSums(is.na(radio_mid)) != 8, ]
nrow(comp_radio_mid)
colnames(comp_radio_mid) <- colnames(comp_radio_left)

radio_right <- radio_data[ , c(17:24)]
head(radio_right)
comp_radio_right <- radio_right[rowSums(is.na(radio_right)) != 8, ]
nrow(comp_radio_right)
colnames(comp_radio_right) <- colnames(comp_radio_left)

#stack em
radio_tidy <- rbind(comp_radio_left, comp_radio_mid, comp_radio_right)
head(radio_tidy)
describe(radio_tidy)
str(radio_tidy)

#need to set int for cats


#now how many missing
85916/(85916+295499)

#explore a bit more, some graphs too



#cut away most of the data so I can quickly check my regression setup to make sure the code works. 
work_radio_data <- radio_data[c(1:50), ]
work_radio_data

#figuring out how to cut out wholely NA rows of an 8 wide df
test <- work_radio_data[rowSums(is.na(work_radio_data)) != 8, ]
test
is.na(radio_left[ , ]) == 8


######After I got sent the stata exported csv, I organized it the way I organizd the original import and compared them

#stack the data, could use dplyr, but I'm being lazy.....or maybe cautious. Do this by cutting into 3, 8 varaible df, cuting out the completely NA rows, and then stacking them
radio_left3 <- radio_data3[ , c(1:8)]
head(radio_left3)
#cut out the completely NA rows
comp_radio_left3 <- radio_left3[rowSums(is.na(radio_left3)) != 8, ]
nrow(comp_radio_left3)

radio_mid3 <- radio_data3[ , c(9:16)]
head(radio_mid3)
comp_radio_mid3 <- radio_mid3[rowSums(is.na(radio_mid3)) != 8, ]
nrow(comp_radio_mid3)
colnames(comp_radio_mid3) <- colnames(comp_radio_left3)

radio_right3 <- radio_data3[ , c(17:24)]
head(radio_right3)
comp_radio_right3 <- radio_right3[rowSums(is.na(radio_right3)) != 8, ]
nrow(comp_radio_right3)
colnames(comp_radio_right3) <- colnames(comp_radio_left3)

#stack em
radio_tidy3 <- rbind(comp_radio_left3, comp_radio_mid3, comp_radio_right3)
head(radio_tidy3)
describe(radio_tidy3)
str(radio_tidy3)

all.equal(radio_tidy, radio_tidy3)

