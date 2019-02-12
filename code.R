#### SET ENVIRONMENT ####
pacman::p_load(
  "tidyverse",
  "knitr",
  "ggplot2",
  "lubridate",
  "arules",
  "arulesViz",
  "plyr"
)

setwd("C:\Users\usuario\Desktop\UBIQUM\Project 5 - Product Associacions\Market Basket Analysis")               

#### read transactions ####
tr<-  read.transactions("./ElectronidexTransactions2017correcta.csv", format = "basket", sep = ',', rm.duplicates = TRUE)
summary(tr)

#### read category ####
cat<- read.csv("C:\Users\usuario\Desktop\UBIQUM\Project 5 - Product Associacions\Market Basket Analysis\categories.csv",strip.white = TRUE) 
str(cat)

#### read transactions per category ####
trcat <- read.transactions("C:/Users/raul9/Documents/task4/ElectronidexTransactions2017forcategory.csv", format = "basket", sep = ',', rm.duplicates = TRUE)
summary(trcat)

#### explore trcat ####
trcat@itemInfo
cat$labels

#### order ####
cat <- cat[order(cat$labels),]

#### including cat as a label ####
cat$labels <- as.character(cat$labels)

itemInfo(trcat) <- cat

str(cat)

trByType <- aggregate(
  trcat,
  trcat@itemInfo[["level1"]]
)

ruleExplorer(trByType)

#### plot item frequency ####
itemFrequencyPlot(tr, topN= 20, type = "absolute")
image(sample(tr, 100))

#### loop and view category ####
indiv.freq <- itemFrequency(tr,
                            type = 'absolute')

cat.freq <- list()

for (c in unique(cat$level1)){
  temp <- subset(cat, subset = level1 == c) 
  temp$num <- indiv.freq[temp$labels]
  cat.sum <- sum(temp$num)
  cat.freq[[c]] <- cat.sum 
}

cat.freq

#### create a df ####
df<- as.data.frame(cat.freq)
df2 <- t(df)
colnames(df2) <- c("Frequency")
df2 <- as.data.frame(df2)
df2$Category <- as.vector(rownames(df2))
df <- df2
colnames(df)

#### ordenamos df ####
freq <- df[,c(2,1)]
freq

#### plot freq ####
ggplot(freq, aes(Category, Frequency))+geom_col()

#### view data ####
summary(tr)
length(tr)
size(tr)
LIST(tr)
itemLabels(tr)

indiv.freq['Roku Express']

#### jj ####
str(trcat@itemInfo$level1)
inspect(trcat@itemInfo$labels)

itemLabels(trcat)
trcat@itemInfo$level1

#### apriori ####
rules<- apriori(tr, parameter = list(supp = 0.006, conf = 0.6))
rules <- sort(rules, by='confidence', decreasing = TRUE)

#### inspect rules ####
summary(rules)
inspect(rules)
is.redundant(rules)

ruleExplorer(rules)

#### inspect Imac ####
rulesImac<- subset(rules, subset = rhs %in% c("iMac"))
inspect(rulesImac)

plot(rulesImac, method = "graph")
plot(rulesImac, method = "grouped")

inspectDT(rulesImac)

#### inspect HP ####
rulesHP<- subset(rules, subset = rhs %in% c("HP Laptop"))
inspect(rulesHP)

plot(rulesHP, method = "graph")
plot(rulesHP, method = "grouped")

inspectDT(rulesHP)