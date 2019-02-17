##################################################
## Project: Market Basket Analysis - eCommerce
## Script purpose: look for association rules between products in Electronidex
## Date: 2 Jan 2018
## Author: Maria Farrés
##################################################

#################################### SET ENVIRONMENT########################################

# load required libraries
pacman::p_load("tidyverse","knitr", "ggplot2",
               "lubridate","arules","arulesViz",
               "reshape2", "plyr", "esquisse")

# set working directory
setwd("C:/Users/usuario/Desktop/UBIQUM/Project 5 - Product Associacions/Market-Basket")  




############################## IMPORTATION & INITIAL EXPLORATION OF DATA ##############################

# read initial data sets and explore
tr_df  <- read.csv("./DataSets/transactions.csv", header = FALSE) # read transactions as df
sum(tr_df !="") # check total number of products sold
tr_matrix <- data.matrix(tr_df) # read as matrix


# read "transactions.csv" as basket to perform association rules analysis
tr <- read.transactions("./DataSets/transactions.csv",  
                        format = 'basket',
                        sep = ',',
                        rm.duplicates = FALSE) # same number of objects when TRUE 


# initial exploration of transactional data 
summary(tr) # 9835 total transactions
itemLabels(tr) # list of unique products (125 prod.)
image(sample(tr, 150)) # plot a random sample of transactions to detect initial patterns




# read initial data "categories.csv" giving overall product categories in Electronidex
# all the products contained in "transactions" belong to one of these categories
cat <- read.csv("./DataSets/categories.csv",strip.white = TRUE) 
str(cat) # categories need to be included as labels to the items in tr 



################################### TRANSFORM DATA #####################################

# so, in order to associate each product in "tr" to its category
# we create a new df containing both the transactional information and the items' category
tr_cat <- tr
summary(tr_cat) # exploration of tr_cat
tr_cat@itemInfo # item info in tr_cat is changed to the categories specified in "cat"


# order and set category labels to each product 
cat <- cat[order(cat$labels),]
cat$labels <- as.character(cat$labels)
itemInfo(tr_cat) <- cat # include the cat label in the new data set 



#################################### FREQUENCY ANALYSIS ####################################

# STUDY FREQUENCY OF APPEARENCE OF EACH CATEGORY IN THE DATA -> create a loop 
item.freq <- itemFrequency(tr_cat, type = 'absolute') # individual item frequency # example: item.freq['Roku Express'] = 41
cat.freq <- c() # create an empty vector to store frequency of transactions per category

for (c in unique(cat$level1)){                  # knowing the frequency of each category
  temp <- subset(cat, subset = level1 == c)     # will allow us to identify the most powerful categories
  temp$num <- item.freq[temp$labels]            # in Electronidex; and see if their top categories would complement
  cat.sum <- sum(temp$num)                      # Blackwell's product portfolio
  cat.freq[[c]] <- cat.sum 
}


# Insert the results of category frequency in a df (visualization purposes)
tr_catfreq_df <- as.data.frame(cat.freq)
tr_catfreq <- t(tr_catfreq_df)
colnames(tr_catfreq_df)[1] <- "Frequency"
tr_catfreq_df$Category <- as.vector(rownames(tr_catfreq_df))

# plot to see the items that most appear in the transactional records
# these will inform us of the categories most sold by Electronidex
# and help us understand if our product portfolio is complementary
# to theirs
ggplot(data = tr_catfreq_df) +                          # both product portfolios are complementary
  aes(x = Category, weight = Frequency) +               # as they excell in the areas Blackwell 
  geom_bar(fill = "#0c4c8a") +                          # has less sales (high-end categories such as
  theme_minimal()+                                      # desktops, monitors & laptops
  ggtitle("Frequency of category appearing in different transactions")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#### apriori ####
rules <- apriori(tr_cat, 
                 parameter = list(supp = 0.006, conf = 0.6,
                 minlen = 2))  # Avoid rules with one empty side)
sort(rules, by='confidence', decreasing = TRUE)



#### inspect rules ####
summary(rules)
inspect(rules)
is.redundant(rules)

itemFrequencyPlot(tr,           # histogram to detect transactions frequency
                  topN = 20,
                  type = 'absolute')
ruleExplorer(rules)





## B2B B2C
size(tr_cat) # check number of items per transaction