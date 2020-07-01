install.packages("readr")
install.packages("arules")
install.packages("arulesViz")

library(readr)
library(arules)
library(arulesViz)

book <- read.csv("C:\\Users\\91755\\Desktop\\Assignment\\0 - Ass Rules\\book.csv")
View(book)
head(book)
class(book)
book_trans <- as(as.matrix(book), "transactions")

#EDA
summary(book_trans)
inspect(book_trans[1:100]) #inspect each row and observe what kind of product purchased

#Graphical Representation
frequentItem <- eclat(book_trans, parameter = list(support = 0.07, maxlen=15))
inspect(frequentItem)
itemFrequencyPlot(book_trans, topN=10, type="absolute") #Plotting the top 10 freequent books

#Model Building
rule1 <- apriori(book_trans) #Default mode
summary(rule1)
#In default mode no rules is observed
#Now setting parameters and find the rules

rule2 <- apriori(book_trans, parameter = list(support = 0.01, confidence = 0.5, minlen = 2))
summary(rule2)
inspect(rule2[1:10])
#we get more rules if we decrease the confidence level

#Visualization
plot(rule2, method = "scatterplot")
plot(rule2, method = "grouped")
plot(rule2, method = "graph")

#change the support and confiedence value to reduce the rule length
rule3 <- apriori(book_trans, parameter = list(support=0.1, confidence=0.05, maxlen=10))
inspect(rule3)
rule_conf <- sort(rule3, by="confidence", decreasing = TRUE)
rule_lift <- sort(rule3, by="lift", decreasing = TRUE)
inspect(rule_conf)
inspect(rule_lift)
plot(rule3, method = "scatterplot")
plot(rule3, method = "grouped")
plot(rule3, method = "graph")
plot(rule3, method="graph", control = list(reorder=TRUE))

#change the support and confidence value 
rule4 <- apriori(book_trans, parameter = list(support=0.1, confidence=0.7, maxlen=10),
                 appearance = list(default="lhs", rhs="CookBks"))
inspect(rule4)
plot(rule4, method = "grouped")
plot(rule4, method = "graph")
plot(rule4, method = "scatterplot", control = list(type="items"))

#Removing Redundant Rules
remove_rule <- which(colSums(is.subset(rule4, rule4))>1)
length(remove_rule)
rule4 <- rule4[-remove_rule]
inspect(rule4)

