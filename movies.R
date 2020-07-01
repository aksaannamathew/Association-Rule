install.packages("readr")
install.packages("arules")
install.packages("arulesViz")
library(readr)
library(arules)
library(arulesViz)

my_movies <- read.csv("C:\\Users\\91755\\Desktop\\Assignment\\0 - Ass Rules\\my_movies.csv")
View(my_movies)
my_movies <- my_movies[6:15]
View(my_movies)

class(my_movies)

my_movies <- as(as.matrix(my_movies), "transactions")

#EDA
summary(my_movies)
inspect(my_movies[1:10]) #Inspect the row to know about the purchase

#GRaphical Representation
itemFrequencyPlot(my_movies, topN=10, type="absolute") #Plotting the most freequent purchasing product

#Model Building
#Default mode
rule1 <- apriori(my_movies)
summary(rule1)
#there is no rule observed in the default mode
#Therefore setting parameters and finding the rules
rule2 <- apriori(my_movies, parameter = list(support=0.01, confidence=0.5, maxlen=15))
inspect(rule2[1:10])

plot(rule2, method = "grouped")
plot(rule2, method = "graph")
plot(rule2, method = "graph", control = list(reorder=TRUE))

#Changing the support and confidence to reduce the rules
rule3 <- apriori(my_movies, parameter = list(support=0.2, confidence=0.5, maxlen=15, minlen=2))
inspect(rule3)
rule3_conf <-sort(rule3, by="confidence", decreasing = TRUE)
rule3_lift <- sort(rule3, by="list", decreasing = TRUE)
inspect(rule3_conf)
inspect(rule3_lift)

plot(rule3, method = "grouped")
plot(rule3, method = "graph")
plot(rule3, method = "graph", control = list(reorder=TRUE))

#Setting the RHS side
rule4 <- apriori(my_movies, parameter = list(support=0.2, confidence=0.7, maxlen=15),
                 appearance = list(default="lhs", rhs="Gladiator"))
inspect(rule4)

plot(rule4, method = "grouped")
plot(rule4, method = "graph")
plot(rule4, method = "graph", control = list(type="items"))

##removing redundant rules
sub_rules <- which(colSums(is.subset(rule1, rule1))>1)
length(sub_rules)
rule1 <- rule1[-sub_rules]
inspect(rule1)
