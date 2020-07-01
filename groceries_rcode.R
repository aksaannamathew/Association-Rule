install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

groceries <- read.transactions("C:\\Users\\91755\\Desktop\\Assignment\\0 - Ass Rules\\groceries.csv")
head(groceries)
View(groceries)

#EDA
summary(groceries)
inspect(groceries[1:10]) #inspecting each rows
class(groceries)

#Graphical Representation
freequent_item <- eclat(groceries, parameter = list(support=0.07, maxlen=15)) #Calculate support for freequency items
inspect(freequent_item)
itemFrequencyPlot(groceries, topN=10, type="absolute") #Plotting top 10 items

#Model Building
rule1 <- apriori(groceries) #Default mode
summary(rule1)
#No rules find in the default mode
#Therefore setting parameter values and fing the rules and plotting it

rule2 <- apriori(groceries, parameter = list(support=0.01, confidence=0.05, maxlen=15))
summary(rule2)
inspect(rule2)

plot(rule2, method = "grouped")
plot(rule2, method = "graph")
plot(rule2, method = "graph", control = list(reorder=TRUE))

#Changing the parameters
rule3 <- apriori(groceries, parameter = list(support=0.01, confidence=0.15, maxlen=10))
inspect(rule3)
rule3_conf <- sort(rule3, by="confidence", decreasing = TRUE)
rule3_lift <- sort(rule3, by="lift", decreasing = TRUE)
plot(rule3, method = "grouped")
plot(rule3, method = "graph")
plot(rule3, method = "graph", control = list(reorder=TRUE))

#Setting the RHS side
rule4 <- apriori(groceries, parameter = list(support=0.005, confidence=0.05, maxlen=6),
                 appearance = list(default="lhs", rhs="bags"))
inspect(rule4)

plot(rule4, method = "grouped")
plot(rule4, method = "graph")
plot(rule4, method = "grapg", control = list(type="items"))

#Removing Redundant Rules
subrules <- which(colSums(is.subset(rule3, rule3))>1)
length(subrules)
rule3 <- rule3[-(subrules)]
inspect(rule3)
