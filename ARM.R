library(arules)
setwd("~/MSDS ML Project")
transactions <- read.transactions("transactions.csv", format = "basket", rm.duplicates = FALSE, cols = NULL, sep = ",")

rules <- arules::apriori(transactions, parameter = list(support = 0.045, confidence = 0.05, minlen = 2))
inspect(rules)

## Rules for confidence
SortRules_Conf <- sort(rules, by = 'confidence', decreasing = TRUE)
inspect(SortRules_Conf[1:15])

## Rules for lift
SortRules_Lift <- sort(rules, by = 'lift', decreasing = TRUE)
inspect(SortRules_Lift[1:15])

## Rules for support
SortRules_Sup <- sort(rules, by = 'support', decreasing = TRUE)
inspect(SortRules_Sup[1:15])

## Plot for confidence
library(arulesViz)
plot(SortRules_Conf, method="graph", engine="htmlwidget", limit = 15)

## Plot for lift
library(arulesViz)
plot(SortRules_Lift, method="graph", engine="htmlwidget", limit = 15)

## Plot for support
library(arulesViz)
plot(SortRules_Sup, method="graph", engine="htmlwidget", limit = 15)
