faceplate <- read.csv("ARM/faceplate.csv")

library(arules)
library(arulesViz)

# take out id
faceplate1 <- as.matrix(faceplate[,-1])
txn <- as(faceplate1, "transactions")

inspect(txn)

#plots
itemFrequencyPlot(txn)

rules <- apriori(txn, parameter = list(minlen = 2, maxlen = 2,
                                       supp = 0.001, conf = 0.001))

inspect(rules)

rules_df <- as(rules, "data.frame")
rules_df[1,]

# support
table(faceplate$Orange, faceplate$Red)

1 / 10

# confidence

prop.table(table(faceplate$Orange, faceplate$Red),1)
# P(red orange) / p(orange)
0.5 * 0.5 / 0.5

# lift
table(faceplate$Red)
6 / 10

#pred
# lift / red
0.5 / 0.6

rules1 <- apriori(txn,
                  parameter = list(supp = 0.001, conf = 0.001))

rules_l <- sort(rules1, by = "lift")
inspect(rules_l)

rules_r <- subset(rules1, rhs %in% 'Red')
inspect(rules_r)


rules_3 <- subset(rules1, lhs %ain% c("Red", "White"))
inspect(rules_3)

arules::image(txn)

plot(rules, type = "scatterplot")

plot(rules1[10:30], method = "graph")

plot(rules1, type = "scatterplot", 
     measure = c("lift", "confidence"), shading = "support", jitter = 0)

# httr
# rJava
# curl
# iplots
# plotly
# arulesViz

inspect(Cplusplus)

