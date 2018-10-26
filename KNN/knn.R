example <- read.csv("KNN/example.csv")

library(ggplot2)
ggplot(example, aes(x = X, y = Y, color = Class)) + geom_point(size = 4)

example[12,] <- c(30,130000, NA)
ggplot(example, aes(x = X, y = Y, color = Class)) + geom_point(size = 4) +
  geom_label(label = row.names(example))


d <- as.matrix(dist(example[,1:2]))
sort(d[,12])

example


sc <- scale(example[,1:2])
sc <- as.data.frame(sc)
example[,1:2] = sc

d <- as.matrix(dist(example[,1:2]))
sort(d[,2])


ggplot(example, aes(x = X, y = Y, color = Class)) + geom_point(size = 4) +
  geom_label(label = row.names(example))


# Signs

signs <- read.csv("KNN/signs.csv")
sapply(signs, class)
  

