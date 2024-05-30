guests <- read.csv("guests.csv")
library(ggplot2)
g <- ggplot(data=guests, mapping=aes(x=age_norm,y=hours_norm))
g1 <- g + geom_point()
centroids <- guests[sample(1:20,4),]
g2 <- g1 + geom_point(data=centroids,
                    aes(x=age_norm, y=hours_norm), shape=3,
                    size=2, colour="red")
g2

guests$cluster <- numeric(20)
guests$dis1 <- numeric(20)
guests$dis2 <- numeric(20)
guests$dis3 <- numeric(20)
guests$dis4 <- numeric(20)

#for (i in 1:nrow(guests)) {
#  guests$dis1[i] <- sqrt((guests$age_norm[i]-centroids$age_norm[1])^2+(guests$hours_norm[i]-centroids$hours_norm[1])^2)
#}
for (i in 1:nrow(centroids)) {
  guests[,i+4] <- dist(rbind(centroids[i,2:3],guests[,2:3]))[1:20]
}
for (i in 1:nrow(guests)) {
  guests$cluster[i] <- which(guests[i,5:8]==min(guests[i,5:8]))
}
new_centroids <- data.frame("cluster"=c(1:4),
                            "age_norm"=numeric(4),
                            "hours_norm"=numeric(4))
for (i in 1:nrow(new_centroids)) {
  new_centroids[i,2] <- mean(guests[guests$cluster==i,2])
  new_centroids[i,3] <- mean(guests[guests$cluster==i,3])
}
g3 <- ggplot(data=guests, mapping=aes(x=age_norm,y=hours_norm,color=cluster)) + geom_point()
g3 + geom_point(data=new_centroids,
                aes(x=age_norm, y=hours_norm), shape=3,
                size=2, colour="red")

for (i in 1:4) {
  guests[,i+4] <- dist(rbind(new_centroids[i,2:3],guests[,2:3]))[1:20]
}
for (i in 1:nrow(guests)) {
  guests$cluster[i] <- which(guests[i,5:8]==min(guests[i,5:8]))
}
for (i in 1:4) {
  new_centroids[i,2] <- mean(guests[guests$cluster==i,2])
  new_centroids[i,3] <- mean(guests[guests$cluster==i,3])
}
guests$cluster <- as.character(guests$cluster)
g4 <- ggplot(data=guests, mapping=aes(x=age_norm,y=hours_norm,color=cluster)) + geom_point()
g4
g4 + geom_point(data=new_centroids,
                aes(x=age_norm, y=hours_norm), shape=3,
                size=2, colour="red")

# Hierarchical clustering
library(cluster)
dist <- dist(guests[,2:3], method="euclidean")
hc <- hclust(d=dist, method="centroid")
?hclust()
plot(hc, labels = guests$names, hang = 0.1, check = TRUE,
     axes = TRUE, frame.plot = FALSE, ann = TRUE,
     main = "Cluster Dendrogram",
     sub = NULL, xlab = NULL, ylab = "Height")
