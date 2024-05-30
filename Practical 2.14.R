library(ggplot2)
library(tidyverse)

data <- read.table("Reporter_assay_4-1-15.txt", header = TRUE)
summary(data)

## Plot the data and examine it
data$Category <- as.factor(data$Category)
g1 <- ggplot(data=data, mapping=aes(x=Category, y=ave, color=Category))
g1 + geom_boxplot()

## Generate the first bootstrap sample
ea <- data[data$Epigenetic_status=="Active",]
er <- data[data$Epigenetic_status=="Repressed",]
real_median <- median(ea$ave)-median(er$ave)
bootstrapping_enhancer <- function(size) {
  sample_ea <- data[sample(nrow(data), size, replace=TRUE),]
  median_active <- median(sample_ea$ave)
  sample_er <- data[sample(nrow(data), size, replace=TRUE),]
  median_repressed <- median(sample_er$ave)
  dif <- median_active-median_repressed
  return(dif)
}
median1 <- bootstrapping(32)

## Generate a large number of bootstraps
medians <- numeric(10000)
for (i in 1:10000) {
  medians[i] <- bootstrapping(32)
}
hist(medians)
p_value <- length(medians[medians>real_median])/length(medians)


# Which movie genres are most popular with Chinese university students?
## Again, plot the data and examine it first.
movie <- read.table("movie_data.txt", header=TRUE)
movie$genre <- as.factor(movie$genre)
g2 <- ggplot(data=movie, mapping=aes(x=genre, y=students, color=genre))
g2 + geom_bar(stat='identity', fill="white")

## Generate a first bootstrap sample.
students_comedy <- c(replicate(73, "comedy"), replicate(267-73,"others"))

bootstrapping_comedy <- function(dataset) {
  sample_comedy <- dataset[sample(267, 267, replace=TRUE)]
  comedy <- str_count(sample_comedy,"comedy")
  comedy <- sum(comedy)
  return(comedy)
}

bootstrapping_comedy(students_comedy)

## Generate a confidence interval by bootstrapping many times
comedies <- numeric(10000)
for (i in 1:10000) {
  comedies[i] <- bootstrapping_comedy(students_comedy)
}
hist(comedies)



