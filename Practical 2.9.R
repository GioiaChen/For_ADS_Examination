# Get and import the data set(s)
tiny_mnist <- read.csv("D:/ADS/really_tiny_dataset.csv", header = FALSE)
mnist_raw <- read.csv("D:/ADS/mnist_train.csv", header = FALSE)
library(tidyr)
library(dplyr)
library(ggplot2)

# Reshaping the dataset
pixels_gathered_tiny <-  tiny_mnist%>%
  rename(label = V1) %>%
  mutate(instance = row_number()) %>%
  gather(pixel, value, -label, -instance) %>%
  tidyr::extract(pixel, "pixel", "(\\d+)", convert = TRUE) %>%
  mutate(pixel = pixel - 2,
         x = pixel %% 28,
         y = 28 - pixel %/% 28)

# Plot the data
pixels_gathered_tiny %>%
  ggplot(aes(x, y, fill = value)) +
  geom_tile() +
  facet_wrap(~ instance + label)

# What does the “average digit” look like?
aggregation <- aggregate(value~pixel+label+x+y, data=pixels_gathered_tiny, FUN=mean)
aggregation %>%
  ggplot(aes(x, y, fill = value)) +
  geom_tile() +
  facet_wrap(~ label)

# Move to the big dataset
# Reshaping the dataset
pixels_gathered_big <- mnist_raw %>%
  rename(labels = V1) %>%
  mutate(instance = row_number()) %>%
  gather(pixel, value, -labels, -instance) %>%
  tidyr::extract(pixel, "pixel", "(\\d+)", convert = TRUE) %>%
  mutate(pixel = pixel - 2,
         x = pixel %% 28,
         y = 28 - pixel %/% 28)
# Plotting the head 20 samples
pixels_gathered_big %>%
  filter(instance<=20) %>%
  ggplot(aes(x, y, fill = value)) +
  geom_tile() +
  facet_wrap(~ instance + labels)
# Plotting the average of the top50 samples
aggregation_big <- aggregate(value~pixel+labels+x+y, data=pixels_gathered_big, FUN=mean)
aggregation_big %>%
  ggplot(aes(x, y, fill = value)) +
  geom_tile() +
  facet_wrap(~ labels)

# Feature selection
zeros <- mnist_raw[mnist_raw$V1==0,]




