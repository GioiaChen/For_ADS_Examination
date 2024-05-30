mnist_raw <- read.csv("D:/ADS/mnist_train.csv", header = FALSE)

# Preprocessing
library(readr)
library(dplyr)

mnist_raw[1:10, 1:10]

# We'll gather the data, do some arithmetic to keep track of x and y within an image,
# and keep only the first 10,000 training instances.
library(tidyr)

pixels_gathered <-  mnist_raw%>%
  head(10000) %>%
  rename(label = V1) %>%
  mutate(instance = row_number()) %>%
  gather(pixel, value, -label, -instance) %>%
  tidyr::extract(pixel, "pixel", "(\\d+)", convert = TRUE) %>%
  mutate(pixel = pixel - 2,
         x = pixel %% 28,
         y = 28 - pixel %/% 28)

pixels_gathered

# We now have one row for each pixel in each image.
# This is a useful format because it lets us visualize the data along the way. 
# For example, we can visualize the first 12 instances with a couple lines of ggplot2.
library(ggplot2)
theme_set(theme_light())

pixels_gathered %>%
  filter(instance <= 12) %>%
  ggplot(aes(x, y, fill = value)) +
  geom_tile() +
  facet_wrap(~ instance + label)

# Exploring pixel data
ggplot(pixels_gathered, aes(value)) +
  geom_histogram()

pixel_summary <- pixels_gathered %>%
  group_by(x, y, label) %>%
  summarize(mean_value = mean(value)) %>%
  ungroup()

pixel_summary

pixel_summary %>%
  ggplot(aes(x, y, fill = mean_value)) +
  geom_tile() +
  scale_fill_gradient2(low ="white", high = "black", mid = "gray", midpoint = 127.5) +
  facet_wrap(~ label, nrow = 2) +
  labs(title = "Average value of each pixel in 10 MNIST digits",
       fill = "Average value") +
  theme_void()


pixels_joined <- pixels_gathered %>%
  inner_join(pixel_summary, by = c("label", "x", "y"))

image_distances <- pixels_joined %>%
  group_by(label, instance) %>%
  summarize(euclidean_distance = sqrt(mean((value - mean_value) ^ 2)))

image_distances

ggplot(image_distances, aes(factor(label), euclidean_distance)) +
  geom_boxplot() +
  labs(x = "Digit",
       y = "Euclidean distance to the digit centroid")

worst_instances <- image_distances %>%
  top_n(6, euclidean_distance) %>%
  mutate(number = rank(-euclidean_distance))

pixels_gathered %>%
  inner_join(worst_instances, by = c("label", "instance")) %>%
  ggplot(aes(x, y, fill = value)) +
  geom_tile(show.legend = FALSE) +
  scale_fill_gradient2(low = "white", high = "black", mid = "gray", midpoint = 127.5) +
  facet_grid(label ~ number) +
  labs(title = "Least typical digits",
       subtitle = "The 6 digits within each label that had the greatest distance to the centroid") +
  theme_void() +
  theme(strip.text = element_blank())

