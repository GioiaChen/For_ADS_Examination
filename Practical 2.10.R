# compute features based on row & column averages (it takes atleast several minutes to run, up to an hour with slower computers):

pixels_gathered <- mnist_raw %>%
  head(1000) %>%
  rename(label = V1) %>%
  mutate(instance = row_number()) %>%
  gather(pixel, value, -label, -instance) %>%
  tidyr::extract(pixel, "pixel", "(\\d+)", convert = TRUE) %>%
  mutate(pixel = pixel - 2,
         x = pixel %% 28,
         y = 28 - pixel %/% 28)

features=data.frame(label=mnist_raw$V1[1:1000])
# set labels (of 1000 examples) as the first column in features dataframe
for (i in 1:56)
  features=cbind(features,fi=c(1:1000)*0)
  # create 56 features (28 for rows and 28 for column) for the 1000 examples
for (i in 1:28) {
  # loop over 28 rows and 28 columns 
  for (j in 1:1000) {
    # compute row & column means for each digit example using pixels gathered
    features[j,i+1]= mean(pixels_gathered$value[pixels_gathered$instance==j&pixels_gathered$y==i]);
    # first 28 features: row means (each row has fixed y)
    features[j,i+29] = mean(pixels_gathered$value[pixels_gathered$instance==j&pixels_gathered$x==i-1]);
    # next 28 features: column means (each row has fixed x)
  }
}

# compute the mean of the value of each column for each label
colnames(features) <- c("label", c(1:56))
label_matrices <- data.frame(matrix(nrow = 56, ncol = 10))
for (i in 1:10) {
  feature <- features[features$label==i-1, -1]
  mean_matrix <- data.frame(matrix(colMeans(feature),ncol = 56))
  colnames(mean_matrix) <- c(1:56)
  label_matrix <- gather(mean_matrix, key = "Feature", value = "Value")
  label_matrices[,i] <- label_matrix[,2]
}

# reshape of the result dataset
feature_number <- 1:nrow(label_matrices)
label_matrices <- cbind(feature_number, label_matrices)
colnames(label_matrices) <- c("label", c(0:9))
labels <- gather(label_matrices, key="label", value = "fi")
index <- data.frame(matrix(replicate(10, 1:56), nrow = 560, ncol = 1))
labels <- cbind(index, labels)
colnames(labels) <- c("index", "label", "fi")

# plot
library(ggplot2)
ggplot(data = labels, aes(x = index, y = fi, color = label, group = label)) +
  geom_line()


# normalization of the mean data
library(nnet)
norm_features <- features
norm_features[2:57] <- norm_features[2:57]/255


# pick out the train dataset for model training, the rest as valid dataset for the validation of the built model.
rows <- sample(1:1000, 700)
train_labels <- norm_features[rows, 1]
valid_labels <- norm_features[-rows, 1]
train_data <- norm_features[rows, -1]
valid_data <- norm_features[-rows, -1]
train_labels_matrix = class.ind(train_labels)
nn = nnet(train_data, train_labels_matrix, size = 4, softmax = TRUE)
pred_train = predict(nn, train_data, type="class")
pred_valid = predict(nn, valid_data, type="class")
mean(pred_train == train_labels)
mean(pred_valid == valid_labels)


