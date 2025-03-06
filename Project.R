#=========================== K-NN =============================
# Load dataset and normalize weight
data("PlantGrowth")
PlantGrowth$group <- factor(PlantGrowth$group, levels = c("ctrl", "trt1", "trt2"), labels = c("control", "treatment 1", "treatment 2"))
normalize <- function(x) { return((x - min(x)) / (max(x) - min(x))) }
data1 <- as.data.frame(lapply(PlantGrowth["weight"], normalize))

# Split data into training and testing sets
data_train <- data1[1:15, , drop = FALSE]
data_test <- data1[16:30, , drop = FALSE]
data_train_labels <- factor(PlantGrowth$group[1:15])
data_test_labels <- factor(PlantGrowth$group[16:30])

# KNN prediction
library(class)
data_test_pred <- knn(train = data_train, test = data_test, cl = data_train_labels, k = 3)

# Accuracy
conf_matrix <- table(Predicted = data_test_pred, Actual = data_test_labels)
accuracy_knn <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("K-NN Accuracy:", round(accuracy_knn * 100, 2), "%\n")

# Bar chart to compare predicted vs actual classes
library(ggplot2)
plot_data <- data.frame(
  Actual = data_test_labels,
  Predicted = data_test_pred
)

# Bar chart of Actual vs Predicted classes
ggplot(plot_data, aes(x = Actual, fill = Predicted)) +
  geom_bar(position = "dodge") +
  labs(title = "K-NN Prediction Comparison", x = "Actual Class", y = "Count") +
  scale_fill_manual(values = c("control" = "blue", "treatment 1" = "red", "treatment 2" = "green")) +
  theme_minimal()

# Scatter plot of Actual vs Predicted (Normalized Weight)
plot(plot_data$Actual, as.numeric(plot_data$Predicted), col = plot_data$Predicted, 
     pch = 16, xlab = "Actual Class", ylab = "Predicted Class",
     main = "K-NN Classification: Actual vs Predicted")
legend("topright", legend = levels(plot_data$Predicted), fill = 1:length(levels(plot_data$Predicted)))

#=========================== Decision Tree =============================

# Load and prepare data
library(rpart)
indexs <- sample(150, 110)
PlantGrowth_train <- PlantGrowth[indexs,]
PlantGrowth_test <- PlantGrowth[-indexs,]
target <- group ~ weight

# Train decision tree
tree <- rpart(target, data = PlantGrowth_train, method = "class")
library(rpart.plot)
rpart.plot(tree, main = "Decision Tree for PlantGrowth", extra = 1, box.palette = "Greens")

# Predictions and Accuracy
predictions <- predict(tree, PlantGrowth_test, type = "class")
conf_matrix <- table(Predicted = predictions, Actual = PlantGrowth_test$group)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Decision Tree Accuracy:", round(accuracy * 100, 2), "%\n")


#=========================== Hierarchical Clustering =============================

# Perform hierarchical clustering
library(cluster)
P_1 <- PlantGrowth[, "weight", drop = FALSE]
d <- dist(P_1, method = "euclidean")
hir <- hclust(d, method = "average")
grps <- cutree(hir, k = 3)

# Accuracy
cm_hier <- table(Actual = PlantGrowth$group, Cluster = grps)
hier_accuracy <- sum(apply(cm_hier, 1, max)) / sum(cm_hier)
cat("Hierarchical Clustering Accuracy:", round(hier_accuracy * 100, 2), "%\n")

# Dendrogram and Cluster Visualization
plot(hir, main = "Hierarchical Clustering Dendrogram")
rect.hclust(hir, k = 3, border = c("red", "blue", "green"))
plot(P_1$weight, col = grps, pch = 16, main = "Hierarchical Clustering", xlab = "Index", ylab = "Weight")
points(tapply(P_1$weight, grps, mean), col = 1:3, pch = 8, cex = 2, lwd = 2)


#=========================== K-means Clustering =============================

# Perform k-means clustering
set.seed(24)
kmeans.re <- kmeans(P_1, centers = 3, nstart = 2)
cm <- table(PlantGrowth$group, kmeans.re$cluster)
cluster_accuracy <- sum(apply(cm, 1, max)) / sum(cm)
cat("K-means Accuracy:", round(cluster_accuracy * 100, 2), "%\n")

# K-means Plot
plot(P_1$weight, col = kmeans.re$cluster, main = "K-means Clustering", xlab = "Index", ylab = "Weight", pch = 16)
points(kmeans.re$centers, col = 1:3, pch = 8, cex = 2)



# Load ggplot2 for visualization
library(ggplot2)

# Bar chart to compare accuracies
ggplot(accuracy_data, aes(x = Method, y = Accuracy, fill = Method)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(title = "Comparison of Accuracy for Different Methods", x = "Method", y = "Accuracy (%)") +
  scale_fill_manual(values = c("blue", "red", "green", "orange")) +
  theme_minimal() +
  geom_text(aes(label = round(Accuracy * 100, 2)), vjust = -0.3, size = 5)

