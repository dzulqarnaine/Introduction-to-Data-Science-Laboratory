# Impor library yang diperlukan
library(readr)
library(dplyr)
library(caret)
library(class)
library(ggplot2)
library(e1071)

Zulkarnaini <- read.csv("Data - diabetes.csv", sep = ";")
head(Zulkarnaini)

# ZULKARNAINI
summary(Zulkarnaini)

str(Zulkarnaini)

target_name <- "Outcome"
y <- Zulkarnaini[[target_name]]
x <- Zulkarnaini %>% select(-Outcome)

# Split Data
set.seed(42)
n <- nrow(x)
index <- sample(1:n, size = 0.8 * n)
X_train <- x[index, ]
X_test <- x[-index, ]
y_train <- y[index]
y_test <- y[-index]


# Standardisasi data
sc <- preProcess(X_train, method = c("center", "scale"))
X_train <- predict(sc, X_train)
X_test <- predict(sc, X_test)
head(X_train)
head(X_test)

# k yang ganjil saja
k_list <- seq(3, 19, by = 2)

print(k_list)

for (k in k_list) {
  y_pred <- knn(train = X_train, test = X_test, cl = y_train, k = k)
  accuracy <- mean(y_pred == y_test)
  cat(sprintf("K = %d, Akurasi = %.4f\n", k, accuracy))
}

# ZULKARNAINI
# Inisialisasi model KNN dengan k = 5
KNN_Zulkarnaini_k <- 5

# Train model KNN
y_pred <- knn(train = X_train, test = X_test, cl = y_train, k = k, use.all = FALSE)


# ZULKARNAINI
akurasi <- mean(y_pred == y_test)
cat(sprintf("Tingkat Akurasi : %.2f persen\n", akurasi * 100))

# Classification report
library(caret)
akurasi_report <- confusionMatrix(y_pred, as.factor(y_test))
print(akurasi_report)

# Confusion Matrix 1
cm <- table(Actual = y_test, Predicted = y_pred)

fourfoldplot(cm, color = c("#FBB4AE", "#B3CDE3"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")


# Confusion Matrix 2
library(ggplot2)
cm_df <- as.data.frame(cm)

ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  ggtitle("Confusion Matrix") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13)
  )




################################### Logistik Regression ####################################
library(caret)
library(ggplot2)

LR_Zulkarnaini <- glm(Outcome ~ ., data = data.frame(X_train, Outcome = y_train),
                      family = binomial)

prob_lr <- predict(LR_Zulkarnaini, newdata = X_test, type = "response")
y_pred_lr <- ifelse(prob_lr > 0.5, 1, 0)
y_pred_lr <- as.factor(y_pred_lr)

akurasi_lr <- mean(y_pred_lr == y_test)
cat(sprintf("Tingkat Akurasi : %.2f persen\n", akurasi_lr * 100))

cm_lr <- confusionMatrix(y_pred_lr, as.factor(y_test))
print(cm_lr)

cm_table_lr <- table(Actual = y_test, Predicted = y_pred_lr)

fourfoldplot(cm_table_lr,
             color = c("#FBB4AE", "#B3CDE3"),
             conf.level = 0,
             margin = 1,
             main = "Confusion Matrix Logistic Regression")

cm_df_lr <- as.data.frame(cm_table_lr)

ggplot(cm_df_lr, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  ggtitle("Confusion Matrix Logistic Regression") +
  theme_minimal()



#################################### Random Forest (RF) #####################################
library(randomForest)
library(caret)
library(ggplot2)

RF_Zulkarnaini <- randomForest(x = X_train,
                               y = as.factor(y_train),
                               ntree = 100,
                               importance = TRUE)

y_pred_rf <- predict(RF_Zulkarnaini, X_test)

akurasi_rf <- mean(y_pred_rf == y_test)
cat(sprintf("Tingkat Akurasi : %.2f persen\n", akurasi_rf * 100))

cm_rf <- confusionMatrix(y_pred_rf, as.factor(y_test))
print(cm_rf)

cm_table_rf <- table(Actual = y_test, Predicted = y_pred_rf)

fourfoldplot(cm_table_rf,
             color = c("#FBB4AE", "#B3CDE3"),
             conf.level = 0,
             margin = 1,
             main = "Confusion Matrix Random Forest")

cm_df_rf <- as.data.frame(cm_table_rf)

ggplot(cm_df_rf, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  ggtitle("Confusion Matrix Random Forest") +
  theme_minimal()
