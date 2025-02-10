# Install packages/load libraries
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("patchwork")
install.packages("ggcorrplot")
install.packages("DMwR2")

library("dplyr")
library("ggplot2")
library("gridExtra")
library("patchwork")
library("ggcorrplot")
library("DMwR2")

# Load the data
dataset = read.csv("E:/cheryl/hku/sem1/FITE 7410/Assignment1/A1_data.csv")

# 1. Exploratory Data Analysis
# desciptive summary
summary(dataset)
str(dataset)
var_types <- sapply(dataset, class)
type_count <- table(var_types)
print(type_count)

# calculate percentage of fraud and non fraud case
table(dataset$isFraud)
table(dataset$isFraud)/length(dataset$isFraud) 

#2.  Univariate Analysis
# histogram
#isFraud
hist(dataset$isFraud, main = "isFraud Distribution", xlab = "isFraud",ylab = "Frequency", 
      col = "skyblue", border = "black",breaks = c(-0.3, 0.2, 0.5,1), xaxt = "n") 
axis(1, at = c(-0.05, 0.75), labels = c("0 Non-Fraud", "1 Fraud")) 
#card1
hist(dataset$card1, main = "card1 Distribution", xlab = "card1",ylab = "Frequency", 
     col = "skyblue", border = "black",breaks = 30) 
#TransactionDT
hist(dataset$TransactionDT, main = "TransactionDT Distribution", xlab = "TransactionDT",
     ylab = "Frequency", col = "skyblue", border = "black",breaks = 30) 
#TransactionAmt
hist(dataset$TransactionAmt, main = "TransactionAmt Distribution", xlab = "TransactionAmt",
     ylab = "Frequency", col = "skyblue", border = "black",breaks = 30) 

boxplot(dataset$TransactionAmt, main = "TransactionAmt Distribution", xlab = "TransactionAmt",
     ylab = "Transaction Amount", col = "skyblue", border = "black", outline = FALSE) 
Q1_value <- quantile(dataset$TransactionAmt, 0.25)
Q3_value <- quantile(dataset$TransactionAmt, 0.75)
median_value <- median(dataset$TransactionAmt)
text(1.3, Q1_value, labels = paste("Q1:", round(Q1_value, 2)), pos = 4, col = "black")
text(1.3, Q3_value, labels = paste("Q3:", round(Q3_value, 2)), pos = 4, col = "black")
text(1.3, median_value, labels = paste("Median:", round(median_value, 2)), pos = 4, col = "black")

# 3. Bi-/Multi-variate Analysis
# correlation matrix
num_vars <- sapply(dataset, is.numeric) # get the numeric variables
numeric_columns <- names(dataset)[num_vars]
sds <- sapply(dataset[, numeric_columns], sd, na.rm = TRUE)
indices <- which(sds == 0)
numeric_columns <- numeric_columns[-indices]
correlation_matrix <- cor(dataset[, numeric_columns], use = "pairwise.complete.obs")

# delete features with 0 correlation coefficient
zero_sd_columns <- sapply(dataset[, numeric_columns], function(x) sd(x, na.rm = TRUE) == 0)
dataset <- dataset[, !zero_sd_columns]
correlation_matrix <- cor(dataset[, numeric_columns[!zero_sd_columns]], use = "pairwise.complete.obs")

c = ggcorrplot(correlation_matrix, method = "circle", type = "lower", 
               lab = TRUE, lab_size = 3, title = "Correlation Matrix", 
               colors = c("blue", "white", "red"), 
               ggtheme = theme_minimal())
output_dir <- "E:/cheryl/hku/sem1/FITE 7410/mini case/pic"
ggsave(filename = file.path(output_dir, paste0("correlation_of_", col, ".png")), plot = c, width = 20, height = 16, bg = "white")

# 4. Data Cleaning
# 4.1 missing data
# delete features with too much missing data
missing_percentage <- colSums(is.na(dataset)) / nrow(dataset) * 100
columns_to_remove <- names(missing_percentage[missing_percentage > 70])
dataset <- dataset %>%
  dplyr::select(-all_of(columns_to_remove))
columns_to_remove #show the delete features

# 4.2 delete features with high correlation coefficient
# get the cor > 0.8 features
strong_correlations <- which(abs(correlation_matrix) > 0.8, arr.ind = TRUE)

strong_pairs <- data.frame(
  Feature1 = character(0),
  Feature2 = character(0),
  Correlation = numeric(0)
)

for (i in 1:nrow(strong_correlations)) {
  var1 <- rownames(correlation_matrix)[strong_correlations[i, 1]]
  var2 <- colnames(correlation_matrix)[strong_correlations[i, 2]]
  
  if (var1 != var2) {  # except feature itself
    correlation_value <- correlation_matrix[strong_correlations[i, 1], strong_correlations[i, 2]]
    
    strong_pairs <- rbind(strong_pairs, data.frame(Feature1 = var1, Feature2 = var2, Correlation = correlation_value))
  }
}
# delete the same same pairs
strong_pairs <- strong_pairs %>%
  group_by(Feature1) %>%
  summarise(Feature2 = first(Feature2), Correlation = first(Correlation))

print(strong_pairs)

# select 1 or 2 features to replace strong pairs  ----new data
data_removed <- dataset %>%
  select(-C10, -C11, -C12, -C13, -C14, -C2, -C4, -C6, -C7, -C8,
         -D4, -D8, -D10, -D12, -D13, -D14, -D6, -TransactionDT..Hour., 
         -V310, -V312, -V313) %>%
  select(where(~n_distinct(.) > 1))

# new correlation plot
num_vars_n <- sapply(data_newremoved, is.numeric)
numeric_columns_n <- names(data_newremoved)[num_vars_n]

correlation_matrix_new <- cor(data_newremoved[, numeric_columns_n], use = "pairwise.complete.obs")
cnew = ggcorrplot(correlation_matrix_new, method = "circle", type = "lower", 
                  lab = TRUE, lab_size = 3, title = "Correlation Matrix", 
                  colors = c("blue", "white", "red"), 
                  ggtheme = theme_minimal())

ggsave(filename = file.path(output_dir, paste0("correlation_of_", col, ".png")), plot = cnew, width = 20, height = 16, bg = "white")

#4.3 Deletion
dataset <- dataset %>% select(-"DeviceInfo", -"id_33", -"id_31",) #delete varibles with too many types
dataset <- dataset %>% select(-"id_16", -"id_23", -"id_27", -"id_28", 
                              "ProductCD", -"card4",-"P_emaildomain", -"id_12",-"id_15")#delete meaningless data
categorical_columns <- sapply(dataset, is.factor) | sapply(dataset, is.character) | sapply(dataset, is.logical)
dataset_categorical <- dataset[categorical_columns]
dataset_categorical[] <- lapply(dataset_categorical, function(x) {
  if (is.logical(x)) {
    # logical -- 0&1
    as.integer(x)
  } else {
    # categorical -- factor
    as.integer(as.factor(x))
  }
})

# Fisher
important_vars <- sapply(dataset[categorical_columns], function(x) {
  if (length(unique(x)) <= 2) {
    fisher.test(table(x, dataset$isFraud))$p.value
  } else {
    chisq.test(table(x, dataset$isFraud))$p.value
  }
})
significant_vars <- names(important_vars[important_vars < 0.01])

dataset_categorical <- dataset_categorical[ , c(significant_vars)]

#numeric
#delete meaningless
dataset <- dataset %>% select(-"addr2", -"id_04", -"id_05",-"id_06", -"id_09", -"id_10", -"id_11", -"id_13", -"id_14"
                              ,-"id_19", -"id_20")
numeric_data <- dataset[, sapply(dataset, is.numeric)]
new_dataset <- cbind(numeric_data, dataset_categorical)


# 4.4 fill in the missing number with mean
data_newremoved <- data_newremoved %>%
  mutate(across(all_of(numeric_columns_n), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
#colSums(is.na(data_newremoved))
#head(data_newremoved)

# 4.5 fill in the missing character
# get mode - empty
char_vars <- names(data_newremoved)[sapply(data_newremoved, is.character)]  # get character variables

get_mode <- function(x) {
  uniq_x <- unique(x[!is.na(x) & x != ""])
  if (length(uniq_x) == 0) return(NA)
  uniq_x[which.max(tabulate(match(x, uniq_x)))] 
}

# relace empty with mode
fill_with_mode_or_default <- function(x, default_value = "default_fill") {
  mode_value <- get_mode(x) 
  mode_value <- ifelse(is.na(mode_value), default_value, mode_value) 
  replace(x, is.na(x) | x == "", mode_value)  
}

data_filled <- data_newremoved %>%
  mutate(across(all_of(char_vars), ~fill_with_mode_or_default(., "your_desired_default")))

# replace NA with mode
handle_na_with_mode <- function(x) {
  mode_value <- get_mode(x) 
  ifelse(is.na(x), mode_value, x) 
}
NA_vars <- c("id_35", "id_36", "id_37","id_38")
data_filled <- data_filled %>%
  mutate(across(all_of(NA_vars), handle_na_with_mode))

# 4.6 outlier 
excluded_columns <- c("TransactionID", "isFraud","TransactionDT", "TransactionAmt", "card1", "card2",
                      "card3", "D1", "V314", "id_11", "id_18","C3","V311")

numeric_newcolumns <- names(data_filled)[sapply(data_filled, is.numeric)]
columns_to_process <- setdiff(numeric_newcolumns, excluded_columns)

summary(data_filled)

data_filled <- data_filled %>%
  mutate(across(all_of(columns_to_process), ~ {
    q25 <- quantile(., 0.25, na.rm = TRUE)  
    q75 <- quantile(., 0.75, na.rm = TRUE) 
    
    ifelse(. < q25, q25, ifelse(. > q75, q75, .))
  }))

 #4.7 Balance Data
install.packages("smotefamily")
library(smotefamily)

dataset_balanced <- SMOTE(new_dataset[, -which(names(new_dataset) == "isFraud")], 
                          new_dataset$isFraud,K = 5)  
colnames(dataset_balanced$data)[colnames(dataset_balanced$data) == "class"] <- "isFraud"

#5. Model

#####################
####Random Forest####
#####################
library(randomForest)
library(caret)

dataset_balanced = dataset_balanced %>% mutate_if(is.character,as.factor)
dataset_rf = data.frame(dataset_balanced$data)
dataset_rf$isFraud <- factor(dataset_rf$isFraud)
train = sample(nrow(dataset_rf), 0.7*nrow(dataset_rf), replace = FALSE)
TrainSet = dataset_rf[train,]
TestSet = dataset_rf[-train,]

train_control = trainControl(method = "cv", number = 5)
model_rf = train(isFraud ~ ., data = TrainSet, method = "rf", trControl = train_control,
                 tuneGrid = expand.grid(mtry = 15), ntree = 500)
model_rf$bestTune
model_rf

#Accuracy, recall, fi-score
prediction_rf = predict(model_rf,TestSet)
conf_matrix <- confusionMatrix(prediction_rf,TestSet$isFraud)
accuracy <- conf_matrix$overall['Accuracy']    # accuracy
recall <- conf_matrix$byClass['Recall']        # recall
precision <- conf_matrix$byClass['Precision']  # Precision
f1_score <- 2 * ((recall * precision) / (recall + precision))  # F1 score

# result
print(paste("Accuracy:", accuracy))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))

#ROC
install.packages("pROC")
library(pROC)

TestSet$isFraud <- as.numeric(TestSet$isFraud) - 1
probability_rf <- predict(model_rf, TestSet, type = "prob")[, 2] 
roc_curve <- roc(TestSet$isFraud, probability_rf)

plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
auc_value_rf <- auc(roc_curve)
print(paste("AUC:", auc_value_rf))

# Feature importance
importance_rf <- importance(model_rf)
print(importance_rf)
sorted_importance <- importance_rf[order(-importance_rf[, 1]), ]
barplot(importance_rf[, 1], main="Feature Importance (Random Forest)", 
        col="lightblue", las=2, cex.names=0.7)
barplot(sorted_importance[, 1], 
        main="Feature Importance (Random Forest)", 
        col="lightblue", 
        las=2, 
        cex.names=0.7, 
        names.arg = rownames(sorted_importance))


#####################
####Neural Network###
#####################

# Calculate the error and find a better model
set.seed(2023)
err11=0
err12=0
n_tr=dim(TrainSet)[1]
n_te=dim(TestSet)[1]

for(i in seq(1, 801, 100))
{
  model=nnet(isFraud ~ ., data=TrainSet,maxit=i,size=20,decay = 0.001)
  err11[i]=sum(predict(model,TrainSet,type='class')!=TrainSet[,35])/n_tr
  err12[i]=sum(predict(model,TestSet,type='class')!=TestSet[,35])/n_te
}

error_1 = na.omit(err11)
error_2 = na.omit(err12)
plot(seq(1, length(error_1)), error_1, col=1, type="b", 
     ylab="Error rate", xlab="Training epoch", 
     ylim=c(min(min(error_1), min(error_2)), max(max(error_1), max(error_2))))

#plot(seq(1, 601, 100),error_1,col=1,type="b",ylab="Error rate",xlab="Training epoch",ylim=c(min(min(error_1),min(error_2)),max(max(error_1),max(error_2))))
#lines(seq(1, 601, 100),error_2,col=2,type="b")
lines(seq(1, length(error_2)),error_2,col=2,type="b")
legend("topleft",pch=c(15,15),legend=c("Train","Test"),col=c(1,2),bty="n")


# Final model and evaluation result
model_best=nnet(isFraud ~ ., data=TrainSet,maxit=500,size=6,decay = 0.1)

#ROC
prediction_nn <- predict(model_best, TestSet, type = "raw")
probability_nn <- as.vector(1 - prediction_nn)

# 计算ROC曲线
roc_curve_nn <- roc(TestSet$isFraud, probability_nn)

# 绘制ROC曲线
plot(roc_curve_nn, main = "ROC Curve for Neural Network", col = "red", lwd = 2)

prediction_test = predict(model_best,TestSet,type="class")
table = table(TestSet$isFraud,prediction_test)
conf_matrix_cnn <- confusionMatrix(table)
conf_matrix_cnn
#AUC, accuracy, recall, F1-score
auc_value <- auc(roc_curve_nn)
print(paste("AUC:", auc_value))
accuracy <- conf_matrix_cnn$overall['Accuracy']    # accuracy
recall <- conf_matrix_cnn$byClass['Recall']        # recall
precision <- conf_matrix_cnn$byClass['Precision']  # Precision
f1_score <- 2 * ((recall * precision) / (recall + precision))  # F1 score

# result
print(paste("Accuracy:", accuracy))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))
