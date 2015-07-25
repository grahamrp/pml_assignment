library(caret)
library(dplyr)
library(readr)
library(beepr)
library(doMC)
registerDoMC(cores = 3)  # Register both cores for parallel processing

# note distinction between "trainING"/"testING" and "train"/"test"

# Specify column types in advance (using shorthand readr format)
coltypes <- paste0('ccddccc', paste(rep('d', 152), collapse = ''), 'c')
training <- read_csv('data/pml-training.csv', col_types = coltypes)
#testing <- read_csv('data/pml-testing.csv', col_types = coltypes)
rm(coltypes)
#probs <- problems(training)

# -------------------- Clean data ----------------------------------------------
# Look at missing values
Amelia::missmap(sample_frac(training, 0.1), y.cex = 0.2, x.cex = 0.5)

clean_data <- function(df){
  # Remove columns with huge proportion of missing values
  # Calculate proportion of missing values in each column
  prop_na <- sapply(df, function(x) {sum(is.na(x)) / length(x)})
  # Remove columns with more than 90% missing values
  keep <- names(df) %in% names(prop_na[prop_na > 0.9])
  df <- df[, !keep]
  # Convert columns to factors
  df$user_name <- as.factor(df$user_name)
  df$classe <- as.factor(df$class)
  df
}
training <- clean_data(training)

# Split data into train and test
in_train <- createDataPartition(y = training$classe, p = 0.7, list = FALSE)
train <- training[in_train, ]
test <- training[-in_train, ]
dim(train); dim(test)
rm(in_train, training, testing)

# --- Investigate --------------------------------------------------------------
library(corrplot)
corrplot(cor(train[, 8:59]))
# Some correlation between vars, worth using PCA
pre_proc <- preProcess(train[, 8:59], method = 'pca', thresh = 0.80)
pre_proc
train_pc <- predict(pre_proc, train[, 8:59])
plot(train_pc[, 1], train_pc[, 2], col = train$classe)

# --- Train model --------------------------------------------------------------
rf1 <- train(train$classe ~ ., 
             method = 'rf',
             data = train_pc)
beep(1)
rf2 <- train(train$classe ~.,
             method = 'rf',
             data = train[, 8:59])
beep(2)

rf

# --- Test ---------------------------------------------------------------------

test_pc <- predict(pre_proc, test[, 8:59])
confusionMatrix(test$classe, predict(rf1, test_pc))
cm <- confusionMatrix(test$classe, predict(rf2, test))

