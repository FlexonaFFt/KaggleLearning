# Housing price regression example in R (base R only)

set.seed(42)

# Path to dataset (relative to this script)
# Rscript doesn't set sys.frame(1)$ofile, so use commandArgs instead.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
data_path <- file.path(script_dir, "..", "..", "DeepLearning", "dataset", "housing.csv")

if (!file.exists(data_path)) {
  stop(paste("Dataset not found at:", data_path))
}

# Load data
housing <- read.csv(data_path)

# Drop index column if present (first column is empty name or looks like row index)
if (colnames(housing)[1] == "" || colnames(housing)[1] == "X") {
  housing <- housing[, -1]
}

# Ensure target column exists
if (!"MedHouseVal" %in% colnames(housing)) {
  stop("Target column 'MedHouseVal' not found in dataset")
}

# Train/test split (80/20)
idx <- sample(seq_len(nrow(housing)))
train_size <- floor(0.8 * nrow(housing))
train_idx <- idx[1:train_size]
test_idx <- idx[(train_size + 1):nrow(housing)]

train <- housing[train_idx, ]
test <- housing[test_idx, ]

# Fit linear regression
model <- lm(MedHouseVal ~ ., data = train)

# Predict
pred <- predict(model, newdata = test)
actual <- test$MedHouseVal

# Metrics
rmse <- sqrt(mean((pred - actual)^2))
mae <- mean(abs(pred - actual))
ss_res <- sum((actual - pred)^2)
ss_tot <- sum((actual - mean(actual))^2)
r2 <- 1 - ss_res / ss_tot

cat("Linear Regression Results\n")
cat(sprintf("RMSE: %.4f\n", rmse))
cat(sprintf("MAE : %.4f\n", mae))
cat(sprintf("R^2 : %.4f\n", r2))

# Save model
model_path <- file.path(script_dir, "model.rds")
saveRDS(model, model_path)
cat(paste("Model saved to:", model_path, "\n"))
