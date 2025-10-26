# =========================================================================
# FILE: b2b_fleet_segmentation.R
# PROJECT: Optimizing Automotive Fleet Sales - B2B Data Mining
# AUTHOR: Ritik Srivastava
# DATE: 2024-09-21 
# DESCRIPTION:
#   Single-script pipeline:
#     - Load & clean Auto Sales data
#     - Exploratory Data Analysis (time series & category summaries)
#     - RFM (Recency, Frequency, Monetary) + K-means segmentation
#     - Random Forest regression for SALES prediction
#     - Diagnostics, evaluation, and artifact export (plots & csv)
# =========================================================================

# ---------------------------
# 0. PREAMBLE & LIBRARIES
# ---------------------------
required_pkgs <- c(
  "tidyverse", "lubridate", "scales", "forecast", "gridExtra",
  "cluster", "factoextra", "rsample", "randomForest", "yardstick", "vip",
  "glue"
)

new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs, repos = "https://cloud.r-project.org")

library(tidyverse)
library(lubridate)
library(scales)
library(gridExtra)
library(cluster)
library(factoextra)
library(rsample)
library(randomForest)
library(yardstick)
library(vip)
library(glue)

# For nicer progress/logging in scripts
log_msg <- function(...) cat(glue::glue(...), "\n")

# ---------------------------
# 1. ARGUMENTS & SETTINGS
# ---------------------------
args <- commandArgs(trailingOnly = TRUE)
data_path <- ifelse(length(args) >= 1, args[[1]], "Auto Sales data.csv")

# Outputs
out_dir <- "outputs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Reproducibility
set.seed(42)

# ---------------------------
# 2. DATA LOADING & CLEANING
# ---------------------------
log_msg("[1] Loading data from: {data_path}")
if (!file.exists(data_path)) stop("Data file not found. Provide correct path as first arg.")

raw <- read_csv(data_path, show_col_types = FALSE)

# Check expected columns and minimal validation
expected_cols <- c("ORDERNUMBER", "ORDERDATE", "QUANTITYORDERED", "PRICEEACH",
                   "SALES", "CUSTOMERNAME", "COUNTRY", "PRODUCTLINE", "MSRP",
                   "DEALSIZE", "STATUS", "DAYS_SINCE_LASTORDER")
missing_cols <- setdiff(expected_cols, colnames(raw))
if (length(missing_cols) > 0) {
  warning("Missing expected columns: ", paste(missing_cols, collapse = ", "))
}

# Standardize and create necessary columns
df <- raw %>%
  rename_with(~ str_replace_all(., " ", "_")) %>%  # spaces -> underscores
  mutate(
    ORDERDATE = parse_date_time(ORDERDATE, orders = c("dmy", "ymd", "mdy")),
    order_year = year(ORDERDATE),
    order_month = month(ORDERDATE),
    order_month_label = month(ORDERDATE, label = TRUE, abbr = TRUE),
    customer = str_c(CUSTOMERNAME, " (", COUNTRY, ")"),
    order_quantity = QUANTITYORDERED,
    sales_amount = SALES,
    price_each = PRICEEACH,
    product_line = PRODUCTLINE,
    deal_size = DEALSIZE
  ) %>%
  select(any_of(c("ORDERNUMBER", "ORDERDATE", "order_year", "order_month",
                  "order_month_label", "customer", "COUNTRY", "product_line",
                  "order_quantity", "price_each", "MSRP", "sales_amount",
                  "deal_size", "STATUS", "DAYS_SINCE_LASTORDER")))

log_msg("[2] Data dimensions: {nrow(df)} rows x {ncol(df)} cols")
log_msg("[3] Missing values (total): {sum(is.na(df))}")

# Save cleaned sample
write_csv(df, file.path(out_dir, "cleaned_auto_sales_sample.csv"))

# ---------------------------
# 3. EXPLORATORY DATA ANALYSIS
# ---------------------------
log_msg("[4] Generating EDA summaries and plots...")

# 3.1 Monthly revenue time series
sales_by_month <- df %>%
  filter(!is.na(ORDERDATE)) %>%
  mutate(year_month = floor_date(ORDERDATE, "month")) %>%
  group_by(year_month) %>%
  summarise(total_sales = sum(sales_amount, na.rm = TRUE), .groups = "drop")

p_time <- ggplot(sales_by_month, aes(x = year_month, y = total_sales)) +
  geom_line(size = 0.8) +
  labs(title = "Monthly Fleet Revenue Over Time", x = "Month", y = "Total Revenue ($)") +
  scale_y_continuous(labels = scales::dollar) +
  theme_minimal()

ggsave(filename = file.path(out_dir, "monthly_revenue.png"), plot = p_time, width = 10, height = 4)

# 3.2 Top countries & product lines
p_country <- df %>%
  group_by(COUNTRY) %>%
  summarise(total_sales = sum(sales_amount, na.rm = TRUE), .groups = "drop") %>%
  slice_max(total_sales, n = 10) %>%
  ggplot(aes(x = reorder(COUNTRY, total_sales), y = total_sales)) +
  geom_col() + coord_flip() + labs(title = "Total Revenue by Top 10 Countries", x = NULL, y = "Total Revenue") +
  scale_y_continuous(labels = scales::dollar) + theme_minimal()

p_product <- df %>%
  group_by(product_line) %>%
  summarise(total_sales = sum(sales_amount, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = reorder(product_line, total_sales), y = total_sales)) +
  geom_col() + coord_flip() + labs(title = "Total Revenue by Vehicle Category", x = NULL, y = "Total Revenue") +
  scale_y_continuous(labels = scales::dollar) + theme_minimal()

ggsave(filename = file.path(out_dir, "top_countries_and_products.png"),
       plot = grid.arrange(p_country, p_product, nrow = 2), width = 10, height = 8)

log_msg("[5] EDA plots saved to {out_dir} (monthly_revenue.png, top_countries_and_products.png)")

# ---------------------------
# 4. RFM & K-MEANS SEGMENTATION
# ---------------------------
log_msg("[6] Building RFM table and clustering clients...")

# RFM: Recency is DAYS_SINCE_LASTORDER if present; else compute relative to max date
if (!"DAYS_SINCE_LASTORDER" %in% colnames(df) || all(is.na(df$DAYS_SINCE_LASTORDER))) {
  reference_date <- max(df$ORDERDATE, na.rm = TRUE)
  df <- df %>%
    mutate(days_since = as.numeric(difftime(reference_date, ORDERDATE, units = "days")))
  recency_col <- "days_since"
} else {
  recency_col <- "DAYS_SINCE_LASTORDER"
}

rfm <- df %>%
  group_by(CUSTOMERNAME) %>%
  summarise(
    Recency = min(.data[[recency_col]], na.rm = TRUE),
    Frequency = n_distinct(ORDERNUMBER),
    Monetary = sum(sales_amount, na.rm = TRUE),
    .groups = "drop"
  )

# Basic sanity checks
log_msg("RFM rows: {nrow(rfm)}; Recency range: {min(rfm$Recency, na.rm=TRUE)} - {max(rfm$Recency, na.rm=TRUE)}")

# Scale for clustering
rfm_scaled <- rfm %>%
  select(Recency, Frequency, Monetary) %>%
  mutate_all(~ ifelse(is.infinite(.) | is.nan(.), NA, .)) %>%
  drop_na() %>%
  scale()

# Optimal k via silhouette (limit search to k = 2..6 to be pragmatic)
nb <- tryCatch({
  fviz_nbclust(rfm_scaled, kmeans, method = "silhouette", k.max = 6)
}, error = function(e) {
  log_msg("Warning: silhouette computation failed; defaulting to k = 3")
  NULL
})

# If silhouette plot exists we won't use it programmatically; choose k = 3 as domain-validated default
k_opt <- 3L
log_msg("Selected k = {k_opt} for K-means clustering")

set.seed(42)
kres <- kmeans(rfm_scaled, centers = k_opt, nstart = 25)
rfm$Cluster <- factor(kres$cluster)

cluster_profiles <- rfm %>%
  group_by(Cluster) %>%
  summarise(
    Avg_Recency = mean(Recency, na.rm = TRUE),
    Avg_Frequency = mean(Frequency, na.rm = TRUE),
    Avg_Monetary = mean(Monetary, na.rm = TRUE),
    Client_Count = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Avg_Monetary))

log_msg("Cluster profiles:\n")
print(cluster_profiles)

write_csv(rfm, file.path(out_dir, "rfm_with_clusters.csv"))
write_csv(cluster_profiles, file.path(out_dir, "cluster_profiles.csv"))

# Visualize clusters (PCA-based)
p_cluster <- fviz_cluster(kres, data = rfm_scaled, geom = "point", ellipse.type = "convex",
                          ggtheme = theme_minimal()) + labs(title = "Client Segments (K-means)")

ggsave(filename = file.path(out_dir, "rfm_clusters.png"), plot = p_cluster, width = 8, height = 5)
log_msg("[7] RFM clustering artifacts saved to {out_dir}")

# ---------------------------
# 5. RANDOM FOREST: Predict SALES
# ---------------------------
log_msg("[8] Preparing data for prediction and training Random Forest...")

# Select predictors; convert categorical columns to factors
predictors <- df %>%
  select(sales_amount, order_quantity, price_each, MSRP, product_line, COUNTRY, deal_size) %>%
  mutate(across(c(product_line, COUNTRY, deal_size), factor, .names = "f_{col}"))

# Some basic cleaning: drop rows with missing sales
predictors <- predictors %>%
  drop_na(sales_amount, order_quantity, price_each)

# Rename for modeling convenience
predictors <- predictors %>%
  rename(SALES = sales_amount, QUANTITYORDERED = order_quantity, PRICEEACH = price_each)

# Train/test split (80/20)
set.seed(42)
split <- initial_split(predictors, prop = 0.80)
train <- training(split)
test <- testing(split)

log_msg("Train rows: {nrow(train)}; Test rows: {nrow(test)}")

# Fit Random Forest
set.seed(42)
rf_model <- randomForest(SALES ~ ., data = train, ntree = 500, importance = TRUE)
log_msg("[9] Random Forest trained. Summary:")
print(rf_model)

# Predictions & evaluation
preds <- predict(rf_model, newdata = test)
results <- test %>%
  select(SALES) %>%
  bind_cols(predicted_sales = preds)

# Compute metrics via yardstick
metrics_tbl <- results %>%
  metrics(truth = SALES, estimate = predicted_sales)

log_msg("Model evaluation metrics:\n")
print(metrics_tbl)

# Save predictions & metrics
write_csv(results, file.path(out_dir, "rf_predictions_test_set.csv"))
write_csv(metrics_tbl, file.path(out_dir, "rf_metrics.csv"))

# Feature importance plot (vip)
p_vip <- vip::vip(rf_model, num_features = 10) + labs(title = "Random Forest Feature Importance")
ggsave(filename = file.path(out_dir, "rf_feature_importance.png"), plot = p_vip, width = 8, height = 5)

log_msg("[10] Random Forest artifacts saved to {out_dir}")

# ---------------------------
# 6. DIAGNOSTICS & SANITY CHECKS
# ---------------------------
log_msg("[11] Running quick diagnostics...")

# Check for overfitting: compare training MSE and test MSE
train_preds <- predict(rf_model, newdata = train)
train_mse <- mean((train_preds - train$SALES)^2, na.rm = TRUE)
test_mse <- mean((preds - test$SALES)^2, na.rm = TRUE)

log_msg("Training MSE: {round(train_mse, 2)}; Test MSE: {round(test_mse, 2)}")
if (train_mse * 2 < test_mse) {
  log_msg("Warning: Possible underfitting/over-regularization (test MSE >> train MSE). Inspect features/model.")
}

# Save session info for reproducibility
writeLines(capture.output(sessionInfo()), file.path(out_dir, "session_info.txt"))
log_msg("[12] Session information saved.")

# ---------------------------
# 7. SUMMARY EXPORT
# ---------------------------
summary_report <- list(
  data_rows = nrow(df),
  rfm_clients = nrow(rfm),
  clusters = as.data.frame(cluster_profiles),
  rf_metrics = as.data.frame(metrics_tbl)
)

# Write a small summary CSV + JSON-like text
write_csv(cluster_profiles, file.path(out_dir, "summary_cluster_profiles.csv"))
write_csv(metrics_tbl, file.path(out_dir, "summary_model_metrics.csv"))

log_msg("Script finished. All artifacts are in the '{out_dir}' directory.")
log_msg("Key outputs:\n - cleaned_auto_sales_sample.csv\n - monthly_revenue.png\n - top_countries_and_products.png\n - rfm_with_clusters.csv\n - cluster_profiles.csv\n - rfm_clusters.png\n - rf_predictions_test_set.csv\n - rf_metrics.csv\n - rf_feature_importance.png\n - session_info.txt")
