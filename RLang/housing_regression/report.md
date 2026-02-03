## Data

``` r
script_dir <- getwd()
data_path <- file.path(script_dir, "..", "..", "DeepLearning", "dataset", "housing.csv")

housing <- read.csv(data_path)
if (colnames(housing)[1] == "" || colnames(housing)[1] == "X") {
  housing <- housing[, -1]
}

str(housing)
```

    ## 'data.frame':    20640 obs. of  9 variables:
    ##  $ MedInc     : num  8.33 8.3 7.26 5.64 3.85 ...
    ##  $ HouseAge   : num  41 21 52 52 52 52 52 52 42 52 ...
    ##  $ AveRooms   : num  6.98 6.24 8.29 5.82 6.28 ...
    ##  $ AveBedrms  : num  1.024 0.972 1.073 1.073 1.081 ...
    ##  $ Population : num  322 2401 496 558 565 ...
    ##  $ AveOccup   : num  2.56 2.11 2.8 2.55 2.18 ...
    ##  $ Latitude   : num  37.9 37.9 37.9 37.9 37.9 ...
    ##  $ Longitude  : num  -122 -122 -122 -122 -122 ...
    ##  $ MedHouseVal: num  4.53 3.58 3.52 3.41 3.42 ...

``` r
summary(housing)
```

    ##      MedInc           HouseAge        AveRooms          AveBedrms      
    ##  Min.   : 0.4999   Min.   : 1.00   Min.   :  0.8462   Min.   : 0.3333  
    ##  1st Qu.: 2.5634   1st Qu.:18.00   1st Qu.:  4.4407   1st Qu.: 1.0061  
    ##  Median : 3.5348   Median :29.00   Median :  5.2291   Median : 1.0488  
    ##  Mean   : 3.8707   Mean   :28.64   Mean   :  5.4290   Mean   : 1.0967  
    ##  3rd Qu.: 4.7432   3rd Qu.:37.00   3rd Qu.:  6.0524   3rd Qu.: 1.0995  
    ##  Max.   :15.0001   Max.   :52.00   Max.   :141.9091   Max.   :34.0667  
    ##    Population       AveOccup            Latitude       Longitude     
    ##  Min.   :    3   Min.   :   0.6923   Min.   :32.54   Min.   :-124.3  
    ##  1st Qu.:  787   1st Qu.:   2.4297   1st Qu.:33.93   1st Qu.:-121.8  
    ##  Median : 1166   Median :   2.8181   Median :34.26   Median :-118.5  
    ##  Mean   : 1425   Mean   :   3.0707   Mean   :35.63   Mean   :-119.6  
    ##  3rd Qu.: 1725   3rd Qu.:   3.2823   3rd Qu.:37.71   3rd Qu.:-118.0  
    ##  Max.   :35682   Max.   :1243.3333   Max.   :41.95   Max.   :-114.3  
    ##   MedHouseVal   
    ##  Min.   :0.150  
    ##  1st Qu.:1.196  
    ##  Median :1.797  
    ##  Mean   :2.069  
    ##  3rd Qu.:2.647  
    ##  Max.   :5.000

## Train/Test Split

``` r
idx <- sample(seq_len(nrow(housing)))
train_size <- floor(0.8 * nrow(housing))
train_idx <- idx[1:train_size]
test_idx <- idx[(train_size + 1):nrow(housing)]

train <- housing[train_idx, ]
test <- housing[test_idx, ]
```

## Model

``` r
model <- lm(MedHouseVal ~ ., data = train)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = MedHouseVal ~ ., data = train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.4940 -0.4628 -0.1315  0.3137  4.2079 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -3.606e+01  7.440e-01 -48.469  < 2e-16 ***
    ## MedInc       4.425e-01  4.783e-03  92.505  < 2e-16 ***
    ## HouseAge     9.216e-03  5.020e-04  18.359  < 2e-16 ***
    ## AveRooms    -1.148e-01  6.799e-03 -16.879  < 2e-16 ***
    ## AveBedrms    6.472e-01  3.125e-02  20.709  < 2e-16 ***
    ## Population  -2.971e-06  5.269e-06  -0.564    0.573    
    ## AveOccup    -3.501e-03  4.891e-04  -7.159 8.46e-13 ***
    ## Latitude    -4.130e-01  8.131e-03 -50.790  < 2e-16 ***
    ## Longitude   -4.248e-01  8.507e-03 -49.941  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7255 on 16503 degrees of freedom
    ## Multiple R-squared:  0.6063, Adjusted R-squared:  0.6061 
    ## F-statistic:  3177 on 8 and 16503 DF,  p-value: < 2.2e-16

## Evaluation

``` r
pred <- predict(model, newdata = test)
actual <- test$MedHouseVal

rmse <- sqrt(mean((pred - actual)^2))
mae <- mean(abs(pred - actual))
ss_res <- sum((actual - pred)^2)
ss_tot <- sum((actual - mean(actual))^2)
r2 <- 1 - ss_res / ss_tot

metrics <- data.frame(RMSE = rmse, MAE = mae, R2 = r2)
metrics
```

    ##        RMSE       MAE        R2
    ## 1 0.7200881 0.5278943 0.6048318
