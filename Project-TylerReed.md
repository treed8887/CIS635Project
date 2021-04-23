CIS635-Project
================
Tyler Reed
4/19/2021

``` r
knitr::opts_chunk$set(error = TRUE, fig.width = 12, fig.asp = 0.618)
```

``` r
library(tidyverse)
library(knitr)
library(e1071)
library(rpart)
library(rpart.plot)
library(neuralnet)
library(hrbrthemes)
library(readr)
library(purrr)
library(ggthemes)
library(varhandle)

testA <- read.table("data/dataTestA.txt", header = TRUE)
testB <- read.table("data/dataTestB.txt", header = TRUE)
trainA <- read.table("data/dataTrainA.txt", header = TRUE)
trainB <- read.table("data/dataTrainB.txt", header = TRUE)
```

``` r
# PRELIMINARY ANALYSIS AND CLEANUP, NO GRAPHS, AND MERGE


trainA <- as_tibble(trainA)

# Calculate summary statistics and produce visuals to check for outliers/noise/NAs
trainA %>%
  summary() %>%
  kable()
```

|  | id           | temp           | bpSys         | vo2            | throat      | atRisk         |
| :- | :----------- | :------------- | :------------ | :------------- | :---------- | :------------- |
|  | Min. : 0     | Min. : 15.00   | Min. : 20.0   | Min. : 10.00   | Min. : 81   | Min. :0.0000   |
|  | 1st Qu.:1673 | 1st Qu.: 97.79 | 1st Qu.:119.0 | 1st Qu.: 34.00 | 1st Qu.: 97 | 1st Qu.:0.0000 |
|  | Median :3352 | Median : 98.19 | Median :124.0 | Median : 39.00 | Median :100 | Median :0.0000 |
|  | Mean :3376   | Mean : 98.47   | Mean :124.6   | Mean : 37.76   | Mean :100   | Mean :0.4652   |
|  | 3rd Qu.:5084 | 3rd Qu.: 98.93 | 3rd Qu.:130.0 | 3rd Qu.: 42.00 | 3rd Qu.:103 | 3rd Qu.:1.0000 |
|  | Max. :6780   | Max. :198.83   | Max. :501.0   | Max. :150.00   | Max. :122   | Max. :1.0000   |
|  | NA           | NA’s :1        | NA’s :1       | NA’s :2        | NA’s :1     | NA             |

``` r
# trainA  %>%
#   mutate(across(.cols = everything(), as_factor)) %>%
#   keep(is.numeric) %>% 
#   gather() %>% 
#   ggplot(aes(value)) +
#     facet_wrap(~ key, scales = "free") +
#     theme_tufte(base_size = 16) +
#     geom_histogram(color = "royalblue", bins = 500)

# Test for duplicate records
length(unique(trainA$id)) == nrow(trainA)
```

    ## [1] TRUE

``` r
# Test for missing values by row
train_A_byrow<- rowSums(is.na(trainA))
max(train_A_byrow)
```

    ## [1] 1

``` r
# Results

# No more than one NA per dataset

# id: looks good and no duplicates
# temp: 1 NA, and min and max troublesome, use average
# bbSys: 1 NA, and min and max troublesome, use average
# vo2: 2 NA, max troublesome
# throat: 1 NA, max troublesome
# atRisk: looks good
```

``` r
trainB <- as_tibble(trainB)

# Calculate summary statistics and produce visuals to check for outliers/noise/NAs
trainB %>%
  summary(trainB) %>%
  kable()
```

|  | id           | headA          | bodyA         | cough          | runny          | nausea         | diarrhea      | atRisk         |
| :- | :----------- | :------------- | :------------ | :------------- | :------------- | :------------- | :------------ | :------------- |
|  | Min. : 0     | Min. : 0.000   | Min. :1.000   | Min. :0.0000   | Min. :0.0000   | Min. :0.0000   | Min. :0.000   | Min. :0.0000   |
|  | 1st Qu.:1673 | 1st Qu.: 3.000 | 1st Qu.:4.000 | 1st Qu.:0.0000 | 1st Qu.:0.0000 | 1st Qu.:0.0000 | 1st Qu.:0.000 | 1st Qu.:0.0000 |
|  | Median :3352 | Median : 3.000 | Median :4.000 | Median :0.0000 | Median :0.0000 | Median :0.0000 | Median :0.000 | Median :0.0000 |
|  | Mean :3376   | Mean : 3.461   | Mean :4.016   | Mean :0.3418   | Mean :0.1986   | Mean :0.2367   | Mean :0.102   | Mean :0.4652   |
|  | 3rd Qu.:5084 | 3rd Qu.: 4.000 | 3rd Qu.:4.000 | 3rd Qu.:1.0000 | 3rd Qu.:0.0000 | 3rd Qu.:0.0000 | 3rd Qu.:0.000 | 3rd Qu.:1.0000 |
|  | Max. :6780   | Max. :100.000  | Max. :7.000   | Max. :1.0000   | Max. :1.0000   | Max. :5.0000   | Max. :1.000   | Max. :1.0000   |
|  | NA           | NA’s :1        | NA            | NA             | NA’s :1        | NA             | NA’s :1       | NA             |

``` r
# 
# trainA %>%
#   mutate(across(.cols = everything(), as_factor)) %>%
#   select(-id) %>%
#   filter(temp > 106) %>%
#   ggplot(aes(x = temp)) +
#     geom_bar(fill = "royalblue", position = "dodge") +
#     scale_fill_brewer(palette = "Dark2") +
#     theme_tufte(base_size = 16) 
    


# Test for duplicate records
length(unique(trainB$id)) == nrow(trainB)
```

    ## [1] TRUE

``` r
# Test for missing values by row
train_B_byrow <- rowSums(is.na(trainB))
max(train_B_byrow)
```

    ## [1] 1

``` r
# Results

# id: looks good and no duplicates
# headA: 1 NA, max troublesome
# bodyA: looks good
# cough: looks good
# runny: 1 NA
# nausea: max is troublesome
# diarrhea: 1 NA
# atRisk: looks good
```

``` r
clean_train <- function(A, B) {
  
  # Merge `trainA` and `trainB`
  merged_train <- A %>%
    select(-atRisk) %>%
    left_join(B, by = "id") %>%
    # Convert NAs of factor variables to the variable mode 
    mutate(across(6:12, ~ replace_na(., getmode(.)))) %>%
    mutate(across(2:5, as.numeric)) %>%
    # Convert NAs of numeric variables to the variable mean 
    mutate(across(2:5, ~ replace_na(., mean(., na.rm = TRUE))))

  # Clean data: replacing any noise with mode or mean according to type
  for (i in 2:11) {
      merged_train[, i] <- modify(merged_train[, i], clean_helpers[i - 1])
  }
  
  # Convert variables to respective types
  merged_train <- merged_train %>%
    mutate(across(6:12, as_factor))
  
  merged_train
}

cleaned_train <- clean_train(trainA, trainB)

kable(summary(cleaned_train), caption = "New Summary Statistics to Confirm Cleaned Training Data")
```

|  | id           | temp           | bpSys         | vo2           | throat      | headA       | bodyA  | cough  | runny  | nausea | diarrhea | atRisk |
| :- | :----------- | :------------- | :------------ | :------------ | :---------- | :---------- | :----- | :----- | :----- | :----- | :------- | :----- |
|  | Min. : 0     | Min. : 96.18   | Min. : 97.0   | Min. :10.00   | Min. : 81   | 3 :2970     | 1: 7   | 0:3570 | 0:4347 | 0:4145 | 0:4871   | 0:2901 |
|  | 1st Qu.:1673 | 1st Qu.: 97.79 | 1st Qu.:119.0 | 1st Qu.:34.00 | 1st Qu.: 97 | 5 : 906     | 2: 91  | 1:1854 | 1:1077 | 1:1279 | 1: 553   | 1:2523 |
|  | Median :3352 | Median : 98.19 | Median :124.0 | Median :39.00 | Median :100 | 4 : 715     | 3: 709 | NA     | NA     | NA     | NA       | NA     |
|  | Mean :3376   | Mean : 98.47   | Mean :124.5   | Mean :37.74   | Mean :100   | 2 : 544     | 4:3745 | NA     | NA     | NA     | NA       | NA     |
|  | 3rd Qu.:5084 | 3rd Qu.: 98.93 | 3rd Qu.:130.0 | 3rd Qu.:42.00 | 3rd Qu.:103 | 6 : 172     | 5: 753 | NA     | NA     | NA     | NA       | NA     |
|  | Max. :6780   | Max. :101.40   | Max. :149.0   | Max. :58.00   | Max. :116   | 1 : 91      | 6: 110 | NA     | NA     | NA     | NA       | NA     |
|  | NA           | NA             | NA            | NA            | NA          | (Other): 26 | 7: 9   | NA     | NA     | NA     | NA       | NA     |

New Summary Statistics to Confirm Cleaned Training Data

``` r
# Convert datatypes of variables and merge test data; testA and testB
cleaned_test <- testA %>%
    as_tibble() %>%
    select(-atRisk) %>%
    left_join(testB, by = "id") %>%
    mutate(across(2:5, as.numeric)) %>%
    mutate(across(6:12, as_factor))
```

``` r
modTree <- rpart(atRisk~temp+bpSys+vo2+throat+headA+bodyA+cough+runny+nausea+diarrhea, cleaned_test)

rpart.plot(modTree)
```

![](Project-TylerReed_files/figure-gfm/trees-1.png)<!-- -->

``` r
predTree <- predict(modTree,cleaned_test, type = "vector")
table(predTree, cleaned_test$atRisk)
```

    ##         
    ## predTree   0   1
    ##        1 655 103
    ##        2  80 519

``` r
# 86.51% Accuracy
```

``` r
modBayes <- naiveBayes(atRisk~.-id, cleaned_test)
predBayes <- predict(modBayes, cleaned_test)
table(predBayes, cleaned_test$atRisk)
```

    ##          
    ## predBayes   0   1
    ##         0 649 127
    ##         1  86 495

``` r
# 84.3% Accuracy
```

``` r
cleaned_test_svm <- cleaned_test %>%
  mutate(across(where(is.factor), unfactor))

modSVM <- svm(cleaned_test_svm[, 2:11], kernel = "linear")
predSVM <- predict(modSVM, cleaned_test_svm[, 2:11])
table(predSVM, cleaned_test_svm$atRisk)
```

    ##        
    ## predSVM   0   1
    ##   FALSE 174 188
    ##   TRUE  561 434

``` r
# 44.8% Accuracy

modSVM_Poly <- svm(cleaned_test_svm[, 2:11], kernel = "polynomial")
predSVM_Poly <- predict(modSVM_Poly, cleaned_test_svm[, 2:11])
table(predSVM_Poly, cleaned_test_svm$atRisk)
```

    ##             
    ## predSVM_Poly   0   1
    ##        FALSE 471 219
    ##        TRUE  264 403

``` r
# 64.8% Accuracy
```
