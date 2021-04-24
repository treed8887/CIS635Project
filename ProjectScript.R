# title: "CIS635-Project"
# author: "Tyler Reed"
# date: "4/23/2021"


# Load packages

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
library(fastDummies)
library(kableExtra)
library(patchwork)
library(gridExtra)

# Read in data
testA <- read.table("data/dataTestA.txt", header = TRUE)
testB <- read.table("data/dataTestB.txt", header = TRUE)
trainA <- read.table("data/dataTrainA.txt", header = TRUE)
trainB <- read.table("data/dataTrainB.txt", header = TRUE)

# CLEAN AND MERGE

trainA <- as_tibble(trainA)
trainB <- as_tibble(trainB)

# Create helper functions to define thresholds for vitals variables

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

temp <- function(v) {
  for (i in 1:length(v)) {
    if (!(between(v[i], 90, 106))) {
      v[i] <- mean(v)
    }
  }
  v
}

bpSys <- function(v) {
  for (i in 1:length(v)) {
    if (!(between(v[i], 90, 150))) {
      v[i] <- mean(v)
    }
  }
  v
}

vo2 <- function(v) {
  for (i in 1:length(v)) {
    if (!(between(v[i], 10, 70))) {
      v[i] <- mean(v)
    }
  }
  v
}

throat <- function(v) {
  for (i in 1:length(v)) {
    if (!(between(v[i], 80, 120))) {
      v[i] <- mean(v)
    }
  }
  v
}

headA<- function(v) {
  for (i in 1:length(v)) {
    if (!(between(v[i], 0, 9))) {
      v[i] <- getmode(v)
    }
  }
  v
}

bodyA <- function(v) {
  for (i in 1:length(v)) {
    if (!(between(v[i], 0, 9))) {
      v[i] <- getmode(v)
    }
  }
  v
}

cough <- function(v) {
  for (i in 1:length(v)) {
    if (v[i] != 1 & v[i] != 0) {
      v[i] <- getmode(v)
    }
  }
  v
}

runny <- function(v) {
  for (i in 1:length(v)) {
    if (v[i] != 1 & v[i] != 0) {
      v[i] <- getmode(v)
    }
  }
  v
}

nausea <- function(v) {
  for (i in 1:length(v)) {
    if (v[i] != 1 & v[i] != 0) {
      v[i] <- getmode(v)
    }
  }
  v
}

diarrhea <- function(v) {
  for (i in 1:length(v)) {
    if (v[i] != 1 & v[i] != 0) {
      v[i] <- getmode(v)
    }
  }
  v
}

# Vector of cleaning helper functions 
clean_helpers <- list(temp,
                      bpSys,
                      vo2,
                      throat,
                      headA,
                      bodyA,
                      cough,
                      runny,
                      nausea,
                      diarrhea)

# The following is to aid in testing the helper functions above
# max(temp_noise(merged_train$temp))
# min(temp_noise(merged_train$temp))
# 
# max(bpSys_noise(merged_train$bpSys))
# min(bpSys_noise(merged_train$bpSys))
# 
# max(vo2_noise(merged_train$vo2))
# min(vo2_noise(merged_train$vo2))
# 
# max(throat_noise(merged_train$throat))
# min(throat_noise(merged_train$throat))
# 
# max(headA_noise(merged_train$headA))
# min(headA_noise(merged_train$headA))
# 
# max(bodyA_noise(merged_train$bodyA))
# min(bodyA_noise(merged_train$bodyA))
# 
# max(cough_noise(merged_train$cough))
# min(cough_noise(merged_train$cough))
# 
# max(runny_noise(merged_train$runny))
# min(runny_noise(merged_train$runny))
# 
# max(nausea_noise(merged_train$nausea))
# min(nausea_noise(merged_train$nausea))
# 
# max(diarrhea_noise(merged_train$diarrhea))
# min(diarrhea_noise(merged_train$diarrhea))

# Function to clean and merge training data

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

xTrain <- clean_train(trainA, trainB)

# Convert datatypes of variables and merge test data; testA and testB
xTest<- testA %>%
  as_tibble() %>%
  select(-atRisk) %>%
  left_join(testB, by = "id") %>%
  mutate(across(2:5, as.numeric)) %>%
  mutate(across(6:12, as_factor))

# Functions for building random forest classification model
rando_forest <- function(x,t,n,d) {
  # two stop statements to check for out of bounds for `n` and `d`
  if (n < 1 | n > nrow(x)) {
    stop("No. of instances chosen not within no. of rows of dataframe.")
  }
  if (d < 1 | d > ncol(x)) {
    stop("No. of attributes chosen not within no. of cols of dataframe.")
  }
  
  # for loop adding `rpart` models to list
  ls <- list()
  for (i in 1:t) {
    str <- "atRisk"
    sep <- "~"
    sam_d <- sample((ncol(x)), d, replace = FALSE)
    # for loop creating string of formula for sampling attributes
    for (j in sam_d) {
      str <- paste0(str, sep, names(x)[j])
      sep <- "+"
    }
    sam_n <- sample(nrow(x), n, replace = TRUE)
    ls[[i]] <- rpart(str, x[sam_n,])
  }
  return (ls)
}

pred <- function(ls, x) {
  # for loop creating dataframe of each models predictions per instance
  df <- cbind(tibble(predict(ls[[1]], x)))
  for (i in 2:length(ls)) {
    df <- cbind(df, data_frame(predict(ls[[i]], x)))
  }
  # for loop creating vector of ensemble random forest predictions by instance
  means <- as_tibble(rowMeans(df))
  vec <- c()
  for (i in 1:nrow(means)) {
    if (means[i, 1] <= 0.50) {
      vec <- rbind(vec, 0)
    } else if (means[i, 1] > 0.50) {
      vec <- rbind(vec, 1)
    }
  }
  return(vec)
}  

# Build classification model, in this case Random Forests was chosen
xTrain_noFactors <- xTrain %>%
  mutate(across(where(is.factor), unfactor))

xTest_noFactors <- xTest %>%
  mutate(across(where(is.factor), unfactor))

forest <- rando_forest(xTrain_noFactors, t = 10, n = 5000, d = 8)
pred_forest <- pred(forest, xTest_noFactors)
pred_table <- data_frame( xTest$id, pred_forest)


# Write predictions per employee to .txt
write.table(pred_table, 
            file = "atRisk_employees_predictions.txt", 
            sep = "\t",
            row.names = FALSE, 
            col.names = TRUE)

# Generate plot of data
temp <- xTest %>%
  ggplot(aes(x = temp, color = atRisk, fill = atRisk)) +
  geom_histogram(binwidth = .01, bins = 2000, stat = "density") +
  scale_fill_brewer(name = "At Risk", palette = "Greens", labels = c("No", "Yes")) +
  scale_color_brewer(name = "At Risk", palette = "Greens", labels = c("No", "Yes")) +
  theme_tufte(base_size = 14) +
  theme(legend.text = element_text(size = 11)) +
  labs(x = "Temperature",
       y = "Density")

bpSys <- xTest %>%
  ggplot(aes(x = bpSys, color = atRisk, fill = atRisk)) +
  geom_histogram(binwidth = 1, bins = 200, stat = "density") +
  scale_fill_brewer(name = "At Risk", palette = "Greens", labels = c("No", "Yes")) +
  scale_color_brewer(name = "At Risk", palette = "Greens", labels = c("No", "Yes")) +
  theme_tufte(base_size = 14) +
  theme(legend.text = element_text(size = 11)) +
  labs(x = "Sys. Blood Pressure",
       y = "Density")

vo2 <- xTest %>%
  ggplot(aes(x = vo2, color = atRisk, fill = atRisk)) +
  geom_histogram(binwidth = 1, bins = 200, stat = "density") +
  scale_fill_brewer(name = "At Risk", palette = "Greens", labels = c("No", "Yes")) +
  scale_color_brewer(name = "At Risk", palette = "Greens", labels = c("No", "Yes")) +
  theme_tufte(base_size = 14) +
  theme(legend.text = element_text(size = 11)) +
  labs(x = "Vo2 Max",
       y = "Density")

throat <- xTest %>%
  ggplot(aes(x = throat, color = atRisk, fill = atRisk)) +
  geom_histogram(binwidth = 1, bins = 200, stat = "density") +
  scale_fill_brewer(name = "At Risk", palette = "Greens", labels = c("No", "Yes")) +
  scale_color_brewer(name = "At Risk", palette = "Greens", labels = c("No", "Yes")) +
  theme_tufte(base_size = 14) +
  theme(legend.text = element_text(size = 11)) +
  labs(x = "Throat Culture",
       y = "Density") 

plot <- (temp | bpSys) / (throat | vo2)

plot <- plot +
  plot_annotation(title = "Select Vitals Comparison",
                  subtitle = "to `atRisk` Probability",
                  caption = "Employee Vitals Data",
                  them = theme(plot.title = element_text(size = 20, face = "bold"),
                               plot.subtitle = element_text(size = 16, color = "#858585")))

# Wrie plot to .png
ggsave("plot.png", width = 40, height = 40, units = "cm")


