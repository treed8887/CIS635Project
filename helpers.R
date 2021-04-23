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
max(temp_noise(merged_train$temp))
min(temp_noise(merged_train$temp))

max(bpSys_noise(merged_train$bpSys))
min(bpSys_noise(merged_train$bpSys))

max(vo2_noise(merged_train$vo2))
min(vo2_noise(merged_train$vo2))

max(throat_noise(merged_train$throat))
min(throat_noise(merged_train$throat))

max(headA_noise(merged_train$headA))
min(headA_noise(merged_train$headA))

max(bodyA_noise(merged_train$bodyA))
min(bodyA_noise(merged_train$bodyA))

max(cough_noise(merged_train$cough))
min(cough_noise(merged_train$cough))

max(runny_noise(merged_train$runny))
min(runny_noise(merged_train$runny))

max(nausea_noise(merged_train$nausea))
min(nausea_noise(merged_train$nausea))

max(diarrhea_noise(merged_train$diarrhea))
min(diarrhea_noise(merged_train$diarrhea))




