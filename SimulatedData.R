library(MASS)

## Functions for generating covariance matrices C_1 - C_4 as described in section 4.2 of paper
generate_C_1 <- function(k) {
  10*diag(k) - 7*(diag(k) - matrix(1, k, k))
}

generate_C_2 <- function(k) {
  C_2 <- matrix(0, k, k)
  for (i in 1:k) {
    for (j in i:k) {
      C_2[i, j] <- 10*0.4**abs(i - j)
      C_2[j, i] <- 10*0.4**abs(j - i)
    }
  }
  C_2
}

generate_C_3 <- function(k) {
  0.3*generate_C_1(k) + 0.7*generate_C_2(k)
}

generate_C_4 <- function(k) {
  C_4 <- matrix(0, k, k)
  for (i in 1:k) {
    for (j in i:k) {
      C_4[i, j] <- 10/sqrt(1 + abs(i - j))
      C_4[j, i] <- 10/sqrt(1 + abs(j - i))
    }
  }
  C_4
}


## Functions for generating datasets as described in 4.2 of paper

generate_C_1_datasets <- function(num_datasets, N, k) {
  C_1 <- generate_C_1(k)
  C_1_datasets <- vector(mode='list', length=num_datasets)
  for (i in 1:num_datasets) {
    C_1_datasets[[i]] <- mvrnorm(N, numeric(k), Sigma = C_1)
  }
  C_1_datasets
}

generate_C_2_datasets <- function(num_datasets, N, k) {
  C_2 <- generate_C_2(k)
  C_2_datasets <- vector(mode='list', length=num_datasets)
  for (i in 1:num_datasets) {
    C_2_datasets[[i]] <- mvrnorm(N, numeric(k), Sigma = C_2)
  }
  C_2_datasets
}

generate_C_3_datasets <- function(num_datasets, N, k) {
  C_3 <- generate_C_3(k)
  C_3_datasets <- vector(mode='list', length=num_datasets)
  for (i in 1:num_datasets) {
    C_3_datasets[[i]] <- mvrnorm(N, numeric(k), Sigma = C_3)
  }
  C_3_datasets
}

generate_C_4_datasets <- function(num_datasets, N, k) {
  C_4 <- generate_C_4(k)
  C_4_datasets <- vector(mode='list', length=num_datasets)
  for (i in 1:num_datasets) {
    C_4_datasets[[i]] <- mvrnorm(N, numeric(k), Sigma = C_4)
  }
  C_4_datasets
}

