bootstrap_test <- function(x,y, n = 1e2){
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  
  new_x <- c()
  new_y <- c()
  
  for(i in 1:n){
    new_x[[i]] <- sample(x, replace = TRUE) %>% mean(na.rm = TRUE)
    new_y[[i]] <- sample(y, replace = TRUE) %>% mean(na.rm = TRUE)
  }
  
  t.test(new_x, new_y) %>% return()
}


bootstrap_test <- function(x, y, n = 1e3) {
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  
  non_b_statistic <- t.test(x, y) %>% .$statistic
  
  
  middle <- c(x, y) %>% mean(na.rm = TRUE)
  xt <- x - mean(x, na.rm = TRUE) + middle
  yt <- y - mean(y, na.rm = TRUE) + middle
  
  boot_t <- c()
  for (i in 1:n) {
    sample_x <- sample(xt, replace = TRUE)
    sample_y <- sample(yt, replace = TRUE)
    boot_t[[i]] <- t.test(sample_x, sample_y) %>% .$statistic
  }
  result <- mean(abs(boot_t) > abs(non_b_statistic))
  result %>% return()
}


bootstrap_test3 <- function(x, y, n = 1e4){
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  diffs <- c()
  for(i in 1:n){
    xt <- sample(x, replace = TRUE)
    yt <- sample(y, replace = TRUE)
    diffs[[i]] <- mean(xt) - mean(yt)
  }
  1 - ecdf(diffs)(0)
}

Boot_fun = function(x, y){
  

  Boot_x = sample(x, length(x), replace =TRUE)
  Boot_y = sample(y, length(y), replace = TRUE)
  mean(Boot_x) - mean(Boot_y)
} 

Diffs = unlist(replicate(1000, Boot_fun)) 

#95% CI
quantile(Diffs, probs = c(0.025, 0.975))
