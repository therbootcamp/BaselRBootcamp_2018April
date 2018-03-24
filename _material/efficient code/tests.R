
require(readr)
require(microbenchmark)
require(data.table)

f0 <- expression(read_csv('https://therbootcamp.github.io/_slides/data/titanic.csv'))
f1 <- expression(read_csv('_slides/D4S1_EfficientCode/titanic.csv'))
f2 <- expression(fread('_slides/D4S1_EfficientCode/titanic.csv'))

a = microbenchmark(eval(f0), eval(f1), eval(f2), times = 5)

microbenchmark(unlist(d), unlist(d, use.names = FALSE), times = 5)

# read data
my_data <- matrix(rnorm(1000000), ncol = 10)

# compute means of columns
f0 <- expression(colMeans(my_data))
f1 <- expression(apply(my_data, 2, mean))

a = microbenchmark(eval(f0), eval(f1), times = 2)


# define character vector
letters10  <- LETTERS[sample(1:26, 10, replace = T)]
letters100 <- LETTERS[sample(1:26, 100, replace = T)] 

# define collapse function
collapse <- function(x) {
  my_string <- ''
  for(i in 1:length(x)) paste(my_string, x, sep = "")
}

# microbenchmark
a=microbenchmark(
  loop10  = collapse(letters10),
  loop100 = collapse(letters100),
  vec10   = paste(letters10, collapse = ""),
  vec100  = paste(letters100, collapse = ""),
  unit = "us"
)


# load package
library(compiler)
library(microbenchmark)

# compile
collapse_c <- cmpfun(collapse)

# microbenchmark
microbenchmark(
  loop10  = collapse(letters10),
  loop100 = collapse(letters100),
  loop_c10  = collapse_c(letters10),
  loop_c100 = collapse_c(letters100)
)

```{r, warning=F, message=F, eval = T}
# read data
f1 = function(n, x=1) for (i in 1:n) x <- 1 / (1 + x)
f2 = function(n, x=1) for (i in 1:n) x <- (1 / (1 + x))
c1 = cmpfun(f1)
c2 = cmpfun(f2)

# microbenchmark
microbenchmark(f1(1e5),f2(1e5),c1(1e5),c2(1e5))
```


lapply2 <- function(x, f, ...) {
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], ...)
  }
  out
}


my_vec <- function(x) {
  res <- numeric(length(x))
  for (i in 1:length(x)) {
    res[i] <- mean(x[[i]])
    }
  }


my_fun <- function(x, f, ...) {
  for (i in seq_along(x)) f(x[[i]], ...)
  }

my_fun_c <- compiler::cmpfun(lapply2)

x <- list(1:10, letters, c(F, T), NULL)
microbenchmark(
  my_fun(x, is.null),
  my_fun_c(x, is.null),
  )


my_vec_c <- cmpfun(my_vec)

#x <- list(1:10, letters, c(F, T), NULL)
x <- data.frame(rnorm(1000),rnorm(1000),rnorm(1000))

microbenchmark(
  my_vec(x),
  my_vec_c(x))





```{r, warning=F, message=F, eval = T}
# create data
my_data <- matrix(rnorm(10000), ncol = 10)

my_apply <- function(x){
  res <- numeric(length(x))
  for(i in 1:ncol(x)){
    res[i] = mean(x[,i])
  }
}

my_apply_c <- cmpfun(my_apply)

# microbenchmark
microbenchmark(
  colMeans = colMeans(my_data), 
  apply = apply(my_data, 2, mean),
  my_apply = my_apply(my_data),
  my_apply_c = my_apply_c(my_data),
  times = 1000)





microbenchmark(
  lapply2(x, is.null),
  lapply2_c(x, is.null),
  lapply(x, is.null)
)


require(parallel)


# define data
my_data <- matrix(rnorm(10000000),ncol = 10)
my_split_data <- lapply(1:10, function(i) data[(1:1000) + (i - 1) * 1000, ])

# my jobs 
clu <- makeCluster(5)

# my cluster fun
my_cluster_fun <- function(my_split_data){

  # apply cluster function
  out <- clusterApplyLB(clu, my_split_data, colMeans)
  
  # combine results
  colMeans(do.call(rbind, out))
  }

# microbenchmark
microbenchmark(vectorz = colMeans(my_data),
               cluster = my_cluster_fun(my_split_data))



# define data
my_data <- matrix(rnorm(10000000),ncol = 10)
my_split_data <- lapply(1:10, function(i) data[(1:1000) + (i - 1) * 1000, ])

# my jobs 




require(Rcpp)

#my_data <- matrix(rnorm(10000000),ncol = 10)

fun = "
NumericVector colMeans_c(NumericMatrix x){
  NumericVector out(nc);
  for(int j = 0; j < x.ncol(); ++j){
    double m = 0;
    for(int i = 0; i < x.nrow(); ++i) m += x(i, j);
  out[j] = m / x.nrow(); 
  }
return out;
}
"

cppFunction(fun)


microbenchmark(vectorz = colMeans(my_data),
               Rcpp = colMeans_c(my_data))





clu <- makeCluster(5)

# my cluster fun
my_cluster_fun <- function(my_split_data){
  
  # apply cluster function
  out <- clusterApplyLB(clu, my_split_data, colMeans)
  
  # combine results
  colMeans(do.call(rbind, out))
}

# microbenchmark
microbenchmark(vectorz = colMeans(my_data),
               cluster = my_cluster_fun(my_split_data))





# define function
my_Rcpp_fun = "
NumericVector colMeans_c(NumericMatrix x){
NumericVector out(nc);
for(int j = 0; j < x.ncol(); ++j){
double m = 0;
for(int i = 0; i < x.nrow(); ++i) m += x(i, j);
out[j] = m / x.nrow(); 
}
return out;
}"

# compile function
require(Rcpp) ; cppFunction(my_Rcpp_fun)

