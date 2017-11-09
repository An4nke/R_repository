## load 'Rcpp' package
library(Rcpp)

## try to evaluate c++ expression
evalCpp("1 + 1")

## 2 way
cppFunction()
sourceCpp()

## function that returns '1'
cppFunction('int one() {
  return 1;
}')

## function that returns 1 if 'x' is positive, -1 if 'x' is negative, 0 otherwise
cppFunction('int signC(int x) {
  if (x > 0) {
    return 1;
  } else if (x == 0) {
    return 0;
  } else {
    return -1;
  }
}')

# function for calculating sum of vector
cppFunction('double sumC(NumericVector x) {
  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total;
}')

# sum function in R
sumR <- function (x) {
  su = 0
  for (i in x) {
    su = su + i
  }
  return (su)
}

sumR(seq(1, 3, 4))

## subset with numeric columns only
num_cols <- sapply(1:ncol(diamonds), function(i) {
  is.numeric(data.frame(diamonds)[, i])
})
diamonds_sub <- as.matrix(diamonds[, num_cols])

## c++-version of 'colMeans'
cppFunction("NumericVector colMeansC(NumericMatrix x) {
            
            // number of rows and columns
            int nCol = x.ncol();
            int nRow = x.nrow();
            
            // temporary variable of size nrow(x) to store column values in
            NumericVector nVal(nRow);
            
            // initialize output vector
            NumericVector out(nCol);
            
            // loop over each column
            for (int i = 0; i < nCol; i++) {
            
            // values in current column
            nVal = x(_, i);
            
            // store mean of current 'nVal' in 'out[i]'
            out[i] = mean(nVal);
            }
            
            return out;
            }")



# use function
one()
signC(-11)
sumC(seq(0, 1, 0.1))
sum(seq(0, 1, 0.1))
means <- colMeansC(diamonds_sub)
names(means) <- colnames(diamonds_sub)
means

# load C++ function from other files # using namespace Rcpp;
## source 'corC' function (remember to adjust the path)
sourceCpp("src/corC.cpp")