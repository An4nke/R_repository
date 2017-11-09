"<-"(b, 15) # b<-15

pythagoreanTheorem <- function(a, b) {
  c <- sqrt(a*a + b*b)
  return(c)
}

pythagoreanTheorem(3, 4)

# function for standarderror
standerr <- function (x, na.rm = TRUE) {
  # remove missing values
  if (na.rm) {
    ids = !is.na(x)
    x = x[ids]
  }
  sd(x, na.rm) / sqrt(length(x))
}

rmsd <- function (x) {
  m<-mean(x)
  # sqrt (sum(m - x(i))/length(x))
}

test<-c(4, 4, 4, 5)

standerr(test)
standerr(diamands[,9])