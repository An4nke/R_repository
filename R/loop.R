for (i in 1:5) { # i iterator
  print(i)
}
# for loops are slow & not memory efficent
for (i in 1:5) {
  print (i^2)
}

for (i in c("Hello", "World")) {
  print (i)
}

# extract + printing names from diamonds dataset
for (name in names(diamonds)) print(name) # here {} not needed
for (name in names(diamonds)) {
  print(name)
}

# modify to calculate mean
for (name in names(diamonds)) {
  diamonds
  print(name)
}

# transformate dataset into regular dataset
diamonds_df = as.data.frame(diamonds)

for (name in names(diamonds)){
  print (mean(diamonds_df[, name]))
} 

# reset diamands data set
diamands = ggplot2::diamonds

## subset diamonds to only numerical columns
#diamonds_num <- diamonds[, -c(2:4)]

## apply function mean to all columns of diamonds
col_means <- apply(diamonds_num, 2, mean, na.rm = TRUE)

# calculate standarddeviation
col_div <- apply(diamonds_num, MARGIN = 2, FUN = sd, na.rm = TRUE)

result <- lapply(1:5, function(i) i) # get back list
result <- sapply(1:5, function(i) i) # simplify output

# alternative using for loop
out = vector("list", 5)
for (i in 1:5) {
  out[[i]] = i
}

fls <- list.files("results", pattern = glob2rx("*subset*.csv"),
                  full.names = TRUE)

dat_lst <- lapply(seq(fls), function(i) {
  read.csv(fls[i])
})

str(dat_lst, 1)

diamands_df = do.call("rbind", data_list) # concatenate lists of dataset

sapply(my_result_list, function(i)) {
  summary(p[[i]])$r.squared
}

p = sapply(my_result_list, "[[", 2), # extracting 2. element of list
print(p[[5]]) 