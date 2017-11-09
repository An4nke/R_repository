library(ggplot2)
class(diamonds)

# get only entrys of high quality diamands
#good<-subset(diamonds, color == c("D", "E", "F"), cut == c("Premium", "high"), carat => 3)

# color, price, carat

# sort best color for the lowest prices

good<-subset(diamonds, select = c("carat", "color","price"))
good_carat<-good[order(good$carat, decreasing = TRUE), ]
good_carat[order(good_carat$carat, decreasing = TRUE) &&  order(good_carat$price, decreasing = FALSE),]

#sort(good, price = decreasing)

#aggregatge(diamands$price, by = list(diamonds$color), FUN = function(x)) {
#  c(MIN = min(x), MEAN = mean(x), MAX = max(x))
#}

# melt diamonds dataset -> new columns id 
# melt()

library(reshape2) # package for big datasets
diamonds$ID = rownames(diamonds)
diamonds_melt<-melt(diamonds, id.vars = c("ID", "cut", "color", "clarity"))
diamonds_melt
