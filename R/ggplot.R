library(ggplot2)

# create scatterplot
ggplot(aes(x = carat, y = price), data = diamonds) + 
  geom_point() +
  # regretion line
  stat_smooth(method = "lm", se = FALSE, 
              colour = "red", size = 1.5, linetype = "dashed") +
  facet_wrap(~ cut) + # split up data
  labs(x = "Carat", y = "Price ($)") + # add axis title
  theme_minimal() # use other color sheme

# create boxplot
p <- ggplot(aes(x = color, y = price), data = diamonds)

## add boxplot
#p_bw <- p + geom_boxplot()

p_bw <- p + 
  geom_boxplot(fill = "grey90") + # alpha : transparency lvl
  facet_wrap(~ cut) +
  theme_bw()

print(p_bw)

# histogramm: p_hist <- p + geom_histogram()
