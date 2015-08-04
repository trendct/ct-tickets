install.packages("truncnorm")
library(truncnorm)
x <- rtruncnorm(n=200, a=0, b=500)

correlatedValue = function(x, r){
  r2 = r**2
  ve = 1-r2
  SD = sqrt(ve)
  e  = rnorm(length(x), mean=0, sd=SD)
  y  = r*x + e
  return(y)
}

corr_list <- c(-1, -.9, -.8, -.7, -.6, -.5, -.4, -.3, -.2, -.1, 0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1)

for (i in corr_list_list) {
  filename <- paste("corr_example_", i, ".png", sep="")
  interval <- corr_list[i,1]
  y <- correlatedValue(x=x, r=interval)
  df_stuff <- data.frame(x,y)
  p <- ggplot(df_stuff, aes(x, y))
  p + geom_point() + theme_minimal() + ylab("") + xlab("")
  
  ggsave(filename, width=6, height=6, dpi=100)
  
}
