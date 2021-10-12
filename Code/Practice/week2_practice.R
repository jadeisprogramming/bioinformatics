##create a vector of 100 random numbers between 0 and 50
x <- runif(n=100, min=0, max=50)
##sort these by order of their value(from largest to smallest)
sort_x <- sort(x, decreasing = TRUE)
