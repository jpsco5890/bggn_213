x <- 1:50
plot(x, sin(x))
?plot
plot(x, sin(x), 
     type = "l", 
     col = "blue",
     lwd = 3,
     xlab = "A vector named x")