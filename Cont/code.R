my_func <- function(x,n) {
  temp2 <- 0
  for(i in 1:n) {
    temp <- x^(i-1) * 5
    print(temp)
    temp2 <- temp + temp2
  }
  return(temp2)
}

my_func(2,3)

x <-stepfun(1:3,2:5, f = 0.5)
summary(x)

y0 <- c(1., 2., 4., 3.)
sfun0  <- stepfun(1:3, y0, f = 0)
sfun.2 <- stepfun(1:3, y0, f = 0.2)
sfun1  <- stepfun(1:3, y0, f = 1)
sfun1c <- stepfun(1:3, y0, right = TRUE) # hence f=1
sfun0
summary(sfun0)
summary(sfun.2)

## look at the internal structure:
unclass(sfun0)
ls(envir = environment(sfun0))

x0 <- seq(0.5, 3.5, by = 0.25)
rbind(x = x0, f.f0 = sfun0(x0), f.f02 = sfun.2(x0),
      f.f1 = sfun1(x0), f.f1c = sfun1c(x0))
## Identities :


## new task

ss <- function(b) {
  temp2 <- 0
  for(i in 1:100) {
    temp <- b^i
    temp2 <- temp + temp2
  }
  return(temp2)
}
plot(ss,xlim=range(0,0.7))







# something else

x<- seq(-1,1, by = 0.001)

x_function <- function(x) x*2
fvalues<- x_function(x)

g <- function(g,z) g*2 + 1 - z

z <- 0

for(i in 1:length(fvalues)) {
  z[i] = uniroot(g, z = fvalues[i], lower = -3, upper= 3)
}
z2 <- unlist(z)

d <- data.frame(x = 2, y = 1:length(x))
d$x <- x
d$y <- z2
library(ggplot2)

ggplot(d, aes(x, y)) + geom_point()
