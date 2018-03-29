# installing and/or loading packages

packages <- c("MASS", "ccaPP", "lattice")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = T)) install.packages(x)
  if (! (x %in% (.packages() )))  library(x, character.only = T)
})

# generate x's and y's
x <- seq(-5, 5, 0.5)
y <- seq(-5, 5, 0.5)

# Pearson Correlation Influence Function
IFP <- function(x,y){
  x*y - 0.5*(x^2 + y^2)/2
}

# Kendall Correlation Influence Function
IFK <- function(x,y){
  2*(4*0.5 - 2*x - 2*y + 1 - corKendall(x, y)) #rho = 0.5
}

# Spearman Correlation Influence Function
IFS <- function(x,y){
  -3*corSpearman(x,y) - 9 +
    12 * (x*y + ) #.....
  #rho = 0.5
}

# M Association Measure Influence Function
IFM <- function(x,y){
  #???
}

#ploting the Influence Function
plotIF <- function(x,y,IF){
  z <- outer(x,y,IF)
  persp(x, y, z, ticktype = "detailed", theta = 40, phi = 30)
}

#plots
plotIF(x,y,IFP)
plotIF(x,y,IFK)
plotIF(x,y,IFS)
plotIF(x,y,IFM)


# Wrong first try ---------------------------------------------------------

# C <- matrix(c(1,0.5,0.5,1),2,2)
# phi <- mvrnorm(n = 100, rep(0, 2), C)
# x <- phi[,1]
# y <- phi[,2]
# 
# # define a bivariate normal distribution with variances of 1
# # for each x and y.
# # http://mathworld.wolfram.com/BivariateNormalDistribution.html
# 
# IF <- function(x,y,rho){
#   z <- (x-mean(x))^2 - 2*rho*(x-mean(x))*(y-mean(y)) + (y-mean(y))^2
#   f <- exp(-(z/(2*(1-rho^2)))) / 2*pi*(1-rho^2)^(1/2)
#   f_x <- exp((-(x-mean(x))^2)/2) / (2*pi)^(1/2)
#   f_y <- exp((-(y-mean(y))^2)/2) / (2*pi)^(1/2)
#   return(
#     2*(4*var(f) - 2 * f_x - 2 * f_y + 1 - corKendall(f_x, f_y))
#     )
# }
# 
# IF(x,y,0.5)
# x <- sort(x)
# y <- sort(y)
# z <- IF(x,y,0.5)
# i <- matrix(c(x,y,z),100,3)
# 
# persp(i)
