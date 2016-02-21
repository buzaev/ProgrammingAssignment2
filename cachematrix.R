## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
         x <<- y
         inv <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
 }
  
## Write a short comment describing this function
 cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
     if(!is.null(inv)) {
         message("data is from cache")
         return(inv)
     }
     data <- x$get()
     inv <- solve(data)
     x$setinverse(inv)
     inv
 }

## x<-c(123,1124,1352,43535,346,3,5,6,5)
## m<-matrix(x,3,3)
## n=makeCacheMatrix(m)
## n$get()
## cacheSolve(n)
##              [,1]          [,2]          [,3]
##[1,]  1.609360e-05 -2.046106e-03  2.439233e-03
##[2,]  2.342596e-05 -5.776587e-05  4.589308e-05
##[3,] -4.365765e-03  5.533016e-01 -4.595962e-01
## cacheSolve(n)
##data is from cache
##              [,1]          [,2]          [,3]
##[1,]  1.609360e-05 -2.046106e-03  2.439233e-03
##[2,]  2.342596e-05 -5.776587e-05  4.589308e-05
##[3,] -4.365765e-03  5.533016e-01 -4.595962e-01
 