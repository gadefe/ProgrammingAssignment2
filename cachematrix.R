## Put comments here that give an overall description of what your
## functions do makeCacheMatrix that cache the inverse of a matrix

## Write a short comment describing this function
## makeCacheMatrix is a function which create a matrix of object which is an invertible square matrix

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
        x <<- y
        inv <<- NULL
  }
   get <- function() x
   setinv <- function(inverse) inv <<- inverse
   getinv <- function() inv
   list(set = set, get = get, setinv = setinv, getinv = getinv)
}	


## Write a short comment describing this function
## cacheSolver is a function which computes the inverse of the matrix returned by the makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'	       
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}	

## ---------------Testing the program------------------------
##> t <- matrix(rnorm(16),4,4)
##> t1 <- makeCacheMatrix(t)
##> View(makeCacheMatrix)
##> cacheSolve(t1)
##            [,1]       [,2]       [,3]       [,4]
##[1,]  0.28375843  0.5995336 -6.3369766 -1.6356561
##[2,]  0.25618105  0.7797807  1.2476118  0.5974809
##[3,]  0.06859356 -0.1610413 -0.5762348  0.5954099
##[4,] -0.71607611 -0.5861204  4.5470605  0.7920920


