## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than 
## compute it repeatedly (there are also alternatives to 
## matrix inversion that we will not discuss here). 
## Assignment is to write a pair of functions that cache 
## the inverse of a matrix.

## ==========================================================

## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, 
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## ==========================================================
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  
}

x <- rbind(c(1, -.25), c(-.25, 1))
x
m <- makeCacheMatrix(x)
m
m$get()
cacheSolve(m)
