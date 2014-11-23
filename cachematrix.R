## These functions allow a matrix and its inverse to be cached into memory for later use 
##  without recalculating its value.

## The makeCacheMatrix function creates a vector of set and get functions to store and 
##  access a matrix and its inverse
makeCacheMatrix <- function(x = matrix())
{
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inv_x <<- inv
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The cacheSolve function either returns an already cahced inverse of the matrix or
##  calculates the inverse of the matrix using solve and then stores the value in
##  the cache.
cacheSolve <- function(x, ...)
{
  inv_x <- x$getinverse()
  if(!is.null(inv_x)) {
    message("getting cached data")
    return(inv_x)
  }
  data <- x$get()
  inv_x <- solve(data, ...)
  x$setinverse(inv_x)
  inv_x
}
