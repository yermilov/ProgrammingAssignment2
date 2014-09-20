## makeCacheMatrix
## ===============
## Construct structure that should be used in cacheSolve function for cached 
## calculation of matrix inverse.
## Input:
## x - matrix, for which inversion is needed.
## Return:
## Function client should not work with returned object, just pass it 
## to cacheSolve function.
## Example:
## x <- matrix(1:4, 2, 2)
## y <- makeCacheMatrix(x)
## z <- cacheSolve(y)

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  get <- function() x
  cache <- function(ninverse) inverse <<- ninverse
  cached <- function() inverse
  list(get = get, cache = cache, cached = cached)
}


## cacheSolve
## ===============
## Performs cached calculation of matrix inverse.
## Input:
## x - special object, constructed on base of need matrix via makeCacheMatrix
## method.
## Return:
## Invertion of matrix, encapsulated in passed object.
## Example:
## x <- matrix(1:4, 2, 2)
## y <- makeCacheMatrix(x)
## z <- cacheSolve(y)

cacheSolve <- function(x, ...) {
  inverse <- x$cached()
  if(!is.null(inverse)) {
    message("result retrieved from cache")
    return(inverse)
  }
  
  message("calculated result")
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$cache(inverse)
  inverse
}
