## These functions implement an object model which combine a matrix and its inverse. 
## The inverse, a relatively expensive operation, will be performed only on first retrieval,
## and cached in the object for subsequent requests.

## Sample use:
##   mat <- makeCacheMatrix(matrix (c(4,3,3,2), nrow=2))
##
##   mat$get()
##          [,1] [,2]
##     [1,]    4    3
##     [2,]    3    2
##
##   cacheSolve(mat)
##          [,1] [,2]
##     [1,]   -2    3
##     [2,]    3   -4

## List-based object model for a matrix and its (cached) inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  set <- function(y) {
      x <<- y
      inv <<- NULL
  }

  get <- function() x
            
  setInverse <- function(inverse) inv <<- inverse

  getInverse <- function() inv

  list(set        = set, 
       get        = get,
       setInverse = setInverse,
       getInverse = getInverse
  )
            
}


## Function to retrieve inverse of the supplied matrix from the above object model

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

    ## check cache and return cached inverse if set
    inv <- x$getInverse()

    if(!is.null(inv)) {
        message("getting cached data")

        ## Return the inverse
        return(inv)
    }

    ## otherwise, calculate inverse, update cache and return the inverse
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)

    ## Return the inverse
    inv
}
