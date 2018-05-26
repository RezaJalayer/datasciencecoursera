## This is to compute the inverse of a matrix efficiently. 
## The efficiency comes from calculating the inverse only once until the matrix is changed 

## This function gets a matrix and returns a list of 4 functions that will be used in the next function for inverse calculation

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function returns the matrix inverse by either returning the previuosly calculated inverse (no calculation) or calculating the inverse  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
