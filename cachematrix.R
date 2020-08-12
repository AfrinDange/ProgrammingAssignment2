## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invmat <- NULL
    set <- function(y){
      x <<- y
      invmat <<- NULL
    }
    get <- function() x
    setInv <- function(inv) invmat <<- inv
    getInv <- function() invmat
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invmat <- x$getInv()
    if(!is.null(invmat)) {
      message("getting cached data")
      return(invmat)
    }
    mat <- x$get()
    invmat <- solve(mat, ...)
    x$setInv(invmat)
    invmat
}
