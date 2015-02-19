## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns a list with four functions used to retrieve and update the value of a matrix as well as its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL ## the inverse matrix is set to NULL initially
    set <- function(y) { ## assigns a new value to the matrix and resets the value of the inverse to NULL
        x <<- y
        i <<- NULL
    }
    get <- function() x ## returns the value of the matrix
    setinverse <- function(solve) i <<- solve ## solves for the value of the inverse matrix
    getinverse <- function() i  ## returns the value of the inverse matrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## returns a list with the 4 functions defined above
}


## cacheSolve checks whether the inverse of a matrix has been cached previously and returns the cached inverse matrix if it exists
## If the inverse of the matrix has not been cached previously, cacheSolve solves for the inverse, caches the result and returns the newly solved inverse matrix
cacheSolve <- function(x, ...) {
    i <- x$getinverse() ## get the existing value of the inverse matrix
    if(!is.null(i)) { ## if a the inverse matrix has been cached (it isn't NULL), then the cached inverse matrix is returned
        message("getting cached data")
        return(i)
    }
    ## if the inverse matrix has not been cached (it is NULL), then the remainder of the function is executed
    data <- x$get() ## retrieve the data for the matrix
    i <- solve(data, ...) ## solve for the inverse matrix
    x$setinverse(i) ## cache the inverse matrix
    i ## return the inverse matrix
}
