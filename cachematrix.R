## makeCacheMatrix returns a list with four functions used to:
## 1. retrieve the value of a matrix
## 2. update the value of a matrix
## 3. retrieve the value of a matrix's inverse
## 4. set the value of a matrix's inverse
makeCacheMatrix <- function(m = matrix()) {
    inverse <- NULL ## the inverse matrix is set to NULL initially
    set <- function(x) { ## assigns a new value to the matrix and resets the value of the inverse to NULL
        m <<- x
        inverse <<- NULL
    }
    get <- function() m ## returns the value of the matrix
    setinverse <- function(solve) inverse <<- solve ## solves for the value of the inverse matrix
    getinverse <- function() inverse  ## returns the value of the inverse matrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## returns a list with the 4 functions defined above
}


## cacheSolve checks whether the inverse of a matrix has been cached previously and returns the cached inverse matrix if it exists
## If the inverse of the matrix has not been cached previously, cacheSolve solves for the inverse, caches the result and returns the newly solved inverse matrix
cacheSolve <- function(m, ...) {
    inverse <- m$getinverse() ## get the existing value of the inverse matrix
    if(!is.null(inverse)) { ## if a the inverse matrix has been cached (it isn't NULL), then the cached inverse matrix is returned
        message("getting cached data")
        return(inverse)
    }
    ## if the inverse matrix has not been cached (it is NULL), then the remainder of the function is executed
    data <- m$get() ## retrieve the data for the matrix
    inverse <- solve(data, ...) ## solve for the inverse matrix
    m$setinverse(inverse) ## cache the inverse matrix
    inverse ## return the inverse matrix
}
