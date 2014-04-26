## makeCacheMatrix is a function that creates a matrix object that can cache 
## its inverse.  the output of the function is list of 4 function that serves 
## as the input of the function cachesolve below 

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) mi <<- solve
        getinverse <- function() mi
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve is a function that solves the inverse of a matrix
## or retrieves the results of a previously solved matrix inverse.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mi <- x$getinverse()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data, ...)
        x$setinverse(mi)
        mi

}

