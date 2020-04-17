## makeCacheMatrix builds a set of functions set(),get(),setinverse(),
## getinverse(), and returns the functions within a list to the parent
## environment.  When the function is run, it creates a list 
## with the four objects and the two data objects x and inv.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## CacheSolve either calculates the inverse matrix of the x matrix
## defined in makeCacheMatrix, or retrieves cached data in case
## the inverse matrix of the same data has been already calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
