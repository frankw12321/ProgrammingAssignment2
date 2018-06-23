## Caching the inverse of a matrix instead of computing repeatly

## Make a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invert <- NULL
        set <- function(y) {
                x <<- y
                invert <<- NULL
        }
        get <- function() x
        setinvert <- function(i) invert <<- i
        getinvert <- function() invert
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)
}


## Check if there is no inverse before, if so, re-inverse and cache it then return

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invert <- x$getinvert()
        if(!is.null(invert)) {
                message("getting cached data")
                return(invert)
        }
        data <- x$get()
        invert <- solve(data)
        x$setinvert(invert)
        invert
}