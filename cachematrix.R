## Caching the inverse of a matrix instead of computing repeatly

## Invert a matrix then cache it

makeCacheMatrix <- function(x = matrix()) {
        invert <- NULL
        set <- function(y) {
                x <<- y
                invert <<- NULL
        }
        get <- function() x
        setinvert <- function(i) invert <<- i
        getinvert <- function() solve(x)
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)
}


## Check if the inverse is null, if so, re-inverse it then return

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