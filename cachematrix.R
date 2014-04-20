
## makeCacheMatrix: convert a matrix to an object with cached inverse
#  the cached matrix object has the following methods
#   cx <- makeCacheMatrix( x )  # Construct x to an object with a cache
#   cx$get()			# return the matrix
#   cx$set(x)			# Put a different matrix into the object
#   cx$getinverse()		# Return the cache of the inverse
#   cx$setinverse()		# Put the inverse matrix into the cache
#
# Note:  The cache is only set when cx$setinverse() is called
#    cx$set(x) will clear the cache as a side-effect

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
# Returns the inverse for the matrix in the cached matrix objec
# If the cache is not already set calculate it and save it
# Otherwise return the value directly from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
                #message("solving the matrix")
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## Incidentally, IMHO it would be better to merge the functionality of
# cacheSolve() into cx$getinverse() - but I do what I am told!
