## These functions compute and store the inverse of a nxm matrix in order to
## have the result saved to avoid future calculations for the same matrix.

## The makeCacheMatrix function recieves a matrix which the user wants to store
## its values and its inverse. As it is stored for the first time it sets (by
## default) the inverse value (inv) as a "NULL" value so the next function can
## calculate and store the value itself.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The cacheSolve function recieve a "special matrix" provided by the
## makeCacheMatrix function as main argument. Internally it determines if the
## inverse has already been calculated. If so, it prints a message indicating
## the inverse is being recovered and prints out the inverse.
## If the inverse hasn't been calculated yet, the original matrix is recovered
## and its inverse is calculated throughout the solve() function, then, it is
## stored and finally printed in the console (if it is not being stored in a
## gloal environment variable).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
