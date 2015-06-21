## makeCacheMatrix() create a storage for a inverse-cacheable matrix
## cacheSolve() calculate and cache the inverse of a matrix storage created by makeCacheMatrix()

## makeCacheMatrix creates a holder for a inverse-cacheable matrix
## Use makeCacheMatrix$get() to get the matrix stored inside
## Use makeCacheMatrix$set(matrix) to set the matrix stored inside
## Use makeCacheMatrix$getInverse to get the cached inverse matrix stored inside
## Use makeCacheMatrix$setInverse to set the cached inverse matrix stored inside
## Warning: this function only create the storage, and do not do any matrix inverse calculation
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list (set = set, get = get,
              setInverse = setInverse,
              getInverse = getInverse)
}

## Calculate the inverse of a matrix created by makeCacheMatrix
## If the inverse has been calculated before, then the result would be cached and return in following function calls
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("Returning cached data!")
                return(inv)
        }
        
        mtx <- x$get()
        inv <- solve(mtx, ...)
        x$setInverse(inv)
        inv
}
