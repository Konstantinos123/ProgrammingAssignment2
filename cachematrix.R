## Matrix inversion is usually a costly computation and there
## may be some benefit to caching the inverse of a matrix.
## Below there are two functions: 1) makeCacheMatrix, 2) cacheSolve.

## Firstly, the 'makeCacheMatrix' creates a special ''matrix'' object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NUL
        set <- function(y) {
                x <<- y
                inv <<- NULL             
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv 
        list( set =set,
              get =get, 
              setInverse = setInverse,
              getInverse = getInverse)
}

## Secondly, the 'cacheSolve' function computes the inverse of the special
## matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message(''getting cached data'')
                return(inv)        
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}        
