## The function stores a given matrix, calculates the inverse of the matrix and stores it in a cache

## The function, makeCacheMatrix creates a matrix. It mainly does the following:
## 1) set the value of matrix
## 2) get the value of matrix
## 3) set the inverse value of the matrix
## 4) get the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatrixInverse <- function(solve) m <<- solve
        getMatrixInverse <- function() m
        list(set = set, get = get,
             setMatrixInverse = setMatrixInverse,
             getMatrixInverse = getMatrixInverse)
}


## This function calculates the inverse of the matrix. 
## It first checks to see if the inverse already exists.  
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setMatrixInverse function.
cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getMatrixInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setMatrixInverse(m)
        m
}
