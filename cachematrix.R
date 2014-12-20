#Programming Assignment 2: Lexical Scoping

#These functions caches the inverse of a matrix.

#This first function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function (x = matrix()) {
    m <- as.null(matrix())
    set <- function(y = matrix()) {
        x <<- y
        m <<- as.null(matrix())
    }
    get <- function() x
    setinverse <- function(solve) m <<- mean
    getinverse <- function() m
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    data <- as.matrix(data)
    m <- solve(data, ...)
    x$setinverse(m)
    as.matrix(m)
}
