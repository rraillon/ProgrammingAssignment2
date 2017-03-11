makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatrixCache <- function(matrixToCache) m <<- matrixToCache
        getMatrixCache <- function() m
        list(set = set, get = get,
             setMatrixCache = setMatrixCache,
             getMatrixCache = getMatrixCache)
}

cacheSolve <- function(x, ...) {
        m <- x$getMatrixCache()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        if (is.matrix(data)) {
                m <- solve(data, ...)
                x$setMatrixCache(m)
        }
        m
}

MatrixToInverse <- matrix(c(1,2,3,4), nrow=2, ncol=2)
CachedMatrix <- makeCacheMatrix(MatrixToInverse)
print(cacheSolve(CachedMatrix))
