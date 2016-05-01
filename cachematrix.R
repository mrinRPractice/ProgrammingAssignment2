## makeCacheMatrix and cacheSolve functions will cache the inverse
##of an matrix. If the inverse of the same matrix is required the
##cached inverse will be retrieved instead of computing the inverse.

## makeCacheMatrix caches the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() {
                x
        }
        setInverse <- function(inv) {
                inverse <<- inv
        }
        getInverse <- function() {
                inverse
        }
        list(
                set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse
        )
}


##cacheSolve returns the inverse of a matrix from cache if 
##it exists, else computes the inverse, stores it in cache 
##and then returns the  inverse

cacheSolve <- function(x, ...) {
        invMat <- x$getInverse()
        
        if (!is.null(invMat)) {
                message("Fetching Inverse from Cache")
                return(invMat)
        }
        mat <- x$get()
        x$setInverse(solve(mat, ...))
        x$getInverse()
}
