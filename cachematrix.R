# Below are two functions that are used to create a special object that 
# stores a inversible matrix and caches its inverse. That way if the inverse of a 
# matrix has already been calculated then the inverse can be looked up in the cache 
# rather than recomputed

# This function creates a special "matrix" object and returns a list of 
# functions that can: set the value of the matrix, get the value of the matrix,
# set the value of the inverse of the matrix and get the value of the inverse of
# the matrix.

makeCacheMatrix <- function(X = matrix()) {
                    i <- NULL
                    set <- function(y) {
                      X <<- y
                      i <<- NULL
                    }
                    get <- function() X
                    setinv <- function(solve) i <<- solve
                    getinv <- function() i
                    list(set = set, get = get,
                         setinv = setinv,
                         getinv = getinv)
}


# The following function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the `setinv`
# function

cacheSolve <- function(X, ...) {
              i <- X$getinv()
              if(!is.null(i)) {
                message("getting cached data")
                return(i)
              }
              data <- X$get()
              i <- solve(data, ...)
              X$setinv(i)
              i
}
