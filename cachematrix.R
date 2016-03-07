## Put comments here that give an overall description of what your
## functions do
## These functions work with environment objects to cache data already processed.

## This function creates an array of functions to manipulate the objects in the current environment.

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


## This function checks if the inverse of the matrix is already cached. 
## Is not, it caches the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        matrix <- x$get()
        m <- solve(matrix)
        x$setinverse(m)
        m
}
