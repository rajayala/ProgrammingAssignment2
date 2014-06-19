
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # Everytime the "set" function is called, it is assumed that new matrix
  # data needs to be initialized and therefore inverse is set to NULL
  set <- function(y) {
    inv <<- NULL
    x <<- y
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

## It doesn't do any error checking.. assumes that the input matrix is a valid
## square matrix and that it is invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(is.null(inv)) message("No precomputed cached Inverse .. first call")
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}

# test script
m <- makeCacheMatrix( )

# sample test matrix

tm <- matrix(c(0,0,2,0,2,0,2,0,0),3,3)

# initialize cache to test matrix
m$set(tm)

# see if the test matricx can be retrieved correctly
m$get()

#test the inverse cacher
cacheSolve(m)

# calling second time.. should give cached result
cacheSolve(m)

# verify test matrix and its inverse multiply to identity matrix
m$get() %*% cacheSolve(m)

# change matrix data and call cacheSolve... should compute inverse freshly
m$set( matrix( rnorm(4), 2, 2) )
cacheSolve(m)

# trying to compute inverse again should return cached inverse
cacheSolve(m)


