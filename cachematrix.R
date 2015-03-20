## 1.  `makeCacheMatrix`: This function creates a special "matrix" object
##      that can cache its inverse.
## 2.  `cacheSolve`: This function computes the inverse of the special
##     "matrix" returned by `makeCacheMatrix` above. If the inverse has
##      already been calculated (and the matrix has not changed), then
##     `cacheSolve` should retrieve the inverse from the cache.

## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to
##
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the inverse of the matrix
## 4.  get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of the inverse in the cache via the `setinverse`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  inverseMatrix <- x$get()
  inverse <- solve(inverseMatrix, ...)
  x$setinverse(inverse)
  inverse
}
