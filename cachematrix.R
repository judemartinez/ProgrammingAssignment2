# makeCacheMatrix is a function that creates and returns a special "matrix" object that can cache its inverse.
# Additional mutator and accessor methods are created:
# setMatrix      initialize the matrix
# getMatrix      returns the matrix and its contents
# cacheInverse   save the data in the cache (inverse of the matrix)
# getInverse     return the cache value (inverse of the matrix)
makeCacheMatrix <- function(x = matrix()) {
  # variable that stores the cached value
  # set to NULL since no cached values yet on the first invocation of the method
  cacheValue <- NULL
  # create a matrix
  setMatrix <- function(newMatrix) {
    x <<- newMatrix
    # since the matrix is assigned a new value, we need to clear the cache
    cacheValue <<- NULL
  }
  # returns the stored matrix
  getMatrix <- function() {
    x
  }
  # save the data passed to the method in cacheValue 
  cacheInverse <- function(dataForCache) {
    cacheValue <<- dataForCache
  }
  # method that returns the cacheValue
  getInverse <- function() {
    cacheValue
  }
  # create a list
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  # get the cached value
  inverse <- x$getInverse()
  # if a cached value exists return it
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  # else retrieve the matrix
  #calculate the inverse using solve() method and store it in the cache
  data <- x$getMatrix()
  inverse <- solve(data)
  x$cacheInverse(inverse)
  ## Return inverse of the matrix
  inverse
}