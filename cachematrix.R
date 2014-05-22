## Functions to create a modifyable data structure to hold a matrix and its
## inverse (makeCacheMatrix) as well as to calculate the inverse and store it in
## the data structure (cacheSolve).



## Function to store the values of a matrix as well as it's inverse

makeCacheMatrix <- function(x = matrix()) {
  x.inverse <- NULL
  ## Function to set the value of the matrix x
  set <- function(m) {
    x <<- m
    x.inverse <- NULL # re-initialize inverse matrix
  }
  ## Function to return the value of the matrix x
  get <- function() {
    return(x)
  }
  ## Function to set the value of the inverse matrix of x
  setInverse <- function(m.inverse) {
    x.inverse <<- m.inverse
  }
  ## Function to get the value of the inverse matrix of x
  getInverse <- function() {
    return(x.inverse)
  }
  ## Subset functions in a list to be returned
  functions <- list(set=set,
                    get=get,
                    setInverse=setInverse,
                    getInverse=getInverse)
  ## Return functions
  return(functions)
}



## Function to return the inverse matrix of a matrix created using the function
## makeCacheMatrix. The inverse matrix is retrieved from cache if possible,
## otherwise it's calculated and stored to cache.

cacheSolve <- function(x, ...) {
  m.inverse <- x$getInverse()
  ## Return the cached inverse matrix if possible
  if(!is.null(m.inverse)) {
    return(m.inverse)
  }
  ## Calculate inverse matrix, cache it and return it
  m <- x$get()
  m.inverse <- solve(m)
  x$setInverse(m.inverse)
  return(m.inverse)
}
