


#
# Create a matrix object 
# @param      : matrix
# @author     : bertrand debroise
# @version    : 1.0
# @Created    : 03/17/2018

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  # @return the matrix
  get <- function() x
  
  # @set inverse matrix
  setInverse <- function(solve) inv_matrix <<- solve
  
  # @return inverse matrix or NULL if inverse matrix has not been computed
  getInverse <- function() inv_matrix
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}




#
# Cache computed matrix 
# @param      : object built with makeCacheMatrix
# @author     : bertrand debroise
# @version    : 1.0
# @Created    : 03/17/2018
# @Return	  : Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv_matrix <- x$getInverse()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data, ...)
  x$setInverse(inv_matrix)
  inv_matrix
}
