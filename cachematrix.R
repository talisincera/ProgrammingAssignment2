
# This function creates a special "matrix" object which is really a list containing a function to:

# 1)set the value of the matrix (set_matrix)

# 2)get the value of the matrix (get_matrix)

# 3)set the value of the inverse (set_inverse)

# 4)get the value of the inverse (get_inverse)

makeCacheMatrix <- function(x = matrix()) {
  ##  returns a list of functions to set and get a matrix input
  ##  and set and get its inverse.
  
  inverse_matrix <- NULL
  set_matrix <- function(y) {
    # the double arrow (scoping) assignment is used to change the global value of x and inv to what is defined in here.
    x <<- y
    inverse_matrix <<- NULL
  }
  
  get_matrix <- function() {x}
  set_inverse <- function(inverse) {inverse_matrix <<- inverse}
  get_inverse <- function() {inverse_matrix}
  
  list(set = set_matrix,
       get = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
}


## cacheSolve takes makeCacheMatrix as an input, gets the inverse if it is
## already computed and computes if Otherwise,
## and passes the inverse to setInverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## gets the inverse of the matrix from makecacheMatrix
  inverse <- x$get_inverse()
  
  # if the inverse is not NULL i.e it has already been calculated
  if (!is.null(inverse)) {
    # get it from the cache and skip the computation
    message("get cached data")
    return(inverse)
  }
  
  # else, compute the inverse of the matrix retrieved by get()
  data <- x$get_matrix()
  inverse <- solve(data, ...)
  
  # sets the computed inverse to set_inverse function
  x$set_inverse(inverse)
  
  inverse
}
