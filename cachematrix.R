# This R program defines functions to create a special matrix object that can cache its inverse and compute the inverse of the matrix, caching the result for future use. 
# The program includes functions makeCacheMatrix to create the special matrix object, cacheSolve to compute and cache the inverse, and a test function to demonstrate the functionality.

# Function to create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(inverse) {
    cache <<- inverse
  }
  
  getInverse <- function() {
    cache
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Function to compute the inverse of the special matrix and cache the result
cacheSolve <- function(x, ...) {
  cache <- x$getInverse()
  
  if (!is.null(cache)) {
    message("Retrieving cached data")
    return(cache)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}

# Test function to demonstrate the functionality
test <- function() {
  base_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
  
  base_matrix$get()
  base_matrix$getInverse()
  
  cacheSolve(base_matrix)
  
  base_matrix$set(matrix(c(0, 5, 99, 66), nrow = 2, ncol = 2)) # Modify the existing matrix
  
  cacheSolve(base_matrix)   # Compute, cache, and return the new matrix inverse
  base_matrix$get()         # Return the matrix
  base_matrix$getInverse()  # Return the matrix inverse
  
  base_matrix$get() %*% base_matrix$getInverse() # Return the identity matrix
}

# Run the test function
test()
