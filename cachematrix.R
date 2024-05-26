## Programming Assignment 2: Lexical Scoping

# In this programming assignment, we will be implementing a set of functions that demonstrate the use of lexical scoping in R.
# We will create a pair of functions that cache the inverse of a matrix to avoid unnecessary recomputation. 
# This is commonly to as the "Caching the Inverse of a Matrix" problem.
# 
# Here's how to we will approach this assignment:
# 
# Make a Cache Matrix: This function creates a special "matrix" object that can cache its inverse.
# Cache Solve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

## Step 1: Create makeCacheMatrix Function
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Step 2: Create cacheSolve Function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

## Step 3: Running the code

# Define a matrix
my_matrix <- matrix(c(1, 2, 3, 4), 2, 2)

# Create a cache matrix
cache_matrix <- makeCacheMatrix(my_matrix)

# Compute and cache the inverse
inverse_matrix <- cacheSolve(cache_matrix)
print(inverse_matrix)

# Retrieve the cached inverse
cached_inverse_matrix <- cacheSolve(cache_matrix)
print(cached_inverse_matrix)
