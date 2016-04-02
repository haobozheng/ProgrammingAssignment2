## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  mat_inv <- NULL
  
  set <- function(y) {
    x<<-y
    mat_inv <<- NULL
  }
  
  get <- function() x
  setInv <- function(z) mat_inv <<- z
  getInv <- function() mat_inv
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}




## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  mat_inv <- x$getInv()
  if (!is.null(mat_inv)) {
    message("getting cached data")
    return(mat_inv)
  }
  
  mat <- x$get()
  
  ## solve() function solves equation a %*% x = b for x, where b is a vector or matrix 
  ## If b is absent, the default is a unit matrix
  mat_inv <- solve(mat, ...)
  x$setInv(mat_inv)
  mat_inv
  
}

