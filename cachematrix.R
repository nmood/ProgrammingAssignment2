## These two functions can create a special object that 
## stores a numeric matrix and cache its inverse

## makeCacheMarix: This Function creates a special matrix object that 
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## set value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## get value of the matrix
  get <- function() x
  
  ## set inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  ## get inverse of the matrix
  getInverse <- function() inv
  
  ## return a list values
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## get inverse
  inv <- x$getInverse()
  
  ## check if inverse already exist
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## if not exist - get matrix, cache inverse and return inverse
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv  
}
