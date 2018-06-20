## This function returns a list of functions to set the value of matrix, get the value of matrix,  

## set the matrix inverse and get the matrix inverse.


makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }
  get <- function() x
  setmi <- function(matrixinv) mi <<- matrixinv
  getmi <- function() mi
  list(set = set,
       get = get,
       setmi = setmi,
       getmi = getmi)
}


## This function returns a matrix that is the inverse of 'x'.
## If the matrix inverse is already computed and exists, it returns the same. Else it computes and returns.

cacheSolve <- function(x, ...) {
  
  mi <- x$getmi()
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  data <- x$get()
  mi <- solve(data)
  message("calculating matrix inverse")
  x$setmi(mi)
  mi
}
