## Put comments here that give an overall description of what your
## functions do

## Takes a matrix as input , and creates a vector of functions
## these functions are used by cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(mat) {
    x <<- mat
    inverse <<- matrix()
  }
  get <- function() x
  setinverse <- function(matrix_inverse) inverse <<- matrix_inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


##tests if inverse is already calculated
##and returns it if it is available

cacheSolve <- function(x, ...) {
  matrix_inverse <- x$getinverse()
  if(!is.null(matrix_inverse)) {
    return(matrix_inverse)
  }
  data <- x$get()
  matrix_inverse <- solve(data)
  x$setinverse(matrix_inverse)
  matrix_inverse
}
