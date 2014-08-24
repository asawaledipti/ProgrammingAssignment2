## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {  ## set the value of the vector
    x <<- y
    m <<- NULL
  }
  get <- function() x    ## get the value of the vector
  setsolve <- function(mean) m <<- solve  ## set the value of the mean
  getsolve <- function() m  ## get the value of the mean
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {  
    message("getting cached data")
    return(m)  ## return the value if inverse already been calculated
  }
  data <- x$get()
  m <- solve(data, ...) 
  x$setsolve(m)
  m  ## calculate the inverse if the value is not calculated before
}
