## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#the makeCacheMatrix() creates a list of functions viz set(),get(),setInverse() and getInverse()
#x is the matrix whose inverse is to be calculated and m is the inverted matrix of x
#m is initialized with NULL value
# set() sets m as NULL 
# get() gets the value of matrix x whose inverse is to be found
#setInverse() take the inverted matrix as an argument and it assigns this value to variable m which is outside of setInverse()
#environment
#getInverse() gets the value of m 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

#this is the function that is to be used as an alternative to solve(). 
#it takes as input the list of functions created by the makeCacheMatrix()
#getInverse function is called to get the cached value of inverse matrix m. If the value is not null it is returned by
#this function as the output
#if the value is null that means no cached value of inverse matrix is present and hence it is calculated by 
#executing the solve() on the matrix X. After the value of m is found out, it is cached by setInverse() and m is returned


cacheSolve <- function(listoffuncns, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- listoffuncns$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- listoffuncns$get()
  m <- solve(data, ...)
  listoffuncns$setInverse(m)
  m
}
