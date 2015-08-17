## R-Programming - Programming Assignment 2

## The function makeCacheMatrix takes a matrix as input and stores its value(s) in the variable x. 
## The function also contains four functions, whose purposes are further explained in the function's body.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## m is set to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  } 
          ## if set(y) is called, the matrix x stored in the main-function is changed to y
  get <- function() x 
          ## get() returns the matrix stored in the makeCacheMatrix-function
  setinverse <- function(inverse) m <<- inverse 
                                  ## setinverse() assigns it's input to m. m is supposed to be the inverse matrix of x.
  getinverse <- function() m      ## get m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  ## the functions defined above are stored in the makeCacheMatrix-function
                ## all functions stored in list can get called by subsetting the object, to which the main function is assigned.
}

## The function cacheSolve returns the inverse matrix which is either stored in cache or is newly calculated
## further details are explained in the function's body

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {                             
    message("getting cached data")
    return(m)
  }                     ## if there is already data assigned to m in cache, m is returned
  data <- x$get()       ## ELSE: the value of x is assigned to data by a call to the get()-function defined in makeCacheMatrix
  m <- solve(data, ...) ## The inverse matrix is calculated with the solve()-function
  x$setinverse(m)       ## the setinverse()-function is called to store the inverse matrix (m) in cache
  m                     ## the inverse matrix is returned
}
