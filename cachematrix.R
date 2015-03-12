## Put comments here that give an overall description of what your
## functions do

## The following functions calculate the inverse of a matrix. 
## They deal with this in  a very special way. Since matrix 
## inversion  calculations can be a time consuming task, the 
## current matrix inverse is cached after calculating it the 
## first time. Each time this inverse is needed it is popped 
## out of cache. If the original matrix is replaced with a 
## new one, obviously the inverse is then calculated and cached.

## Write a short comment describing this function

## The makeCacheMatrix function gets a matrix as its argument and 
## caches its inverse. It creates and holds the functions necessary 
## for keeping track of the matrix and its inverse, so one can get/set 
## the matrix (get/set) or get/set its inverse (getinv/setinv). 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## Initialize cache.
  ## set - function to replace matrix with a new one.
  ## Here we really exploite the lexical scope R uses.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x ## Deliver matrix.
  setinv <- function(inv) m <<- inv ## Cache the inverse.
  getinv <- function() m ## Pop the cached inverse.
  ## Keep all functions in a list and return it to caller.
  ## This way they can be used in function cacheSolve.
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function

## The function cacheSolve take a list of functions as argument.
## It the uses these functions to compute and/or cache the inverse. 
## Firstly, it checks to verify if the inverse has been calculated.
## If so, it returns it. Otherwise it calculates it and then it 
## sends it to be cached.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv() ## Is the inverse cached?
  ## Yes? return it.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Not cached. Calculate it.
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m) ## Cache it.
  m ## Well, here is the inverse!
}
