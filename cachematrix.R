## Put comments here that give an overall description of what your
## functions do
## This is assignment 2 for the Coursera course 'Programming in R'
## Blake Christiansen 18 Dec 2016

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## Write a short comment describing this function
## This function finds the inverse of the special "matrix" created with the above function.
## It first checks to see if the inverse has already been found. 
## If so, it `get`s the inverse from the cache and skips the work. 
## Otherwise, it finds the inverse of the matrix 
## and sets the value of the inverse in the cache via the `setinverse` function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv

}

## Example of using these two functions
## testm <- matrix(c(1,1,4,0,3,1,4,4,0),nrow=3,ncol=3)
## a <- makeCacheMatrix(testm)
## cacheSolve(a)
