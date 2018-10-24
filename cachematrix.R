## This file has two functions: makeCacheMatrix and cacheSolve
## makeCacheMatrix is a function that build up a funtion storing the inverse of 
##                  a matrix in the lexical scope cache

## cacheSolve is a function that build up a funtion to evaluate the inverse if 
##                  it is not cached in lexical scope, then store and return

## Missing validation for input type as those are under assumption

## Makecachematrix accepts a square matrix, defines
## getter, setter for its value
## getter and setter for cached inverse value

makeCacheMatrix <- function(x = matrix()) {
  ## Initiate the inverse value to NULL 
  xinv <- NULL
  
  ## Setter function for initializing the matrix and its inverse in lexical scope
  setval <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  
  ## Getter to return the matrix
  getval <- function() x
  
  ## Setter for storing inverse in lexical scope
  setinv <- function(xinve) xinv <<- xinve
  
  ## Getter to return the matrix inverse from lexical scope
  getinv <- function() xinv
  
  list(setval = setval, getval = getval,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve accepts a square matrix, retrieve chached inverse 
## from lexical scope if it is available or evaluates the inverse,
## stores and returns the same

cacheSolve <- function(x, ...) {
  
  ## Retrive cached inverse
  xinv <- x$getinv()
  
  ## Validate if it is null or has valid value
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  
  ## Evaluate the inverse
  data <- x$getval()
  ## print(data)
  detval <- det(data)
  
  if(detval[1] == 0)
  {
    message("Determinant is 0. Inverse does not exist.")
    return()
  }
  
  xinv <- solve(data, ...)
  
  ## Store the inverse in cache
  x$setinv(xinv)
  
  ## Return the inverse
  xinv
}



