## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix calculates the inverse of a given matrix assigns
## that value and stores it
## Function cacheSolve checks to see if the inverse of a given matrix has
## already been calculated and assigned.  If so it returns the cached value,
## Otherwise it calculates the value

## Write a short comment describing this function
## Essentially a function that creates a list containing a function which sets
## the value of a matrix, gets the value of a matrix, sets the value of its
## inverse, and gets the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y){
    x <<- y
    I <<- NULL
  }
  get <- function()x
  setinv <- function(solve) I <<- solve
  getinv <- function() I
  list(set = set, get = get, setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## Retrieves the value of the inverse of a given matrix.  If it is not "NULL",
## it returns the value calculated in makeCacheMatrix.  If it is "NULL", it 
## calculates the inverse and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getinv()
  if(!is.null(I)){
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinv(I)
  I
}
