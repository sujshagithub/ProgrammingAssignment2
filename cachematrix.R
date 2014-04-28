## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse of the matrix
##Inverse of A where A is a square matrix = solve(A)


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i<<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function


##cacheSolve function calculates the mean of the  "matrix" created
##with the above function. However, it first checks to see if the inverse has 
##already been calculated. If so, it gets the inverse from the cache and skips 
##the computation. Otherwise, it calculates the inverse of the data and sets the value
##of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
