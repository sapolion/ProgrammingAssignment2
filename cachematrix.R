## Put comments here that give an overall description of what your
## functions do
# I believe this does what the assignment calls for.
# ### makeCacheMatrix [basically a copy of the makeVector function]
# creates a list containing four functions 
# 
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse<- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# This returns the inverse of the matrix if it has not already been 
# solved. This simply copies the example, but if I understood matices
# I would make sure that the matrix was square and check it's determinant
# and throw some kind of error but I'm running late

cacheSolve <- function(x, ...) {
 
         ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  }
