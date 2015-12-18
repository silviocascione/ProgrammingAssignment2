## This function creates a special "matrix" object that can cache its inverse.
## The solution is very similar to example given. Instead of setting the value 
## of a vector, we should set up a matrix and then set the value of its inverse.
## The structure is pretty much the same, we only need to change the functions 
## accordingly - most importantly, x=matrix() instead of x=numeric(), and function
## inverse instead of mean.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned. As above,
## the structure of the following function is the same from the example in the
## assignment instructions. Most importantly, instead of using the function mean,
## we should use solve(), as stated in the instructions.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

