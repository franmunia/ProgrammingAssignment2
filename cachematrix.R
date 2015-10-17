## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function makes a vector containing the matrix and the inverse of the matrix,
# if it had been calculated, or inverse=null, if it hadn't been calculated 

makeCacheMatrix <- function(x = matrix()) {inv <- NULL
set <- function(y) {
  #this function is useful to change the matrix without needing to change the list
  # use `<<-` to assign a value to an object in an environment 
  # different from the current environment. 
  x <<- y
  inv <<- NULL
}
#defining function get (as matrix "x")
get <- function() x 
#defining function setinv: it sets inverse as inv
setinv <- function(inverse) inv <<- inverse
#defining function getinv (as "inv")
getinv <- function() inv
#builting the list of functions
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)

}


## Write a short comment describing this function
#the next function verifies if the inverse was calculated. If it was, the function
# returns it, without calculating again. If it was not, this function calculates the
# inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv() #this line returns the result of "getinv" (the inverse) function
  if(!is.null(inv)) {
    #if the inverse has already been calculated, the following message is show and the function returns "inv"
    message("getting cached data")
    return(inv)
  }
  #if the inverse hasn't been calculated, it calculate it
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  return(inv)
}

