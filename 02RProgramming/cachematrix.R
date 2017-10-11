## Some words about "makeCacheMatrix" and "cacheSolve":
## To demonstrate the use of lexical scoping, the following functions
## "makeCacheMatrix" and "cacheSolve" can modify their parent environment
## in order to cache data and improve the calculating time.

## makeCacheMatrix creates a list-vector with the functions set, setinverse,
## get, getinverse. 

makeCacheMatrix <- function(x = matrix()) {
  # initialize variable inv
  inv <- NULL
  
  # define the function "set" to set the matrix data from the parent variable "y"
  # and initialize variable inv
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # define the function "get" to show the data of the input matrix
  get <- function() x
  
  # define the function "setinverse" to set the value of the inversed matrix from
  # the parent variable "inverse"
  setinverse <- function(inverse) inv <<- inverse

  # define the function "getinverse" to show the data of the inversed matrix  
  getinverse <- function() inv
  
  # generate a list with the defined functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function, which returns the inverse to 'x'. Before 
## solving the matrix, the function checks if this operation has been
## already done for the given matrix. If this is true, cacheSolve gives the 
## cached inverse and stops calculating. If not, it calculates the inverse
## and set in the caché.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    # look for a cached inverse matrix
    inv <- x$getinverse()
    
    # if true, display a message, return the inverse (variable "inv") and stop execution
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    # else: get the new data
    data <- x$get()
    
    # solve the inverse
    inv <- solve(data, ...)
    
    # set the inverse in the caché
    x$setinverse(inv)
    
    # return the inverse
    inv
}
