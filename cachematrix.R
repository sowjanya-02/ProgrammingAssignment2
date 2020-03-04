## The following functions are used to create the object x that stores
##matrix and caches inverse of a matrix


## cache matrix creates a special matrix set is used to set the value a matrix,
## get is used to get the value similarly setinverse and getinverse are used

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
        x <<- y
        inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse)inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)


  }


## cachesolve is a function to calculate a inverse of a matrix returned by above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- inverse(data, ...)
        x$setinverse(inv)
        inv

}
