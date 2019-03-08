## The first function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  cm <- NULL
  setmatrix <- function(y) {  ##sets the matrix
    x <<- y
    cm <<- NULL
  }
  getmatrix <- function() x   ##gets the matrix
  setmatrixinverse <- function(inverse) cm <<- inverse  ##sets the inverse of the matrix
  getmatrixinverse <- function() cm     ##gets the inverse of the matrix
  
  list(setmatrix = setmatrix, getmatrix=getmatrix,
       setmatrixinverse = setmatrixinverse,
       getmatrixinverse = getmatrixinverse)
}

##  The second function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {  ## Returns a matrix that is the inverse of 'x'
  cm <- x$getmatrixinverse()
  if(!is.null(cm)) {   #If the inverse has already been calculated (and the matrix has not changed)... 
    message("getting cached data")  ##....then the cachesolve function retrieves the inverse from the cache....
    return(cm)   ##...and returns the inverse here.
  }
  data <- x$getmatrix()  
  cm <- solve(data, ...)   ##If the inverse matrix has not already been calculated, cacheSolve then inverses the matrix...
  x$setmatrixinverse(cm)   ##...and sets the value of the matrix in the cache via the setmatrixinverse function
  cm
}

