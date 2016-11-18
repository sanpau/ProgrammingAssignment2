





## pair of functions that
## cache the inverse of a matrix.

## `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  invt <- NULL
  # function that sets the value of the matrix IN set
  set <- function(y) {
    x <<- y
    invt <<- NULL
  }
  #returns matrix stored in the main function
  get <- function() x
  
  # store the value of the input in a variable invt into the main function 
  # makeCacheMatrix (setinvt) and return it (getinvt)
  setinvt <- function(inverse) invt <<- inverse
  getinvt <- function() invt
  
  
  list(set = set, get = get, setinvt = setinvt, getinvt = getinvt)
}  


## This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## If the inverse has already been calculated, then the cacheSolve should retrieve the inverse from the cache.
  invt <- x$getinvt()
  if(!is.null(invt)) {
    message("getting cached result")
    return(invt)
  }
  ##  If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix, 
  ##  invt calculates the inverse, and x$setinvt(invt) stores it in the object invt in makeCacheMatrix
  data <- x$get()
  invt <- solve(data, ...)
  x$setinvt(invt)
  invt
}



##Output of the above :
  
##  mtrx <- matrix(c(1,1,1,3,4,3,3,3,4),3,3)
##> mtrx
##[,1] [,2] [,3]
##[1,]    1    3    3
##[2,]    1    4    3
##[3,]    1    3    4
##> mtrx1 <- makeCacheMatrix(mtrx)
##> cacheSolve(mtrx1)
##[,1] [,2] [,3]
##[1,]    7   -3   -3
##[2,]   -1    1    0
##[3,]   -1    0    1
