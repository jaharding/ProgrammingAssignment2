## MakeCacheMatrix
## The following function makes a Matrix in an environment that is different 
## from the current environment.
## 
## This is an assignment for CourseRA course on R programming
## Date: 8/30/16



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve
## The following function returns a matrix that is the inverse of the input matrix 'x'
## If the cached matrix exist, the cached matrix is utilized.
## 
## This is an assignment for CourseRA course on R programming
## Date: 8/30/16

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
  
}
