makeCacheMatrix <- function(x = matrix()){
  # Initialize the inverse value
  m <- NULL
  
  # Method to set the matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  # Method to get the matrix
  get <- function() x
  
  # Method to set the inverse of the matrix
  setinverse <- function(inverse) m <<- inverse
  
  # Method to get the inverse of the matrix
  getinverse <- function() m
  
  # Output list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...){
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
