## Cached Invertion of invertible matrix 


## Create a special list of matrices

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    
    x <<- y
    
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(solve) m <<- solve     
  getinverse <- function() m
  
  list(set = set, get = get,
       
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Invert a matrix and store it cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  if(!is.null(m))
  {
    
    message("getting cached data")
    
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setinverse(m)
  m
}
