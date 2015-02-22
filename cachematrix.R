## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly.

## The following pair of functions are created to cache the Inverse of a Matrix 

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {                           ## set the value of the matrix
    
        x <<- y
        m <<- NULL
    
  }
  
  get <- function() x                            ## get the value of the matrix
  
  setinverse <- function(inverse) m <<- inverse  ## set the inverse of the matrix
  getinverse <- function() m                     ## get the inverse of the matrix
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
  
}

## cacheSolve() computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {       ## Return a matrix that is the inverse of 'x'
        
  
  m <- x$getinverse()                  ## getting the inverse of x 
                                       ## from makeCacheMatrix function
  
  
  if(!is.null(m)) {                    ## check if inverse of x has been calculated
    message("getting cached data")     ## if yes, return inverse of x and exit function
    return(m)
  }
  
  data <- x$get()                      ## else, get the matrix and calculate the inverse
  m <- solve(data, ...)                ## and return the inverse of x
  x$setinverse(m)
  m
  
}

