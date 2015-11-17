##Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly (there are also alternatives to matrix inversion that we will not discuss here).


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
  
  {
  
  ivx = NULL

## makeCacheMatrix: Set This function creates a special "matrix" object   
  set = function(y)
  {
    x <<- y
    ivx <<- NULL
  }
  
## makeCacheMatrix: Get This function creates a special "matrix" object   
  get = function()
  {
    x
  }
  
  setinverse = function(inverse)
  {
    ivx <<- inverse
  }

 ## makeCacheMatrix: Set This function creates a special "matrix" object   
 
  getinverse = function()
  {
    ivx
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
  
}

## END makeCacheMatrix: 


## cheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## cheSolve: This function computes the inverse of the special "matrix" 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inverse = x$getinverse()
  if(!is.null(inverse)) {
    message("Getting Cached Data")
    return(inverse)
  }

## cheSolve: This function computes the inverse of the special "matrix" 

  
  inverse = solve(x$get())
  x$setinverse(inverse)
  inverse
}
## END cheSolve: This function computes the inverse of the special "matrix" 

## End Of 2nd Function