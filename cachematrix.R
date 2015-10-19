## Put comments here that give an overall description of what your
## functions do:
## These functions serve to define the matrix <<- is used to define an objec that differs (inverse) 
## return: a list containing functions to        
## 1. set matrix
## 2. get matrix
## 3. set inverse
## 4. get inverse
## 5. list is then used to calculate cacheSolve
## Write a short comment describing this function
## in order to do rapid calculations we define a matrix that can cache it's own inverse to make calculating easier

makeCacheMatrix <- function(x = matrix()) {

  v  <- NULL
  set <- function(y) {
    x <<- y
    v <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) v<<- inverse 
  getinv <- function() v
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## Write a short comment describing this function
## define x as the output of makeCacheMatrix()
## it gives the inverse of the original matrix input to makeCacheMatrix() or calculates
## if has not been done so already 

cacheSolve <- function(x, ...) {
  v <- x$getinv()        
  if (!is.null(v)){
    message("getting cached data")
    return(v)
  }
  
  mat.data <- x$get()
  v <- solve(mat.data, ...)
  
  x$setinv(v)
  
  
  return(v)        
}
