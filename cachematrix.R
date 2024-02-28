## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function can provide a special "matrix" object that is able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y){
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setin <- function(inverse){iv <<- inverse}
  getin <- function() iv
  return(list(set = set, get = get, setinverse = setin, getinverse = getin))
}



## The function should generate the inverse of the special "matrix" returned by the aforementioned makeCacheMatrix 
## function. As long as the inverse has already been calculated and the matrix has not been altered, the cacheSolve 
## function should be able to obtain the inverse from the cache.

cacheSolve <- function(x, ...) {
        iv <-x$getin()
        if (!is.null(iv)){
          message("finding data in cache")
          return(iv)
        }
        data <- x$get()
        iv <- solve(data, ...)
        x$setin(iv)
        iv
}
