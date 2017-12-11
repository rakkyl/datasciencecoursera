## The two functions work together to cache the inverse of a matrix, rather than constantly computing the inverse. 
## We assume that the matrix supplied is always invertible.

## The first function 'makeCacheMatrix' produces a list containing the following functions:
##  1. set value of the matrix
##  2. get value of the matrix
##  3. set value of the inverse matrix
##  4. get value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
          i <- NULL
          set <- function(y)  {
              x <<- y 
              i <<- NULL
          }
          get <- function()  x
          setinverse <- function(inverse)  i <<- inverse
          getinverse <- function() i
          list(set = set, get = get, 
               setinverse = setinverse, 
               getinverse = getinverse)  
}

## The second function 'cacheSolve' outputs the inverse of the matrix. First, the function checks to see whether
## the inverse has already been computed. If the inverse exists, then the function retrieves the result and 
## returns it. If the inverse does not exist, the function computes the inverse and subsequently stores the value 
## in the cache with the help of the 'setinverse' function located in 'makeCacheMatrix'.

cacheSolve <- function(x, ...) {
          i <- x$getinverse()
          if(!is.null(i))  {
              message("getting cached data.")
              return(i)
          }
          data <- x$get()
          i <- solve(data)
          x$setinverse(i)
          i
}
