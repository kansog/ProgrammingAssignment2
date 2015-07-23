## This script was built as part of Assignment#2 of
## the Data Science training course. It contains a pair
## of functions that cache the inverse of a matrix.
 
## Example
## x <- matrix(rnorm(9)), nrow(3)           -- Creates a "matrix" x
## cx <- makeCacheMatrix(x)                 -- Creates special matrix
## cx$get()                                 -- Returns the matrix
## cacheSolve(cx)                           -- Returns the inverse
## cacheSolve(cx)                           -- Call the 2nd time, so return
##                                             the cached inverse.
 
## makeCacheMatrix - This function creates a special "matrix"
## object that can cache its universe. It return a list of functions to:
## 1. Set the value of the matrix;
## 2. Get the value of the matrix;
## 3. Set the value of the inverse; and
## 4. Get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
   
   # solve will store the cached inverse matrix
   m <- NULL
   
   # 1. Set the value of the matrix
   set <- function(y) {
     x <<- y
     m <<- NULL
   }
   
   # 2. Get the value of the matrix
   get <- function() x

   # 3. Set the value of the inverse
   setsolve <- function(solve) m <<- solve
   
   # 4. Get the value of the inverse
   getsolve <- function() m
   list(
     set = set,
     get = get,
     setsolve = setsolve,
     getsolve = getsolve)
 }

## cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the universe
## has already been calculated, then the cachesolve should retrieve
## the inverse from the cache.
 
cacheSolve <- function(x, ...) {
         
   m <- x$getsolve()
   
   ## If the inverse was already calculated, return it.
   if(!is.null(m)) {
   message("getting cached data...")
     return(m)
   }

   ## If the inverse was not calculated, we calculate it.
   data <- x$get()
   m <- solve(data, ...)
   
   ## Cache the inverse
   x$setsolve(m)
   
   ## Return it
   m
   }
