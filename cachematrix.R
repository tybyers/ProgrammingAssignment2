## Tyler Byers
## Coursera RProg-004
## Programming Assignment #2 (peer graded)
## 7 June 2014

# The makeCacheMatrix function creates a special "matrix", 
# which is really a list containing a function to
## 1) set the matrix
## 2) get the matrix
## 3) set the inverse of the matrix
## 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y) {
       x <<- y
       i <<- NULL
   }
   get <- function() x
   setinv <- function(solve) i <<- solve
   getinv <- function() i
   list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse
## of the data and sets the value of the inverse in the cache via the
## setinv function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message('Getting Cached Inverse')
        return(i)
    }
    matx <- x$get()
    i <- solve(matx, ...)
    x$setinv(i)
    i
}
