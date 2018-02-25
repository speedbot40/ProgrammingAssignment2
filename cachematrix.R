## This file contains two functions, makeCacheMatrix()
## and cacheSolve(). These functions, when used in combination,
## can be used to calculate and retrieve the inverse of a
## matrix. The inverse will be stored in cache. If the input
## matrix has not changed and the inverse is needed again,
## the inverse will be retrieved from cache instead of being
## recomputed.

## Example usage:
##
## > myMatrix <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## > myMatrixFunctions <- makeCacheMatrix(myMatrix)
## > cacheSolve(myMatrixFunctions)
##      [,1] [,2]
## [1,]    6    8
## [2,]    2    4
## >
## > cacheSolve(myMatrixFunctions)
## getting cached data
##      [,1] [,2]
## [1,]    6    8
## [2,]    2    4 


## makeCacheMatrix()
##
## This function takes as its input parameter the matrix
## to be inverted.

## m is used to determine whether the function has already been
## executed for the current input matrix.
## m is initially set to NULL.

## The input matrix is stored in x in the makeCacheMatrix()
## environment.

## makeCacheMatrix() returns a vector of functions:

##   set(), which can be used to specify a new input matrix
##   get(), which returns the input matrix.
##   setinverse(), which sets a new value for m.
##   getinverse(), which returns the value of m.



makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
		
		## function to set a new input matrix value
		## and reset m to NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		
		## function to return the input matrix
        get <- function() x
		
		## function to set the value of m to the
		## inverted matrix
        setinverse <- function(inverse) m <<- inverse
		
		## function to return m (either NULL or the
		## inverted matrix)
        getinverse <- function() m
		
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve()
## This function takes as its input parameter the vector 
## of functions returned by makeCacheMatrix().
##
## cacheSolve() returns the inverse of the matrix which was
## input to makeCacheMatrix().
##
## If cacheSolve() is executed and the input matrix in the
## makeCacheMatrix() environment has been changed since the time
## cacheSolve() was previously executed, the inverse of the
## matrix will be calculated. Otherwise, the inverse will be
## retrieved from cache.
## 
## cacheSolve() first calls the getinverse() function to
## determine whether the value of m in the makeCacheMatrix()
## environment is NULL. It assigns the returned value to the
## vector m in its own environment.
## 

cacheSolve <- function(x, ...) {

        ## get the value of m from makeCacheMatrix()
        m <- x$getinverse()
		
		## If m is not NULL, it is already caching the inverted
		## matrix for the same input matrix. Return m and
		## stop further execution of this function.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		
		## The remaining code will only execute if m is NULL.
		
		## Get the input matrix from makeCacheMatrix().
        data <- x$get()
		
		## Calculate the inverse of the matrix.
        m <- solve(data, ...)
		
		## Pass the inverted matix to makeCacheMatrix() so
		## that m in makeCacheMatrix() gets updated.
        x$setinverse(m)
		
		## Return the calculated inversion.
        m
}
