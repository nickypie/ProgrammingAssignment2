## makeCacheMatrix and cacheSolve are used to create a special matrix,
## a "CacheMatrix", whose inverse needs to be computed only once.
## The first call for the inverse of the CacheMatrix using cacheSolve
## is computed with the solve function.
## Any subsequent call to cacheSolve on the same CacheMatrix will return
## the cached value of the inverse, without needing to call solve again.
## However, if the matrix in cacheMatrix is changed with the set function,
## the cached inverse is cleared (set to NULL).


## makeCacheMatrix returns a list of functions to represent the CacheMatrix.
## The functions in the list are:
##  set: takes a matrix y as an argument, and stores the value of y
##       into the CacheMatrix variable x. it also sets inv_x (the inverse
##       of the CacheMatrix) to NULL
##  get: takes no arguments, returns the value of the matrix x
##  setinverse: takes a matrix inverse_x as an argument,
##              and stores it into inv_x
##  getinverse: takes no arguments, returns the value of the matrix inv_x

makeCacheMatrix <- function(x = matrix()) {
    ## initialize the inverse of the matrix x as NULL
    inv_x <- NULL
    
    ## define the function set, which stores the argument matrix y into
    ## the variable x in the parent environment,
    ## and re-initializes the matrix inv_x in the parent environment to NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL 
    }
    
    ## define the function get, which returns the matrix x
    get <- function() x
    
    ## define the function setinverse, which stores the argument matrix
    ## inverse_x into the variable inv_x in the parent environment
    setinverse <- function(inverse_x) inv_x <<- inverse_x
    
    ## define the function getinverse, which returns the matrix inv_x
    getinverse <- function() inv_x
    
    ## return the list containing the four above-defined functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes a "CacheMatrix" list as an argument.
## this function checks to see if the inverse of the CacheMatrix has been
## computed already. if it has, the cached value of the inverse is returned.
## otherwise, the inverse of the CacheMatrix is computed using solve. The inverse
## is then stored in the CacheMatrix cache and returned.

cacheSolve <- function(x, ...) {
  
    ## Get the inverse of the CacheMatrix
    inv_x <- x$getinverse()
    
    ## if inv_x is not equal to NULL, that means the inverse of the CacheMatrix
    ## has already been computed and the cached value of inv_x can be returned
    if(!is.null(inv_x)) {
        message("getting cached data")
        return(inv_x)
    }
    ## if inv_x is NULL, the inverse of the CacheMatrix hasn't been computed yet.
    ## get the matrix to invert from the CacheMatrix
    m_to_invert <- x$get()
    
    ## calculate the inverse using solve
    inv_x <- solve(m_to_invert)
    
    ## set the value of the inverse of the CacheMatrix using setinverse
    x$setinverse(inv_x)
    
    ## return the inverted matrix
    inv_x
}
