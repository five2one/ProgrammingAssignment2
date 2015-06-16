## Functions 1: Used to create an object that stores a matrix and cached
## its inverse.

## 1. Set the value of the matrix (set_matrix)
## 2. Get the value of the matrix (get_matrix)
## 3. Set the value of the inverse (set_inverse)
## 4. Get the value of the inverse (get_inverse)

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL                 ## assign NULL to inverse
    
    set_matrix <- function(y) {
        x <<- y                     ## Set input matrix y to x
        inverse <<- NULL
    }

    get_matrix <- function() x      ## Get matrix x
    
    ## Cached the value of the 'inverse' equal to inverse of matrix x
    set_inverse <- function(solve) inverse <<- solve 
    
    get_inverse <- function() inverse ## Get cached inverse of x
    list(set_matrix = set_matrix, 
         get_matrix = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## Function 2: Calculates the inverse of the special matrix from above

## 1. First checks to see if the inverse has already been calculated
## 2. If yes, get the inverse from the cache & skips the computation
## 3. Else, calculate the inverse of the data and sets the value of the inverse
##    in the cache via the 'set_inverse' function


cacheSolve <- function(x, ...) {        ## Get inverse of matrix x
    inverse <- x$get_inverse()          ## Get inverse info

    if(!is.null(inverse)) {             ## Check for the presence of inverse
        message("getting cached data")  ## Display message
        return(inverse)
    }

    dta <- x$get_matrix()              ## Get matrix
    inverse <- solve(dta, ...)         ## Calculate inverse
    x$set_inverse(inverse)             ## Cached the inverse
    inverse 
}