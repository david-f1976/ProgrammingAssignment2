## Cache the inverse of a matrix in 4 parts;
##
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    ## initially set inverse to NULL
    inv <- NULL
    
    ## Set the matrix (not the inverse)
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Get the matrix
    get <- function() x
        
    ## Set the inverse
    setInverse <- function(inverse) inv <<- inverse
        
    ## Get inverse
    getInverse <- function() inv
        
    ## Put it all into a list
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)    
}


## Calculate the inverse of the matrix
## If it has already been calculated (and the matrix has not changed) retrieve from cache instead of recalculating

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv = x$getInverse()
    
    
    # If inverse hasn't been calculated
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Calculate it and return result
    data <- x$get()
    inv <- solve(data, ...)
    
    x$setInverse(inv)
    
    ## Return new result
    inv
    
}
