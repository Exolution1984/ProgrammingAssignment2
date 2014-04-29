## These two function work in conjunction to solve the inverse matrix, with 
## a mechanism to cache the inverse matrix.

## makeCacheMatrix creates a special matrix caching the matrix and its inverse matrix, which 
## also contains a list of functions getting and setting the matrix and its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(i) inv <<- i
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)  
}


## cacheSolve detects whether the inverse of matrix exists first. If it exists
## cacheSolve simply get the inverse matrix, or else cacheSolve calculates the inverse

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setInv(inv)
    inv
}
