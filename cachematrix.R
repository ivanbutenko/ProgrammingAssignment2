## Implementation of matrix as a special object where the inverse matrix is computed on demand and then cached for latter use in this object
## makeCacheMatrix creates this object which actually is a list of functions with neccessary values kept in these values closures

## 'makeCachematrix' recieves a matrix as an argument (or coerces its argument to matrix), 
## keeps matrix in 'x' variable, initializes 'inv' 
## with NULL for latter storage of inverse matrix and defines four functions which closures contain variables 'x' and 'inv'. TChese 
## functions are 'set' to change the matrix, 'get' to return matrix, 'getinv' to return inverse matrix if it's calculated or NULL if not 
## yet, and 'setinv' to write the inverse matrix. These 4 functions are returned in a named list which actually is an object the 
## function is designed to construct

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## cache for inverse
    set <- function(y) {
        x <<- y ## 'x' in closure keeps the original matrix
        inv <<- NULL ## initialize cache with NULL after new matrix is saved
    }
    get <- function() x
    setinv <- function(inverse) inv  <<- inverse ## '<<-' tells R to look up for the variable on the left (the one to be 
                                                 ## assigned a value) in functions parent environment - not to create a local 
                                                 ## variable with this name 
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## 'cacheSolve' recieves an object representing a matrix with inverse cached and returns its inverse - either by looking it up in cache ## ## with function 'getinv' which returns functions internal variable 'inv', or if it's NULL by calculating it after reading 
## the matrix with 'get' function from variable 'x' in functions closure. If the inverse is calculated (not read from cache),
## 'cacheSolve' writes this inverse matrix to the cache with 'setinv' to its internal 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv() ## read cache
    if(!is.null(inv)) { ## test if inverse is already computed and stored in cache (i.e. cache has something, not NULL)
        message("getting cached data")
        return(inv) ## return cached value
    }
    data <- x$get() ## read the matrix
    inv <- solve(data, ...) ## compute its inverse
    x$setinv(inv) ## cache inverse
    inv ## return inverse
}
