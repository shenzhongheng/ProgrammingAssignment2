## two functions to compute the inverse of a matrix, if the inverse is stored in cache, 
## it will be returned directly from cache instead of computing it again. If the inverse 
## cannot be found in cache, it will be computed

## makeCacheMatrix function returns 4 sub-functions. 
## set function allows user to set the matrix 
## get function allows user to get the matrix which has been set. If not, get function will return NULL matrix
## set_inverse function allows user to set the inverse of the matrix
## get_inverse function allows user to get the inverse of the matrix which has been set. If not, get_inverse function will return NULL matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() {x}
    set_inverse <- function(inv) {m <<- inv}
    get_inverse <- function() {m}
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## the argument of cacheSolve function is a list 
## returned by the function makeCacheMatrix
 

cacheSolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
    m <- x$get_inverse()
    if (!is.null(m)){
        message("return inverse from cache")
        return(m)
    }
    mat <- x$get()
    m <- solve(mat, ...)
    x$set_inverse(m)
    m
}
