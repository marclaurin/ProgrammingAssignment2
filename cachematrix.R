## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly. 
## Below are a pair of functions that cache the inverse of a matrix.


## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inversed <- NULL
        set <- function(y){
                x <<- y
                inversed <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrx) inversed <<- matrx
        getmatrix <- function() inversed
        list(set=set,get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversed <- x$getmatrix()
        if(!is.null(inversed)){
                message("Inverse already calculated. Retrieving data from cache")
                return(inversed)
        }
        result <- x$get()
        inversed <- matrx(result)
        X$setmatrix(inversed)
        inversed
}
