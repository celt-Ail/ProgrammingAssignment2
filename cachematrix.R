#'  A program to return the inverse of a matrix, with caching capabilities. 
#' @examples
#' x <- makeCacheMatrix(matrix(1:4, nrow=2))
#' cacheSolve(x)
#' 
#' Function based of example given by Roger Peng 
#' (https://github.com/rdpeng/ProgrammingAssignment2)

#' makeCacheMatrix(x): Create special matrix object for cachable inverse:
#' @param x, invertible matrix
#' Return a list with following functions:
#'        set(): store matrix in containing environment
#'        get(): get stored matrix from containing environment
#'        setInverse(): store 'inverse' in containing environment
#'        getInverse(): return stored inverse from containing environment
makeCacheMatrix <- function(x = matrix()) {
        
        if(!is.matrix(x)) stop("Object is not a matrix")
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve(x): function that either return inverse of 
## makeChacheMatrix-object, or the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        if(det(data) == 0) stop('Matrix is not invertible')
        i <- solve(data, ...)
        x$setInverse(i)
        i
}