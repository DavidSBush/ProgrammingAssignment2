## This pair of functions provides a mechanism for computing the inverse of a matrix and
## preserving the result between function calls by caching both the input matrix and its inverse


## The function makeCacheMatrix defines four functions for setting and retrieving the
## value of an input matrix and its inverse.  The matrix value is either that passed
## when this function is first invoked or as updated by the set function. The inverse
## value is set to NULL when the function is first invoked and when the value is updated
## with set.  This forces the function cacheSolve below to recompute the inverse.  The
## inverse value is saved in the invoking environment by super-assignment.
## The setInverse function should be used only by cacheSolve


makeCacheMatrix <- function(x = matrix()) {
            saved_inverse <- NULL		## Force inverse to be computed on first getInverse
            set <- function(y) {
                    x <<- y					## Save matrix value across function calls
                    saved_inverse <<- NULL	## Invalidate cached inverse value
            }
            get <- function() x
            setInverse <- function(matrix_inverse) saved_inverse <<- matrix_inverse
            getInverse <- function() saved_inverse
			## Return a list of the above functions for accessing matrix and inverse.
            list(set = set, get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)

}


## This function returns the inverse of the matrix that is contained in the object
## passed as the parameter x.  This parameter is created by the companion function
## makeCacheMatrix above.  The value of the matrix and its inverse are accessible
## only through the functions defined in makeCacheMatrix and returned as a list.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            inverse <- x$getInverse()
            if(!is.null(inverse)) {
                    message("getting cached data")
                    return(inverse)
            }
            data <- x$get()
            inverse <- solve(data, ...)
            x$setInverse(inverse)
            inverse
}
