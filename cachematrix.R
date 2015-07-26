## The functions in this file use the scoping rules in R to 
## create a special 'matrix' object and cache its inverse


## Function: makeCacheMatrix
## makeCacheMatrix will create a special copy of the input matrix x  
## within its scope. It returns the special "matrix", which is really 
## a list containing the following functions to store and retrieve 
## the matrix as well as its cached inverse:
## set = Sets the Matrix,
## get = Gets the Matrix,
## setinverse = Sets the Inverse,
## getinverse = Gets the Inverse

makeCacheMatrix <- function(x = matrix()) {
        CachedMatrixInverse <- NULL
        
        ##Store the new supplied matrix in the scope of the function
        SetMatrix <- function(NewMatrix) {
                x <<- NewMatrix
                ## Invalidate the cached Inverse since the matrix could have 
                ## changed. A possible optimization could be to leave the cache
                ## valid if the new matrix supplied is the same as the stored 
                ## one.
                CachedMatrixInverse <<- NULL
        }
        
        ## Retrieve the stored matrix 
        GetMatrix <- function() x
        
        ## Store the supplied matrix inverse in the cache 
        SetInverse <- function(SuppliedInverse) 
                CachedMatrixInverse <<- SuppliedInverse
        
        ## Return the cached inverse
        GetInverse <- function() CachedMatrixInverse
        
        ## Return a list of functions to store and retrieve the matrix and its 
        ## cached inverse 
        list(set = SetMatrix, get = GetMatrix,
             setinverse = SetInverse,
             getinverse = GetInverse)

}


## Function: cacheSolve
## This function computes the inverse of the special "matrix" x returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve retrieves the inverse
## from the cache.
## As per the allowed assumption of the assignment, the function currently 
## supports invertible (and hence square) matrices only.
## Any arguments supplied other than the special matrix will be passed to the 
## solve function in addition to the original matrix.

cacheSolve <- function(x, ...) {
        ##Get the stored inverse
        Inverse <- x$getinverse()
        
        ##If the stored inverse is not NULL, the cache is usable!
        if(!is.null(Inverse)) {
                message("getting cached data")
                return(Inverse)
        }
        
        ##Retrieve the original matrix and compute the inverse
        Data <- x$get()
        Inverse <- solve(Data, ...)
        
        ##Store the Inverse in Cache for future use
        x$setinverse(Inverse)
        
        ##Return the computed inverse
        Inverse
}
