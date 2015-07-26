## The functions in this file use the scoping rules in R to 
## create a special 'matrix' object and cache its inverse

## makeCacheMatrix will create a special copy of the input x and 
## store its inverse within the scope of the function. It returns
## a list of functions to store and retrieve the matrix as well as its 
## cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        CachedMatrixInverse <- NULL
        
        ##Store the new supplied matrix in the scope of the function
        SetMatrix <- function(NewMatrix) {
                x <<- NewMatrix
                ## Invalidate the cached Inverse since the matrix could have 
                ## changed. A future improvement could be to check if the new matrix
                ## is actually different from the stored one and only then
                ## invalidate the cache
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


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve retrieves the inverse
## from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
