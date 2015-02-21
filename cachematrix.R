## Caching the Inverse of a Matrix

## Function which creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(original_matrix = matrix()) {
        original_matrix <- NULL
        # - re-initiates 'original_matrix'
        # - re-sets 'inverce_matrix' to NULL, since it has to be recomputed
        #   ones original has been changed
        setMatrix <- function(set_matrix) {                
                original_matrix <<- set_matrix
                inverce_matrix  <<- NULL
        }
        # returns original matrix
        getMatrix <- function() original_matrix        
        # sets inverce matrix
        setInverceMatrix <- function(set_inverce_matrix){
                inverce_matrix <<- set_inverce_matrix
        }
        # returns inverce matrix
        getInverceMatrix <- function() inverce_matrix
        
        # list with above functions as elements
        list(setMatrix          = setMatrix,
             getMatrix          = getMatrix,
             setInverceMatrix   = setInverceMatrix,
             getInverceMatrix   = getInverceMatrix) 
}

## Function which gets special "matrix" object (mCM_Obj) as input
## and computes the inverse of the matrix if has not been computed
## and stored in cache yet - otherwise gets it from cache
cacheSolve <- function(mCM_Obj, ...) {
        inverce_matrix <- mCM_Obj$getInverceMatrix()
        if(!is.null(inverce_matrix)) {
                message("getting inverce matrix data")
                return(inverce_matrix)
        }
        original_matrix <- mCM_Obj$getMatrix()
        inverce_matrix <- solve(original_matrix)
        mCM_Obj$setInverceMatrix(inverce_matrix)
        inverce_matrix
        ## Return a matrix that is the inverse of 'original_matrix'
}