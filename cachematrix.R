##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(mt = matrix()) {
        inv_mt <- NULL
        set <- function(fm) {
                mt <<- fm
                inv_mt <<- NULL
        }
        get <- function() mt
        setSolve <- function(invmat) inv_mt <<- invmat
        getSolve <- function() inv_mt
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.
cacheSolve <- function(mt, ...) {
        ## Return a matrix that is the inverse of 'mt'
        
        inv_mt <- mt$getSolve()
        if(!is.null(inv_mt)) {
                message("Getting cached Inverse of a Matrix")
                return(inv_mt)
        }
        data <- mt$get()
        inv_mt <- solve(data)
        mt$setSolve(inv_mt)
        inv_mt
}
