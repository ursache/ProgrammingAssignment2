## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#
# this function creates an object that computed the inverse of the matrix
# if first time, store the result and set the m flag to 1
# if not the first time, stores the cached inverse


makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
#
# this function returns:
# - the inverse of the matrix computed with the function solve if m is non zero
# - the cached invserse of the matrix
#
cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        mat_inv <- solve(data, ...)
        x$setinv(mat_inv)
        mat_inv
}
