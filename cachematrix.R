## Put comments here that give an overall description of what your
## functions do

##these functions work together to calculate the inverse of the input 
##matrix object and save it to cache.  If the inverse of a matrix object
##has already been calculated the cached value is retrieved

## Write a short comment describing this function

##this function saves a vector which is a list containing functions to
##get the matrix and its inverse and cache the inverse

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
}


## Write a short comment describing this function

##If a value for the inverse of a matrix object is stored in the cache 
##(ie has already been calculated) this function will return the cached
##value and show a message indicating that the value has come from the cache.

##If the value of m is null indicating there is no value stored in the
##cache it runs the functions from the first function which get the matrix,
##solve its inverse and return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
        
}
