## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object and caches its inverse
## First it sets the matrix value
## Second it gets the matrix value
## Third it sets the inverse of initial matrix
## Fourth it gets the inverse of initial matrix

makeCacheMatrix <- function(x = matrix()) {
        
        ## Set the initial inverse matrix to NULL
        m <- NULL
        
        ## Function to cache the initial matrix value
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Function to get the initial matrix value
        get <- function() x
        
        ## Function to cache the inverse matrix value
        setinverse <- function(inverse) m <<- inverse
        
        ## Function to retrieve the inverse matrix value from cache
        getinverse <- function() m
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## Computes the inverse of the "special" matrix from the function above
## If the inverse has already been calculated, it retrieves
## this value from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        ## Check if the inverse already exists
        ## and return it if true
        if(!is.null(m)) {
                message("retrieving cached data")
                return(m)
        }
        
        ## Otherwise get the "special" matrix value
        data <- x$get()
        
        ## Compute the inverse of the "special" matrix
        m <- solve(data, ...)
        
        ## Add the inverse matrix value to cache
        x$setinverse(m)
        
        ## Return the inverse matrix value
        m
}
