## Put comments here that give an overall description of what your
## functions do

## This function creates a special vector, which is really a list containg a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. get the value of the inverse of the matrix
## 4. set the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
        
        ## initialize the "global variable to null"
        m <- NULL
        
        ## function to set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## function to get the value of the matrix
        get <- function() x
        
        ## function to set the inverse into the global variable
        setinverse <- function(inverse) m <<- inverse
        
        ## function to get the inverse
        getinverse <- function() m
        
        ## construct a list of the functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## this function computes the inverse of a matrix. 
## it caches the value of the inverse
## if asked to re-compute the inverse, it returns the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
