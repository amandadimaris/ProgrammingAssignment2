## The following functions are used to create a special object that stores a matrix and cache's its inverse. 

## The first function, makeCacheMatrix creates a special "matriz", which is really a list containing a function to 
  #1.set the matrix 2.get the matrix  3.set the inverse  4.get the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)      
        
}

## The following function calculates the inverse of the special "matrix" created with the above function. 
#However, it first checks to see if the inverse has already been calculated. If so, it gets the matriz from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix and sets the inverse in the cache via the setinverse function.

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
