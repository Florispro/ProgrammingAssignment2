## The two functions below allow to make a cache for matrix inverse
## calculations. If the inverse of a matrix has not been calculated
## yet, the solve function will be executed. The result will be stored
## in the cache. If at a later time the inverse of the matrix is
## again requested, it is not calculated, but retrieved from the cache,
## instead.

## Given a matrix x, this function returns a cache to store the
## matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        # Return a list of functions designed to store and retrieve
        # the matrix and its inverse. Storing is done by using the
        # '<<-' operator, to store the matrix and the inverse
        # in the global environment.
  
        # We need to initialize the inverse variable here, to make
        # sure 'getinverse' will always return something, even
        # if we didn't use 'set' or 'setinverse' yet
        # The matrix x is already initialized by giving it as
        # argument of the function
        inv <- NULL
        
        # Function that resets the cache to a new matrix.
        # inv is set to NULL, since we don't know it's value yet
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Get the original matrix
        get <- function() x
        
        # Store the inverse in the cache
        setinverse <- function(inverse = matrix()) inv <<- inverse
        
        # Get the stored inverse. If not stored yet, returns NULL
        # due to initialization earlier
        getinverse <- function() inv

        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function tries to find the stored inverse of x's matrix,
## or, if not found, calculate the inverse and store it in the
## cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## where 'x' is a cache from the 'makeCacheMatrix'
        ## function
  
        inv <- x$getinverse()
        
        # If a non-null value is returned, the inverse was already
        # saved, and we don't need to calculate it again
        # We escape from the function by returning the stored
        # inverse
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # The cache doesn't have the inverse yet, so we need to
        # calculate it. 
        # We therefore (1) get the matrix from the cache
        # (2) calculate the inverse of the matrix
        # (3) store the result in the cache
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        
        inv  
}