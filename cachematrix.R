#This script is made up of two parts

#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        m <- x$getinv()
                        
        # Condition to check if there are cached data of the inverse if so it prints a
        # message to inform the user and then shows the result of the inverse
       
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # Obtaining data, the original matrix
        data <- x$get()
        #Computation of the inverse and then it is stored (cached) for the future
        m <- solve(data, ...)
        x$setinv(m)
        #Printing the result
        m
}