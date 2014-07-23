#This script is made up of two parts

#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        m <- x$getmean()
                        
        # Condition to check if there are cached data if so it recovers the data 
        # and avoids to get the inverse of the matrix and prints a message to inform the user
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        #If the inverse is not cached then the inverse is computed and stored in m
        m <- solve(data, ...)
        x$setmean(m)
        m
}