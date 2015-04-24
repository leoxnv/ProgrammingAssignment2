## This functions create an object that create an object that stores a matrix 
## and cache's its inverse

## The first function "makeCacheMatrix" creates a list of four functions that:
##1) set the value of the matrix
##2) get the matrix
##3) set the value inverse of the matrix
##4) get the value inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        ## creates an empty object that will contain the inverse of the matrix        
                s <- NULL
        ##sets the value of the matrix inside  the function       
                set <- function(y) {
                        x <<- y
                        s <<- NULL
                }
        ##assings the value of the matrix to the "get" object of the list
                get <- function() x
        ##sets the value of the inverse to the "setinverse" object of the list
                setinverse <- function(solve) s <<- solve
        ##assings the value of the inverse to the "getinverse" object of the list
                getinverse <- function() s
        ##returns the list of functions
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)  
}


## The following function evaluates if the inverse of the matrix has been 
##alrready calculated. If so, it returns the inverse of the matrix stored in
##the cache without calculating it. Otherwise, it calcules the inverse of the matrix
## and sets the inverse in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ##sets te value of the function getinverse to s
        s <- x$getinverse()
        ##evaluates if getinverse is different to Null, if so it returns the 
        ##the inverse in the cache
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        } 
        ##if not, it calculates the inverse… 
        data <- x$get()
        s <- solve(data, ...)
        ##and stores in the set inverse function
        x$setinverse(s)
        ## and finally it returns the object s (the inverse of the matrix)
        s
}
