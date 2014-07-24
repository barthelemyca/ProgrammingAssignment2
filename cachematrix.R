


## this function is used to store a matrix and 
## store its inverse (if needed). 
## Accessing the value of the matrix and its
## inverse are done by a list of 4 functions 
## returned by makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, ## function used to access the matrix 
             get = get, ## function used to change the matrix
             setinv = setinv, ## function used to access the inverse
             getinv = getinv ## function used to chnage the inverse
             )
}


## this function return the cached inverse matrix. If it is missing, it 
## is recomputed on the fly and then returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m) # if inverse already cached, return it
        }
        # if inverse not cached yet, compute it, cache it and then return the result
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m	       
}
