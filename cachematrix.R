## The following functions find the inverse of a matrix and creates a
## special matrix to cache the inverse as to save computation in the future.
## Note: I did use Coursera course's makeVector and cacheMean as an outline.

## makeCacheMatrix creates a special "matrix" that caches its inverse in the 
## parent function. When first called, the inverse is set to null and a list
## containing four functions (set, get, setinv, getinv) is returned. After 
## cacheSolve has been called the inverse (i) will be cached.

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## cacheSolve takes a makeCacheMatix and returns its inverse. To do this, it 
## first checks to see if the inverse has already been computed, if so it simply
## returns the inverse. Otherwise, it will find the inverse by calling solve and
## then saves the inverse in the makeCacheMatix by calling setinv. Then the next
## time the same matrix is called, the function will not have to compute it since
## it will now be cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
