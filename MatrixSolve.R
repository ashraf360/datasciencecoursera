# There will be two functions that are used to cache and solve the inverse of a matrix

makeCacheMatrix <- function(x) {
        m<-NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setCachedInverse <- function(solve) m<<- solve
        getInverse <- function() m
        list(set=set,get=get,setCachedInverse=setCachedInverse,getInverse=getInverse)
}
        

cacheSolve <- function(x, ...){
        
        m<- x$getInverse()
        if (!is.null(m)) {
                message("getting the cached inverse")
                return(m)
        }
        
        data <- x$get()
        message("Calculating the inverse instead of retrieving the cache")
        m <- solve(data, ...)
        x$setCachedInverse(m)
        m
}