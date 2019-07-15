
# this function creates a matrix object that can be inverse
makeCacheMatrix <- function(x=matrix()){
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(x) m <- solve(x)
        getInverse <- function() m
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

# the following function computes the invers of the matrix that returned by function makeCacheMatrix()
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)){
                massage('getting cashed data')
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInvers(m)
        m
}