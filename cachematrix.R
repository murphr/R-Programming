## makeCacheMatrix builds a matrix by:
## 1) setting the matrix value
## 2) getting the matrix value
## 3) setting the inverse matrix value
## 4) getting the inverse

## example:
## x <- matrix(data,nrow) # create matrix
## xinv <- makeCacheMatrix(x) # cache matrix


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y           
                m <<- NULL        
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve will return the cached inverse matrix
## example:
## xinv$get()                 # return matrix
## cacheSolve(xinv)           # return inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)        
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
