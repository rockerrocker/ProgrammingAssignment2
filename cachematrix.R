#### The two functions contained in this file calculate the inverse
## of a supplied matrix.
## In order to avoid unnecessary repeat computation of the same matrices ## the results are cached.
## It is assumed that the supplied matrix is invertible.

#### Function makeCacheMatrix ####
## The following function creates a special "matrix", which is
## really a list containing a function to the following:-
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse matrix
## 4) get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    
}

#### Function cacheSolve ####
## The following function calculates the inverse of the special "matrix" 
## created by the makeCacheMatrix function. However, it first checks to ## see if the inverse matrix has already been calculated.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m     
}
