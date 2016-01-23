## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Here we set the value of the matrix and for compartibility with the second part (cacheSolve()) of this assignment, the matrix x has to be invertible)
## the get() function gets the value of the matrix, we compute the inverse of the matrix

makeCacheMatrix <- function(X = matrix()) {

    inverse <- NULL
    set <- function(y) {
        X <<- Y
        inverse <<- NULL
    }
    get <- function() X
    setinverse <- function(inverse) inverse <<- inverse
    getinverse <- function() inverse
    list(set=set, get=get, 
    setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve function searches the makeMatrix function to check if inverse has already been computed... 
## If the inverse exist, it gets the inverse and skips the computation  .... 
## otherwise it computes the inverse

cacheSolve <- function(X, ...) {
    inverse <- X$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- X$get()
    inverse <- solve(data)
    X$setinverse(inverse)
    inverse
}

        
}
