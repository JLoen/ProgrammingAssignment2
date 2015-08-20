## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix: This function creates a special "matrix" object that can 
# cache its inverse.makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function()x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Calculate the inverse of a matrix
## If the inverse was calculated before, return cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getinverse()
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data, ...)
        x$setinverse(inver)
        return(inver)
}
