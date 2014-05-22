## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## initialize the inverse matrix value
        inverse <- NULL
        
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        ## get the value of the matrix
        get <- function() x
        
        ## set the value of the inverse
        set_inverse <- function(inv_input) inverse <<- inv_input
        ## get the value of the inverse
        get_inverse <- function() inverse
        
        ## return a list of all the above functions
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)

}


## Write a short comment describing this function

## The following function calculates the inverse of the special 
## "matrix" created with the above function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache via 
## the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## check if the inverse is already cached,
        ## if so, we get the inverse from the cache directly
        inverse <- x$get_inverse()
        if(!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
        }
        ## else, get the matrix
        data <- x$get()
        ## calculate the inverse
        inverse <- solve(data, ...)
        ## next, cache the inverse of the matrix
        x$set_inverse(inverse)
        ## return the result
        inverse
}
