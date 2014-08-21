## JHU R Programming Assignment 2
## makeCacheMatrix will return a matrix object which is invertible.



makeCacheMatrix <- function(x = matrix()) {
    inverse_of_x <- NULL
    set <- function(y) {
        x <<- y
        inverse_of_x <<- NULL
    }
    get <- function() x
    set_inverse <- function(solve) inverse_of_x <<- solve
    get_inverse <- function() inverse_of_x
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## cacheSolve will try to solve the matrix if no cached inversed matrix is found.

cacheSolve <- function(x, ...) {
    inverse_of_x <- x$get_inverse()
    if(!is.null(inverse_of_x)) {
        message("getting cached data")
        return(inverse_of_x)
    }
    data <- x$get()
    inverse_of_x <- solve(data, ...)
    x$set_inverse(inverse_of_x)
    inverse_of_x
}
