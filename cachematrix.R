# create a special "matrix" object that can cache its inverse


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # reset cached inverse value
        cached_inverse <- NULL
        
        # reset matrix value
        set_matrix <- function(m) {
                x <<- m
                cached_inverse <<- NULL
        }
        
        # get matrix
        get_matrix <- function() x 
        
        # set/cache inverse
        set_inverse <- function(inverse) cached_inverse <<- inverse
        
        # get cached inverse
        get_inverse <- function() cached_inverse
        
        # return a list of the functions
        list(set_matrix = set_matrix,
             get_matrix = get_matrix,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


# this function computes the inverse of the special "matrix" returned by 
# the makeCacheMatrix() function, above

cacheSolve <- function(x, ...) {
        
        # return the inverse of 'x'
        cached_inverse <- x$get_inverse
        
        # determine if inverse has already been calculated
        if(!is.null(cached_inverse)) {
                message("getting cached data")
                return(cached_inverse)
        }
        
        # otherwise, get original matrix
        data <- x$get_matrix()
        
        # compute the inverse
        inverse <- solve(data, ...)
        
        # cache inverse
        x$set_inverse(inverse)
        
        # return inverse
        inverse
}
