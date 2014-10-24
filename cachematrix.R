## Put comments here that give an overall description of what your
## functions do

## takes a matrix x as parameter

makeCacheMatrix <- function(x = matrix()) {
        solved_m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # define the possible functions
        get <- function() x
        setsolve <- function(mean) solved_m <<- mean
        getsolve <- function() solved_m
        
        # return the functions as a list, so you can run the different functions "on" this special matrix-object
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        # using my own function to solve a matrix
        resolveMatrix <- function(data) {
                # check if matrix is n*n dimensional
                if (dim(data)[1] == dim(data)[2]) {
                        
                } else {
                        
                }
        }

        solved_m <- x$getsolve()
        
        #check if solve of the matrix is cached, yet
        if(!is.null(solved_m)) {
                message("getting cached data")
                return(solved_m) # return cached matrix
        }
        
        # matrix is not solved and cached, yet, so calculate and save
        data <- x$get()
        solved_m <- resolveMatrix(data, ...)
        x$setsolve(solved_m)
        solved_m
}
