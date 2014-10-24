## Put comments here that give an overall description of what your
## functions do

## takes a matrix x as parameter

makeCacheMatrix <- function(x = matrix()) {
      solved_m <- NULL
      
      # this function can be used to set the value of the actual matrix
      set <- function(y) {
            x <<- y                 # new value is stored
            solved_m <<- NULL       # cache is deleted
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
                  solved_m <- solve(data) %*% data # calculate the inverse
            } else {
                  # if not computable, throw error
                  stop("Dimensions of the matrix are not equal - please use a squared matrix")
            }
      }
      
      solved_m <- x$getsolve()      # load the cache which can contain a cached solution or null - if null we have to calculate
      
      #check if solve of the matrix is cached, yet
      if(!is.null(solved_m)) {
            message("getting cached data")
            return(solved_m) # return cached matrix
      }
      
      # matrix is not solved and cached, yet, so calculate and save
      data <- x$get()
      solved_m <- resolveMatrix(data, ...) # calculate inverse
      x$setsolve(solved_m) # cache solved inverse
      solved_m    # finally return what's interesting
}
