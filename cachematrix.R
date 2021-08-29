## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
### Function to create a list of 4 functions, used to store and retrieve the results of matrix inversion.
### The list contains 4 functions: $set, $get, $setinvert and $getinvert that setup the variables environment and 
### that will be called by the main function to store the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL   # initialization of variable as NULL
    set <- function(y) {   # function "set" that assigns a new matrix value in the parent environment (<<-)
      x <<- y
      inv <<- NULL
    }
    get <- function() x   # function used by the main function to parse the data
    setinvert <- function(invert) inv <<- invert   # function to store the calculated inverted matrix
    getinvert <- function() inv   # function to call the stored inverted matrix
    list(set = set, get = get, setinvert = setinvert, getinvert = getinvert)    # code required to enable the "$"
                                                                                # when calling the 4 functions
}


## Write a short comment describing this function
### This function uses the small "library" of functions included in MakeCacheMatrix to retrieve the data
### and calculate the inverse of a given matrix. Then it stores the results.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinvert()   # this calls the stored value of the inverted matrix
  if(!is.null(inv)) {    # if a non-null value is found, the value is returned without calculating it again
    message("getting cached data")
    return(inv)
  }
  data <- x$get()       # the data (matrix) is retrieved
  inv <- solve(data, ...) # the inverse is calculated and store in the variable "inv"
  x$setinvert(inv)    # the result is then stored (in the parent environment, using the "setinvert" function
                      # created by MakeCacheMatrix)
  inv  # the result is returned
}
