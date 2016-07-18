## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#[MD] This function provides interfaces to manage matrix and its inverted version ()listed in last row list() command

makeCacheMatrix <- function(x = matrix()) {
  #x_1 - param for inverted matrix
  
  x_1 <- NULL
  #function for setting initial values or reset to new matrix
  set <- function(y) {
    x <<- y
    x_1 <<- NULL
   
  }
  #get matrix 
  get <- function() x
  
  #calculate the inverted matrix of provided  
  set_inverted <- function(x_inv) x_1 <<- x_inv
  
  #return inverted matrix 
  get_inverted <- function() x_1
  
  list(set = set, get = get, set_inverted =  set_inverted, get_inverted = get_inverted)
}


## Write a short comment describing this function
## This function returns inverted version of 'x' matrix. At first by checking if inverted matrix 
##already kept in cache (x_1 value) or using solve() func to calculate inverse
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  
  x_1 <- x$get_inverted()
  if(!is.null(x_1)) {
    message("getting cached data")
    return(x_1)
  }
  data <- x$get()
  x_1 <- solve(data)
  x$set_inverted(x_1)
  return(x_1)
}
##example of using the code:
## create a matrix by any way

mdat <- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 2, byrow = TRUE,
               dimnames = list(c("row1", "row2"),
                               c("C.1", "C.2")))

##then run makeCacheMatrix() at this matrix
test_m <- makeCacheMatrix(mdat)
# and call cacheSolve()
cacheSolve(test_m)
