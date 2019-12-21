## See comments inline below that gives an overall description of what happens within each function to cache the
## inverse of a matrix

## The first function provides behaviors typical in object oriented program. They are called getters and setters. 
## See inline in the function for more details

makeCacheMatrix <- function(x = matrix()) {
  
  inv_mat <- NULL
  
  ## set() assigns the input argument to the x object in the parent environment
  ## assigns the value of NULL to the inv_mat object in the parent environment. This clears the value of inverse matrix when cached
  ## by a prior execution. So basically, when x is reset, the value of inv_mat cached in the memory of the object is cleared to ensure that
  ## the wrong value is not cached
  set <- function(y){
    x <<- y
    inv_mat <<- NULL
  }
  
  ## Here R retrieves x from the parent environment; given that x is not defined within get()
  ## R retrieves this value from the parent environment
  get <- function() x
  
  ## Since invmat is defined in the parent environment, invmat needs accessed after the setinvmat() is executed. The code uses
  ## << operator to assign the inversed matrix to "invmat" in the parent environment
  setinvmat <- function(solve) inv_mat <<- solve
  
  ## Leverage lexical scoping to access the invmat from the parent environment
  getinvmat <- function() inv_mat
  
  ## this assigns each function as an element and returns to the parent environment
  ## also this allows to use $ extract operator to access functions by name rather than using the [[ operator
  list (set = set, get = get, setinvmat = setinvmat, getinvmat = getinvmat)
}

## Without cacheSolve(), the makeCacheMatrix() function is incomplete. It calculates the inverse matrix from a vector object of type
## makeCacheMatrix();  Call cacheSolve(matrix(1:4, 2, 2)) and see what happens.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## It first checks to get the value from makeCacheMatrix()
  inv_mat <- x$getinvmat()
  
  ## If the value is already in the cache, it does not calculate and instead returns the value from cache
  if(!is.null(inv_mat)) {
    
    message("getting cached data")
    return(inv_mat)
  }
  ## if the value does not exist in cache, R executes program logic to find the inverse of a matrix
  invdata <- x$get()
  inv_mat <- solve(invdata, ...)
  x$setinvmat(inv_mat)
  inv_mat
}
