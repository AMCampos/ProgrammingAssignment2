## This two functions allow us to optimize a usually costly
## computation like matrix inversion.

## This function creates a special "matrix" object that can cache
## its inverse, so we don't need to repeat the computation

makeCacheMatrix <- function(x = matrix()) {

     #We create a variable 'minv' with a null value:
     minv <- NULL
     
     # We create the anonymous functions we need:
     #
     # The first anonymous function is 'set'. This function stores
     # the value of argument 'y' in the variable 'x' (this variable 
     # belongs to the makeCacheMatrix environment), and stores the
     # 'NULL' value to minv (also is a variable in makeCacheMatrix
     # environment):
     #
     set <- function(y){
          x <<- y
          minv <<- NULL
     }
     
     # Anonymous function: return the value of x:
     get <- function() x
     
     # Anonymous function: stores the value of solve (it will be the
     # inverse matrix calculates by cacheSolve) in minv:
     setinv <- function(solve) minv<<- solve
     
     # Anonymous function:return the value of minv:
     getinv <- function() minv
     
     # Here, the list with the function's results we return as 
     # special matrix:
     list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function calculates the inverse of the special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     
     # Here, we use the function 'getinv' of the special matrix
     # object to store its result in 'msol':
     msol <- x$getinv()
     
     # We check if the msol value is 'NULL'. If it isn't, then
     # we show a message, get the cached matrix, and return it:
     if (!is.null(msol)){
          message("getting cached data")
          return (msol)
     }
     
     # If the value of msol was 'NULL' we store the original
     # matrix in a variable ('data'), calculate its inverse, and 
     # stores the result in 'msol':
     
     data <- x$get()
     msol<-solve(data,...)
     
     # Once we have the result, we use 'setinv' to stores its 
     # value in the variable 'minv' in makeCacheMatrix's environment
     # in order to avoid to repeat the computation:
     x$setinv(msol)
     
     # Now, we return the result:
     msol
}
