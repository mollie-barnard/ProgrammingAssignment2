############
## The pair of functions created below return 
## the inverse of a matrix. 
## If the inverse of the matrix was previously 
## calculated it is retreived from the the cache 
## (i.e., not recalculated.) 

## I assume that input argument y is a square invertible matrix. 
############

############
## Function 1
## This function creates a special "matrix" 
## object that can cache its inverse.
############

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <- y #assign input argument y to x object in parent environment. 
        i <<- NULL #clear values of i cached by prior execuctions of cachesolve
    }
    get <- function() x #retrieving x from parent environment makeCacheMatrix
    setinv <- function(solve) i <<- solve
    getinv <- function()i
    list(set=set, get=get,
         setinv=setinv, 
         getinv=getinv)
      }

#############
## Function 2
## This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. 
##If the inverse has already been calculated 
##(and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.
#############

cacheSolve <- function(x, ...) {
    i <- x$getinv() #try to retreive inverse matrix from makeVector
    #if value is not null, we have a valid, cached inverse matrix 
    #and can return it to the parent environment
    if(!is.null(i)){
      message("getting cached data")
      return(i)
    }
    #if value is null, get the matrix from the input object, calculate
    #inverse and use set(inv). Return value of inverse matrix to parent
    #environment
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
