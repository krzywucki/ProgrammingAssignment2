## The script provides a couple of functions to calculate and cache an inverse of a
## given matrix.

## makeCacheMatrix function provides caching capabilities for computing an inverse of
## a given matrix.
makeCacheMatrix <- function(x=matrix()){
    i <- NULL
    set <- function(y){
        x<<-y
        rev<<-NULL
    }
    get <- function(){
        x
    }
    setInverse <- function(theInv) i <<- theInv
    getInverse <- function()  i
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


## cacheSolve function benefits from caching feature of makeCacheMatrix to store 
## an inverted matrix in the cache after it is computed.

cacheSolve <- function(x, ...){
   inverted <- x$getInverse()
   if(!is.null(inverted)){
       message("getting cached inverted matrix")
       return(inverted)
   }
   inverted <- solve(x$get(), ...)
   x$setInverse(inverted)
   inverted
}
