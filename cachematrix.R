#Because computing the inverses of large matrices is a time-consuming process, if the inverse of a given matrix has already
#been computed there is no point to recompute the same inverse. Using the lexical scoping rules of R, one can cache a computed inverse
#matrix and check if a later starting matrix is equivalent to a prior starting matrix. Since, in that case, the resulting inverses
#will be the same, one can simply return the cached inverse, saving the time of recomputation. The following functions accomplish this.

#This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     inverse<-NULL
     set<-function(in){
          x<<-in
          inverse<<-NULL
     }
     get<-function() x
     setInverse <- function(solved) inverse <<- solved
     getInverse <- function() inverse
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#This function computes the inverse of the object returned by the above function. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
     inverse <- x$getInverse()
     if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
     }
     y <- x$getInverse()
     inverse <- solve(y)
     x$setInverse(inverse)
     inverse
}
