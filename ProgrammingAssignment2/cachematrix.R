## The code below creates a special matrix object and then calculates 
## and caches its inverse matrix 
 

## The makeCacheMatrix function creates a special "matrix" with the form 
## of a list , which stores the value of a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
         i <- NULL
         
         set <- function(y){
           
           x <<- y
           
           i <<- NULL
           
         }
         get <- function() x
         
         setinverse <- function(solve) i <<- solve
         
         getinverse <- function() i
         
         list(set=set, get=get, setinverse=setinverse, getinverse=getinverse )
         
         
}


## The cacheSolve function computes the inverse of the "matrix" created 
## from the previous function.In case of an inverse matrix that has been
## already calculated , it gets the cached inverse without computation.
## Otherwise, it computes the inverse of the matrix and stores it in the 
## cache via the setinverse function  


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        i <- x$getinverse()
      
        if(!is.null(i)) {
                message("getting cached matrix")
                return(i)
        }
        mat <- x$get()
        
        i <- solve(mat,...)
        
        x$setinverse(i)
        
        i

}
