## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #assign the matrix inverse as null
        inv <- NULL
        
        #set matrix x to new matrix y if matrix changes
        set <- function(y){
                x <<- y
                inv <<- NULL #make inv NULL again if matrix changes
                
        }
        
        #get the new matrix x
        get <- function(){
                return(x)
        }
        
        #set the inverse if it's not in the cache
        setInverse <- function(inverse){
                inv <<- inverse
        }
        
        #get the cached inverse
        getInverse <- function(){
                return(inv)
        }
        
        #return list of the functions
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
        

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #first check if the inverse exists
        inv <- x$getInverse()
        
        if(!is.null(inv)){
                message("Retrieving the cached inverse")
                return(inv)
        }
        
        #if there is no inverse in the cache, then find the inverse
        theMatrix <- x$get() #get the matrix. This is the new matrix y, which we assignned to x
        
        inv <- solve(theMatrix, ...) #find its inverse
        x$setInverse(inv) #cache this inverse
        
        return(inv) #return the inverse
        
}
