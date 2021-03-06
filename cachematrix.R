## Author: Amal Awad
## Creation Date: 12/9/2014
## Purpose: Matrix inversion is usually a costly computation and their may be some
## benefit from caching the inverse of a matrix rather than compute it repeatedly 
## "makeCacheMatrix" and "cacheSolve" functions are used to create a special object 
## that stores a matrix and cache's its inverse

##makeCacheMatrix function creates a special Matrix which is really a list containing functions to
#set the value of the matrix
#get the value of the matrix
#set the inverse of the matrix
#get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    #define a placeholder for the inverse of a matrix
    i <- NULL
    
    #set the matrix 
    set <- function(y)
    {
        x <<- y
        i <<- NULL
    }
    
    #get the matrix
    get <- function() x
    
    #set the inverse of the matrix
    setinverse <- function(inverse) i <<- inverse
    
    #get the inverse of the matrix
    getinverse <- function() i
    
    #return a list of setter/getter functions for matrix and matrix inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##cachesolve function calculates the inverse for the special Matrix created in
#makeCacheMatrix function only if the inverse is not already calculated, otherwise
#the inverse will be returned from cashe and calculation will be skipped 
cacheSolve <- function(x, ...) {
    #get the inverse from cache function
    i <- x$getinverse()
    
    #check if inverse is already in cashe
    if(!is.null(i)) {
        message("getting cached data")
        #return cashed inverse if available
        return(i)
    }
    
    #no inverse found in the cache, it must be calculated
    #get the matrix
    data <- x$get()
    #compute the inverse of the matrix
    i <- solve(data, ...)
    #set the inverse to be cashed
    x$setinverse(i)
    ## Return a matrix that is the inverse of 'x'
    i
       
}

