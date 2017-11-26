## This R script provides the makeCacheMatrix and cacheSolve that allows the 
## creation of a matrix that can cache its inverse.
## reference: Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
## coursera.org

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL #initializes value of matrix inverse as NULL
    
    #function to set value of matrix object:
    set <- function(y){
        x <<- y
        i <<- NULL  #sets value of matrix inverse as NULL 
                    #for when the matrix is changed/re-set
    }
    
    #function to get value of matrix object:
    get <- function() x 
    
    #function to set/cache the inverse of matrix object
    setinverse <- function(solve) i <<- solve
    
    #function to get cached inverse of matrix object
    getinverse <- function() i
    list (set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then cacheSolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse() #gets the cached inverse of the matrix x
    
    #checks if there's a cached inverse, i.e., cached inverse is not NULL
    if(!is.null(i)) {
        #prints message to console to inform user that there's 
        #a cached inverse and it will be returned
        message("getting cached data")
        return(i)
    }
    data <- x$get()  #gets value of matrix x
    i <- solve(data, ...) #calculates matrix inverse
    x$setinverse(i) #sets value of matrix inverse
    i #returns inverse
}
