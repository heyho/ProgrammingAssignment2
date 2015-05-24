## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    #initialize the inverse matrix as NULL 
    #(so even if set() is not run - x was not passed as an arg - it will be NULL)
    inverse_x <- NULL
    
    #overwrite x with the argument and set inverse matrix to NULL 
    set <- function(y) {
        x <<- y
        inverse_x <<- NULL
    }
    
    #just return the matrix data
    get <- function() x
    
    #set the inverser matrix
    setinverse <- function(inverse) inverse_x <<- inverse
    
    #return the inverse matrix
    getinverse <- function() inverse_x

    #create the actual "object"
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    #get the inverse matrix and see if it is NULL
    inverse_x = x$getinverse()
    
    #If it's not
    if(!is.null(inverse_x)){
        message("Getting cached inversed matrix")
        #Just return it
        return(inverse_x)
    } else {
        #If inverse matrix is NULL
        #get the matrix data
        x_data = x$get()
        #get the inverse of the matrix
        inverse_x = solve(x_data)
        #set the inverse (write it to the cache)
        x$setinverse(inverse_x)
        #return the inverse
        return(inverse_x)
    }
}
