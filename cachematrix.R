## Coursera DataScience Specialization
## Course: R Programming
## Assigment: Programming Assignment 2: Lexical Scoping
## Student: Marco A. Silva Reyes

## In this script we will have two functions that  will help us to 
## create the inverse matrix from a given matrix


## makeCacheMatrix() will initialize the internal matrix "x"
## and the functions that will help us manage the matrix and
## the inverse matrix "inv". Both matrix x and inv are in the scope
## or makeCacheMatrix, and the function will help us to return the value 
## or set new value to these variables.

makeCacheMatrix <- function(x = matrix()) {
        ## initializing a null inv matrix
        inv<-NULL
        
        ## Function for setting new values to the original matrix
        set <- function(y) {
                ## Giving the new values to the x matrix 
                x <<- y
                ## seting null to the inverse since we have a new matrix
                inv <<- NULL
        }
        
        ## Function for returning the original matrix 
        get <- function() x
        
        ## Function for setting new values for the inverse matrix 
        ## we are not calculating the new inverse        
        setinv <- function(inverse) inv <<- inverse
        
        ## Function for returning the inverse matrix 
        getinv <- function() inv
        
        ## returning the function list
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve() will calculate the inverse matrix of a given matrix
## it needs to run makeCacheMatrix first 

cacheSolve <- function(x, ...) {
        ## get the matrix that is the inverse of 'x' 
        i <- x$getinv()
        
        ## check if the inverse exist 
        ## remember that if it is the first time that we are executing the cacheSolve() 
        ## it will be null, also if it is the first time that we executing the cacheSolve() 
        ## after executin x$get().
        if(!is.null(i)) {
                message("Getting cached data")
                return(i)
        }
        
        ##if i is null
        message("Calculating data")
        
        ##getting the original matrix
        data <- x$get()
        
        ##calculating the inverse with solve
        i <- solve(data, ...)
        
        ## returning the resul to the origin
        x$setinv(i)
        
        ## showing the inverse
        i
}
