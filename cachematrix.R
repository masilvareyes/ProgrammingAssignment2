## In this script we will have two functions that  will help us to 
## create the inverted matrix from a given matrix


## makeCacheMatrix will initialize the internal matrix "x"
## and the functions that will help us manage the matrix and
## the inverted matrix "inv". Both matrix x and inv are in the scope
## or makeCacheMatrix, and the function will help us to return the value 
## or se new value to these variables.

makeCacheMatrix <- function(x = matrix()) {
        ## initializing the inv matrix
        inv<-NULL
        
        ## seting new values to the original matrix
        set <- function(y) {
                ## Giving the new values to the x matrix 
                x <<- y
                ## seting null to the inverted since we have a new matrix
                inv <<- NULL
        }
        
        ## returning the original matrix 
        get <- function() x
        
        ## we are not calculating the new inverted only 
        ## settin new values for the inverted matrix
        setinv <- function(inverted) inv <<- inverted
        
        ## returning the inverted matrix 
        getinv <- function() inv
        
        ## returning the function list
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The cacheSolve will calculate the inverted matrix of a given matrix
## it needs to run makeCacheMatrix first 

cacheSolve <- function(x, ...) {
        ## get the matrix that is the inverse of 'x' 
        i <- x$getinv()
        
        ## check if the inverted exist 
        ## remember that if the first time that we are execute the cacheSolve 
        ## i will be null, also if is the first time that we execute the cacheSolve 
        ## after executin x$get().
        if(!is.null(i)) {
                message("Getting cached data")
                return(i)
        }
        
        ##if i is null
        message("Calculating data")
        
        ##getting the original matrix
        data <- x$get()
        
        ##calculating the inverted with solve
        i <- solve(data, ...)
        
        ## returning the resul to the origin
        x$setinv(i)
        
        ## showing the inverted
        i
}
