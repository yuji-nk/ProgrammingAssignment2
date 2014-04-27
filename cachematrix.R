#####################################################
#
# I created the following functions using makeVector
# and chasemean in the example as a reference.
#
# The function "set" in makeVector has no role,
# Thus, I do not make the corresponding part 
#  in makeChaseMatrix.
#
#####################################################
## 
## makeVector creates a list 
## containing 
## a matrix named get 
## and functions
## named setSolve
## named getSolve

makeCacheMatrix <- function(x = matrix()) {
        #assign NULL to inv
        inv <- NULL
        
        #assign x(matrix to be inverted) to get
        #
        # I think 
        # no need to create "get" as a function
        get <- x

        #assign Inv globally to inv
        setSolve <- function(Inv) inv <<- Inv

        getSolve <- function() inv
        # as a result, getSolve has inv as its value
        
        #return the following list
        list(get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

## cacheSolve calculates the inverse of a matrix that is x$get 
## if x$getSolve() in empty.
##
## otherwise, returns x$getSolve() 

cacheSolve <- function(x, ...) {
        #assign x$getSolve() to inv
        inv <- x$getSolve()

        if(!is.null(inv)) {
        #if inv is not null,
        		 #return the next message
                message("getting cached data")
                #return the value of inv
                return(inv)

        }else{
                #assign x$get to data
                data <- x$get
                #make inverse of data,
                # and assign it to inv
                inv <- solve(data, ...)

                #execute setSolve(inv) in x
                x$setSolve(inv)
                
                #return the value of inv
                return(inv)
        }
}
