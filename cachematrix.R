## Two functions. first function creates the matrix. 
## In order to work it needs a function, which can be created with matrix(data,nrow,crow). The data need to be inserted with c()

## First function is creating the matrix and setting up the lexical scoping. 

makeCacheMatrix <- function(x = matrix()) {
        S <- NULL
        set <- function(y){
                x <<- y
                S <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) S <<- solve
        getSolve <- function() S
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## Second function creates the inverse and delivers it back to construct from the first function. 

cacheSolve <- function(x, ...) {
        S <- x$getSolve()
        if(!is.null(S)){
                message("getting cached data")
                return(S)
        }
        data <- x$get()
        S <- solve(data,...)
        x$setSolve(S)
        S
}
