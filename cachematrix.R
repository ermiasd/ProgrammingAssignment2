## create a special "matrix" object with a list of funtions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(X = matrix()) {
        inv_m <- NULL
        set <- function(y){
          X <<- y
          inv_m <<- NULL
        }
        get <- function() X
        setinverse <- function(inverse) inv_m <<- inverse
        getinverse <- function() inv_m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## calcualte inverse of the "matrix" object created with the above funciton
cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_m <- X$getinverse()
        if(!is.null(inv_m)){      
                ## inverse exists
                message("getting cached data")
                return(inv_m)
                
        }
        ## else, proceed to solve the inverse and cache for future use
        data <- X$get()
        inv_m <- solve(data, ...)
        X$setinverse(inv_m)
}
