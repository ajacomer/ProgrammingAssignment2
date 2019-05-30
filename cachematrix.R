## The overall is a funcrion that cache the inverse of a matrix avoiding to be
## recalculated when the elements of the matrix being inverted don't change

## set and get the value of the matrix and the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)
}


## The function returns thr inverse of the matrix but first checks if the
## inverse have already computed. In the event that has been calculated it skip
## the computation. if not, the function computes the inverse and sets the value
## in the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
