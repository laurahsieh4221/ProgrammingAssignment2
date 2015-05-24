##These functions will be used to calculate the inverse matrix from the cache and will 
##return the cached version if calculated


##The first function will have a inevertible matrix as an input, and make a list of 
##functions to store the matrix as an object in the cache

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(x) {
                x <<- y
                i <<- NULL
        }
        get <- function() x 
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i 
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##The second function will use the output of the first function to calculate and output
##the inverse matrix if it has not been calculated yet, or if it already has been
##calculated then the second function will output the cached inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return (i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
}
