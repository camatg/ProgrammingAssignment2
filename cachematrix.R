## The functions above calculates the inverse of a matrix introduced and
## safe it in the cache and if you need the inverse matrix it retrieves
## from the cache so the calculation don’t have to me made again. 

## This function creates a list with the functions that will be used 
## in the cacheSolve function and saves the matrix that will be inverted

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks if there is already calculated the inverted matrix
## in the makeCacheMatrix object,if it is, it returns the result with a
## message telling the matrix is retrieved from the cache, if not,
## it calculates the inverse and save it in the makeCacheMatrix object

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}