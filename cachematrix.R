## Thhis pair of functions serves to calculate the inverse of a matrix,
## cache the resulting inverted matrix, and then upon subsequent calculations check
## whether the value has previously been calculated, in which case the value is retrieved from cache

## The first function, makeCacheMatrix, creates a special matrix object containung the Get and Set for the original inverted matrices
## This allows the values to be retrieved from Cache

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}

## This function computes the inverse of the matrix. 
## First it checks to see whether the inverse was already calculated and stored in the cache
## If the inverse was already calculated, caccheSolve retrieves the previously stored inverse instead of calculating it again

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
## Return a matrix that is the inverse of 'x'
}
