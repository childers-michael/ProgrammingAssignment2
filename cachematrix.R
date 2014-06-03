## These functions allow an R user to have matrices that
## store a cached version of their inverse. This may save
## processing time if the user needs to access the inverse
## of a particular matrix frequently, put does not need to
## change the matrix often.

## This function creates the CacheMatrix object. It
## includes methods to:
## 1. set the value of the object
## 2. get the value of the object
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of the CacheMatrix
## and stores it in the object. However, it first checks to
## see if the object has already cached its inverse, in
## which case it just returns the cached inverse.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
