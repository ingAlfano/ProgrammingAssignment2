# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

## This function creates a special "matrix" object which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y) {
                    x <<- y
                    inv <<- NULL
            }
            get <- function() x
            setInverse <- function(inverse) inv <<- inverse
            getInverse <- function() inv
            list(set = set, get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)
    }
}

## This function computes the inverse of the special "matrix" returned by 
## 'makeCacheMatrix' above. If the inverse has already been calculated
## (and the matrix has not changed), it retrieves the value from the cache.

## This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
		    inv <- x$getInverse()
            if(!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
            }
            data <- x$get()
            inv <- solve(data)
            x$setInverse(inv)
            inv
    }
}