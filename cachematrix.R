
# Overall, following two finctions cache the inverse of a given matrix


# Function "makeCacheMatrix" set/get value of the matrix and also
# set/get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


# Function "cacheSolve" returns the inverse of matrix. First, function 
# check is it the inverse matrix computed. Then, if is so, it get result
# and skips computation. Second, it computes inverse matrix, and storing 
# the value using setinvers function.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data ...")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
        
}
