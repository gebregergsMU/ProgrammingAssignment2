## makeCacheMatrix creates inverted matrix and returns list of functions
##cacheSolve calcluates the inverted matrix created by makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {     
        # initialize to NULL
        cache=NULL

        # To create the matrix
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }

        # get the value of the matrix
        get <- function() x
        # To invert the matrix and store it in cache
        setMatrix <- function(inverse) cache <<- inverse
        # To get the inverted matrix from cache
        getInverse <- function() cache

        # to return the created functions
        list(set = set, get = get,setMatrix = setMatrix,getInverse = getInverse)
}


## cacheSolve calcluates the inverted matrix created by makeCacheMatrix
## If the inverted matrix does not exist in cache then creates, and it's inverted value is stored in cache

cacheSolve <- function(x, ...) {
        ## finds the inverse of the matrix stored in cache
        cache <- x$getInverse()

        ## if the inverted matrix exits in cache it return is otherwise creates
        if (!is.null(cache)) {
                message("getting cached data")

                return(cache)
        }

        # create matrix since it does not exist
        matrix <- x$get()
         cache <- solve(matrix, ...)
      # set inverted matrix in cache

             x$setMatrix(cache)
              return (cache)

       }
## Demonstartion
#qq <- makeCacheMatrix()
#qq$set(matrix(c(1,1,2,3,4,7,3,3,7),3))
#cacheSolve(qq)


