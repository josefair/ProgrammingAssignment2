## This function creates a special "matrix" object that can cache its inverse.

## The first function, makeChache Matrix creates a special "Matrix"
## It is a list containing a function to:
        ##1.set the value of the Matrix
        ##2.get the value of the Matrix
        ##3.set the value of the inversedMatrix calling solve function
        ##4.get the value of the inversedMatrix


makeCacheMatrix <- function(x = matrix()) {

        m_x <- NULL
        set <- function(y) {
                x <<- y
                m_x <<- NULL
        }
        get <- function() x
        setinverse_m_x <- function(solve) m_x <<- solve  ##this line calls the solve function to inverse m_x
        getinverse_m_x <- function() m_x
        list(set = set, get = get,
             setinverse_m_x = setinverse_m_x,
             getinverse_m_x = getinverse_m_x)
}


## Function cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse Matrix has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inversed Matrix from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m_x <- x$getinverse_m_x()
        
        ## This conditional statement will look if the matrix was already inversed and return the cached data
        if(!is.null(m_x)) {
                message("getting cached data")
                return(m_x)
        }
        data <- x$get()
        m_x <- solve(data, ...)
        x$setinverse_m_x(m_x)
        m_x
        
}
