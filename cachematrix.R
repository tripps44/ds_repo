# ---------- 1 ----------

# Note: <<- operator is used to assign a value to an object in an environment 
# that is different from the current environment

makeCacheMatrix <- function(x = matrix()) 
        # This function creates a special "matrix" object that can cache its inverse
{
        m <- NULL
        
        set <- function(y) 
        {
                x <<- y
                m <<- NULL
        }        

        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        
        list(set = set, 
             get = get, 
             setmatrix = setmatrix, 
             getmatrix = getmatrix)
        
}

# ---------- 2 ----------
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
# should retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) 
        # Return a matrix that is the inverse of 'x'
{
        
        m <- x$getmatrix()
        if(!is.null(m)) 
        {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
