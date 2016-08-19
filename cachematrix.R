## getting inverse by solve() function by checking chached data first, if not exists
## inverse will be calculated

## makeChcheMatrix is to set matrix 'x' to be calculated by cacheSolve
## or set invesre directly in variable 'm'

makeCacheMatrix <- function(x = matrix()) {
	##initialize 'm' with null value
        m <- NULL
		
	##function to set 'x' value
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		
	##function to get value of 'x'
        get <- function() x
		
	##function to set 'm' value with inverse
        setsolve <- function(solve) m <<- solve
		
	##function to get saved inverse in variable 'm'
        getsolve <- function() m
		
	##lising function
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve to check whether inverse is set or calculate it 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        
		## if makeCacheMatrix$setsolve has value then return it's saved value
		if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
	##if makeCacheMatrix$setsolve DOES NOT have value then calculate inverse
	data <- x$get()
        m <- solve(data)
        x$setsolve(m)
        m
}
