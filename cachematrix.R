

## makeCacheMatrix stores a matrix and the cached inverse
## SetMatrix - set the value of the matrix
## GetMatrix - returns the stored matrix
## SetInverse - stores the inverse of the matrix
## GetInverse - returns the inverse

makeCacheMatrix <- function(x = matrix()) {
 # m_cache is the cache, set it to null initially
      m_cache <- NULL
      
    SetMatrix <- function(new_var) {
      x <<- new_var
      # set cache to null as using value of new_var
      m_cache <<- NULL
    }
    GetMatrix <- function(){
      x
    } 
    SetInverse <- function(solve) {
      m_cache <<- solve
    }

    GetInverse <- function() {
      # return from cache
      m_cache
    }
    
    list(SetMatrix = SetMatrix, GetMatrix = GetMatrix,
         SetInverse = SetInverse,
         GetInverse = GetInverse)
  }

## cachesolve checks if the matrix inverse exists in the cache, or creates and stores it


cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
 # m_inv is the matrix inverse
    m_inv <- x$GetInverse()
    #if the inverse already exists in the cache use it
    if(!is.null(m_inv)) {
      message("getting cached data")
      return(m_inv)
    }
    #else 
    data <- x$GetMatrix()
    m_inv <- solve(data, ...)
    # put the solved inverse back into the cache
    x$SetInverse(m_inv)
    m_inv
  }

