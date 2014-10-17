## The following 2 functions are used in conjunction to store and retreive computationally
## intensive results by using a 'cache'. The values obtained after computationally intensive
## values are operations are stored in local variables. The next time there is a need for
## these values, they are retreived from this cache instead of doing the whole operation again

## makeCacheMatrix function contains a list of functions to read and write the matrix and to
## read and write the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
  
  cacheValue <- NULL #initializing the cache value to NULL
  
  #Takes a matrix as input and stores it in x.
  #Since a new matrix is being set, the cache is set to NULL
  setMatrix <- function(inputMatrix)
  {
    x <<- inputMatrix
    cacheValue <<- NULL
  }
  
  #returns the matrix stored by setMatrix
  getMatrix <- function()
  {
    x
  }
  
  #Assigns the function arguement(which is matrix inverse) to the cache value
  #Ideally, this function should only be called by cacheSolve function below after calculating the inverse
  #Else, it would be possible to manually set the inverse value (which would be wrong) instead of 
  #having cacheSolve calculate the inverse and then assign it to cacheValue
  setInverse <- function(inverse)
  {
    cacheValue <<- inverse
  }
  
  #returns the cached value set by setInverse
  getInverse <- function()
  {
    cacheValue
  }
  
  #list of functions. The elements of the list refer to functions defined above.
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)

}

##--------------------------------------------------------------------------


##cacheSolve returns the inverse of the matrix prepared by makeCacheMatrix

cacheSolve <- function(x1, ...) 
{
        
  matrixInverse <- x1$getInverse()
  
  if(!is.null(matrixInverse)) #if not null, it means a cached value exits
  {
    message("getting cached data")
    return(matrixInverse)
  }
  
  #No cached value exists. Therefore, get the matrix, calculate the inverse and set the cache
  data <- x1$getMatrix()
  
  matrixInverse <- solve(data) #getting the inverse value
  x1$setInverse(matrixInverse)
  
  matrixInverse #return the inverse
}
