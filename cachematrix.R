## ProgrammingAssignment2
## Adomas Galdikas
## R function that is able to cache matrix reversion computations.

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the reversed matrix
## 4. get the value of the reversed matrix

makeCacheMatrix <- function(x = matrix()) {
      reverse <- NULL
      set <- function(y){
            x <<- y
            reverse <<- NULL
      }
      get <- function() x
      setreverse <- function(rev) reverse <<- rev
      getreverse <- function() reverse
      list(set = set, get = get, 
           setreverse = setreverse,
           getreverse = getreverse, 
           reverse = reverse)
}


##  cacheSolve reverse matrix created with the above function.

cacheSolve <- function(cachedMatrix, ...) {
        ## Return a matrix that is the inverse of 'cachedMatrix'
      reversedMatrix <- cachedMatrix$getreverse()
      if(!is.null(reversedMatrix)){
            message("getting cached matrix")
            return(reversedMatrix)
      }
      data <- cachedMatrix$get()
      reversedMatrix <- solve(data, ...)
      cachedMatrix$setreverse(reversedMatrix)
      reversedMatrix
}

##  runtest runs an example of other functions.

mytest <- function(){
      k<-c(4,2,7,6)
      dim(k)<-c(2,2)
      print("--- Matrix ----")
      print(k)
      ch<-makeCacheMatrix(k)
      print("--- First cacheSolve() run ----")
      print(cacheSolve(ch))
      print("--- Second cacheSolve() run ----")
      print(cacheSolve(ch))
}
