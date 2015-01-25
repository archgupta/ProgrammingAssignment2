# Creates a special "matrix" object that can cache inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMat <- matrix(0,0,0)
  
  # setMatrix sets the new matrix and resets the inverse matrix as the inverse needs to be calculated for new data 
  setMatrix<- function(y){
    x <<- y
    inverseMat <<- matrix(0,0,0)
  }
  
  # getMatrix gets the value of matrix.
  getMatrix<-function()x
  
  # setInverse sets the values of inverse matrix 
  setInverse<- function(inverseMatrix){
    inverseMat <<- inverseMatrix
  }
  
  # getInverse gets the values of inverse matrix
  getInverse<-function()inverseMat
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}



## Takes a 'matrix' object as input argument and if the inverse of the matrix has been calculated,
## retrives the inverse from cache. Else, it calculates inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMat <- matX$getInverse()
  
  ## check if inverse has already been calculated. 
  ## if inverse has been clauculated for a given matrix, then fetch the inverse from cache
  if (!(nrow(invMat)==0 && ncol(invMat)==0)){
    print("retrieving cached inverse matrix")
    return(invMat)
  }
  
  
  else{
    # Calculate inverse of the matrix
    calInvMat <- solve(matX$getMatrix(),...)
    matX$setInverse(calInvMat)
    calInvMat
  }
}

