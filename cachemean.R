cacheSolve <- function(x, ...) {
  
  #go into the list, see what the cache says the inverse is
  inverted_mat <- x$getinverse()
  
  #inverted matrix is already in the cache
  if(!is.null(inverted_mat)) {
    message("getting cached inverted matrix")
    return(inverted_mat)
  }
  
  #obtain original matrix from the cache
  un_inv_mat <- x$get()
  
  #solve for inverse of the original matrix 
  inverted_mat <- solve(un_inv_mat, ...)
  
  #put inverted matrix into the cache
  x$setinverse(inverted_mat)
  
  inverted_mat
}