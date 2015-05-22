## This R script is for caching a matrix inverse to avoid its repeated computation
## Script has two functions
## 1) For initating the matrix whose inverse to be computed 
## --> --> --> eg: use as : x <- makeCacheMatric(MATRIX_TO_COMPUTE_INVERSE)
## 2) For computing inverse of matrix initiated above as 'x' 
## --> --> --> eg: use as : cacheSolve(x) --> -->to get the inverse

## This function is used to prepare a matirx for computing inverse
makeCacheMatrix <- function(matrix_input = matrix()) {
  
  inverse <- NULL
  set_matrix<- function(mat1)
  {
    matrix_input <<- mat1
    inverse <<- NULL
  }
  get_matrix <- function() matrix_input
  set_inverse <- function(inv) inverse <<- inv
  get_inverse <- function() inverse
  list(set_matrix = set_matrix, get_matrix = get_matrix,set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function calculates the inverse if the inverse is not present in cache
cacheSolve <- function(x, ...) {

  inverse <- x$get_inverse()
  if (!is.null(inverse)){
    message ("returning matrix inverse from cache")
    return(inverse)
  }    
  else{
    GivenMatrix <- x$get_matrix()
    inverse <- solve(GivenMatrix,...)
    x$set_inverse(inverse)
    inverse
    ## 'inverse' Returns a matrix that is the inverse of 'x'
  }
  
}
