##This program will solve for the inverse of a matrix.
##It involves two functions which are CMC (Cache Matrix Creator) and CS (Cache Solve)

makeCacheMatrix <- function(q = matrix()) {
  imbers <- NULL
    get <- function() q
    set <- function(w) {
      q <<- w
      imbers <<- NULL
    }
    getimbers <- function() imbers
    setimbers <- function(inverse) imbers <<- inverse
    list(get=get, set=set, getimbers=getimbers, setimbers=setimbers)
}

##This is the second function
cacheSolve <- function(q, ...) {
  imbers <- q$getimbers()
  if (!is.null(imbers)) {
    message("inverse is cached")
    return(imbers)
  }
  m <- q$get()
  imbers <- solve(m, ...)
  q$setimbers(imbers)
  return(imbers)
}