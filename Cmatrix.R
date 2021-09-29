##This program will solve for the inverse of a matrix.
##It involves two functions which are CMC (Cache Matrix Creator) and CS (Cache Solve)


CMC <- function(q = matrix()){
  imbers <- NULL
  sit <- function(w){
    q <<- w
    imbers <<- NULL
  }
  git <- function() {q}
  imbersset <- function(inverse) {imbers <<- inverse}
  imbersget <- function() {imbers}
  list(sit = sit, git = git, imbersset = imbersset, imbersget = imbersget)
}
 ##This is the second function
CS <- function(q, ...){
  imbers <- q$imbersget()
  if(!is.null(imbers)){
    message("getting cached data")
    return(imbers)
  }
  tam <- q$git()
  imbers <- solve(tam, ...)
  q$imbersset(imbers)
  imbers
}