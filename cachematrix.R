## Whith the cache matrix we create a function that allow to inverse the matrix in an easy way and it´s not time consuming
##To do this first we create a special "matrix" and in the second function that works in conjuctions the other function do the inverse of the special "matrix"

makeCacheMatrix<-function(x=matrix()){
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function(){x}
    setInverse<-function(inverse) {inv<<-inverse}
    getInverse<-function() {inv}
    list(set = set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## cacheSolve do the inverse of the special matrix
cacheSolve <- function(x, ...){
    inv<- x$getInverse()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    mat<-x$get()
    inv<-solve(mat, ...)
    x$setInverse(inv)
    inv
}
