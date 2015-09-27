## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##the function below summarily cache other functions, that
##once called inside the cacheSolve function can avoid rework, since the inverse is
##already caculated.

makeCacheMatrix <- function(t = matrix()) {
    inv<-NULL
    set<-function(z){
        t<<-z
        inv<<-NULL
    }
    get<-function()t
    setinv<-function(inverse) inv<<-inverse
    getinv<-function()inv
    list(set=set, get=get, setinv=setinv,getinv=getinv)
}



cacheSolve <- function(t, ...) {
        ## Return a matrix that is the inverse of 't'
    inv<-t$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    #otherwise calculates the inverse matrix
    mt<-t$get()
    inv<-solve(mt,...)
    t$setinv(inv)
    return(inv)
}
