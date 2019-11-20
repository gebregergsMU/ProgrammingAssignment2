makeCacheMatrix<-function(x=matrix())
{
inverse=NULL
set_data=function(y)
{
x<<-as.matrix(y)
inverse<<-NULL
}
getMatrix<-function()x
setInvers<-function(m)inverse<<-m
getInverse<-function()inverse
list(set_data=set_data,getMatrix=getMatrix,setInvers=setInvers,getInverse=getInverse)
}

cacheSolve<-function(x,...)
{
inverse<-x$getInverse()

if(!is.null(inverse[1,1]))
{
message("The inverse is already calculated")
return(inverse)
}
data=x$getMatrix()
inverse=solve(data,...)
x$getInverse()
inverse
}

qq=makeCacheMatrix(x=matrix(c(1,1,2,3,4,7,3,3,7),3))
qq$getMatrix()
cacheSolve(qq)
