#' mvls
#'
#' Takes a mvls object and give a plot about cluster distribution
#'
#' @author Lorenzo Querci <lorenzo.querci@studio.unibo.it>
#'
#' @param mvls A object class mvls
#'
#' @return Plot
#'
#' @export

visualdiagmvls<-function(mvls){
  matrix<-mvls$matrix
  Cluster<-as.character(as.vector(mvls$cluster[,1]))
  id<-seq(1,dim(matrix)[1], by=1)
  matrix<-data.frame(id,matrix,Cluster)
  matrix.ggplot<-reshape(na.omit(matrix),idvar ="id", varying=list(2:(dim(matrix)[2]-1)), direction = "long")
  ggplot(data=matrix.ggplot, aes(x=time, y=a, colour=Cluster, group=id))+geom_line(alpha=.5)+ggtitle("Distribuzione dei pattern")+labs (x="Time", y = "Values")+theme_classic()
}
