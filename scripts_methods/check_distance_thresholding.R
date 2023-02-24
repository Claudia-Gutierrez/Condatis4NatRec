x<-1:4

dm<-dist(x)#is a distance object, not a matrix

alpha <- 2/0.5
norm <- alpha^2/2/pi 


maxdisp<-2


#Current between cells
Cfree <- norm*exp(-alpha*as.matrix(dm))

diag(Cfree) <- 0

Cfree#is 4x4 matrix

test<-as.matrix(dm)>=maxdisp#is 4x4 matrix

 if(maxdisp!=Inf){
 Cfree[as.matrix(dm)>=maxdisp] <- 0
 }

Cfree#is still 4x4 matrix, low values have been replaced with zeros

