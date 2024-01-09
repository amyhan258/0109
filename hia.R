b <-data.table(date=c(1,5,14,2,8,16),du=c(2,5,3,4,4,5),personanddrug=c(1,1,1,2,2,2))

k<-cbind(b[rep(c(1:6),b$du),],mapply(function(x,y){return(c(x:(x+y-1)))},b$date,b$du)%>% unlist())

k[,sum(c(date[1]:date[1]+27) %in% V2),keyby="personanddrug"]


b[,.(hh= data.table(c(1,2),c(3,4))$V1           ,       aaa=    data.table(c(1,2),c(3,4))$V2        ),keyby="personanddrug"]



function(x)