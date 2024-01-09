a<-data.table(pe=c(1,1,1,1,1,2,2,2,2,2),dr=c(1,2,3,2,3,1,1,2,2,3),start=c(1,10,20,30,40,1,15,25,35,45),du=c(3,2,5,4,3,6,1,2,5,2))
b<-data.table(pe=c(1,1,1,2,2),date=c(1,17,24,2,25),dis=c("p","p","q","q","r"))
c<-merge(a,b,by="pe",allow.cartesian=T) %>% .[, e:=ifelse((start<=date) & (date<=start+27),1,2)] %>%
  .[e==1,.(dr,dis,e)] %>% dcast(dr~dis,fun=length)


aa<-data.table(c(1,2),c(3,4))
