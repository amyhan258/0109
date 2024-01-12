a<-data.table(pe=c(1,1,1,1,1,2,2,2,2,2),dr=c(1,2,3,2,3,1,1,2,2,3),start_date=1,10,20,30,40,1,15,25,35,45,end_date=c(1,10,15,30,45,6,15,27,35,48)+10)

b<-data.table(pe=c(1,1,1,1,2,2,2),처방명_date=c(1,2,17,44,23,25,40),처방명=c("p","p","w","q","q","r","w"))

tb6.1<-merge(a,b,by="pe",allow.cartesian=T) %>% .[, e:=ifelse((start_date<=처방명_date) & (처방명_date<=start_date+27),1,2)] %>%
  .[e==1,.(dr,처방명)] %>% dcast(dr~처방명,fun=length)


tb6.2<-merge(a,b,by="pe",allow.cartesian=T) %>% .[, e:=ifelse((end_date<=처방명_date) & (처방명_date<=end_date+27),1,2)] %>%
  .[e==1,.(dr,처방명,e)] %>% dcast(dr~처방명,fun=length)



aa<-data.table(c=c(1,1,2,2),d=c(2,3,4,5))
bb<-split(aa,aa$c)
as.data.table(bb[1])
srcfile <- file.path( R.home("doc"), "html", "logo.jpg" )
