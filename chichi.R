drug<-c(rep("T",49),rep("T",15),rep("I",223),rep("I",72))
status<-c(rep(1,49),rep(0,15),rep(1,223),rep(0,72))

chisq.test(table(status, drug),correct=T)

#ITT
prop.test(x=c(49,223),n=c(64,295),correct=F)
# 95 percent confidence interval:
#   -0.1050818  0.1244674

#PP
prop.test(x=c(48,222),n=c(54,251))
