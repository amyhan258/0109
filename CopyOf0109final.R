library(data.table);library(magrittr)

exercise_data <- data.table(
  start_date = as.Date(c("2024-01-01", "2024-01-05", "2024-01-13", "2024-02-29")),
  duration = c(2, 6, 6, 6, 7))

happy<-function(start_dates, durations) {
  new_exercise_data<-data.table(start_date=start_dates[1],duration=durations[1])
  for (i in c(2:length(start_dates))){
    if (new_exercise_data[.N, start_date+duration]<start_dates[i]) {
      new_exercise_data<-new_exercise_data %>% rbind(data.table(start_dates[i],durations[i]),use.names=F)
    } else {
      new_exercise_data<-new_exercise_data[.N,duration:=duration+durations[i]]
    }
  }
  drug_yes<-vector()
  for (i in c(1:length(start_dates))) {
    drug_yes<-c(drug_yes,seq(start_dates[i], start_dates[i]+durations[i]-1, by = "days"))
  }
  # print(new_exercise_data)
  # print(drug_yes)
  # hu<-new_exercise_data[,candidate_1:=sapply(start_date,function(x){ifelse(sum(seq(as.numeric(x),as.numeric(x)+27,1) %in% drug_yes)>=14,"Y","N")})] %>%
  #   .[,candidate_2:=sapply(duration,function(x){ifelse(x>=7,"Y","N")})]
  # 
  # print(hu)
  
  
  hu2<-new_exercise_data[,candidate:=mapply(function(x,y){ifelse(sum(seq(as.numeric(x),as.numeric(x)+27,1) %in% drug_yes)>=14,"Y",ifelse(y>=7,"Y","N"))},start_date,duration)] %>%
    .[order(start_date)] %>% .[candidate=="Y",head(.SD,1),.SDcols="start_date"]
  #hu2[,head()]
   print(hu2)
    
  #return(0)
}



happy(exercise_data$start_date,exercise_data$duration)
