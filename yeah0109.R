library(data.table);library(magrittr)

exercise_data <- data.table(
  start_date = as.Date(c("2024-01-01", "2024-01-05", "2024-01-13", "2024-02-29")),
  duration = c(2, 6, 5, 7),k=c(1,1,1,1))

happy<-function(start_dates, durations) {
  new_exercise_data<-data.table(start_date=start_dates[1],duration=durations[1])
  if (length(start_dates)>=2) {
    for (i in c(2:length(start_dates))){
      if (new_exercise_data[.N, start_date+duration]<start_dates[i]) {
        new_exercise_data<-new_exercise_data %>% rbind(data.table(start_dates[i],durations[i]),use.names=F)
      } else {
        new_exercise_data<-new_exercise_data[.N,duration:=duration+durations[i]]
      }
    }
  }
  print(new_exercise_data)
  drug_yes<-vector()
  for (i in c(1:length(new_exercise_data$start_date))) {
    drug_yes<-c(drug_yes,seq(new_exercise_data$start_date[i], new_exercise_data$start_date[i]+new_exercise_data$duration[i]-1, by = "days"))
  }
  return(as.list(drug_yes))
  # result<-new_exercise_data[,candidate:=mapply(function(x,y){ifelse(sum(seq(as.numeric(x),as.numeric(x)+27,1) %in% drug_yes)>=14,"Y",ifelse(y>=7,"Y","N"))},start_date,duration)] %>%
  #   .[order(start_date)] %>% .[candidate=="Y",head(.SD,1)]
  # return(result$start_date)
}

#happy(exercise_data$start_date,exercise_data$duration)
exercise_data<-exercise_data[,mapply(happy,start_date,duration),keyby="k"]
