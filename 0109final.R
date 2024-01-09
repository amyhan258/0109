library(data.table);library(magrittr)

exercise_data <- data.table(
  start_date = as.Date(c("2024-01-01", "2024-01-18", "2024-02-05", "2024-02-29")),
  duration = c(2, 13, 6, 6))

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
  print(drug_yes)
  for (i in start_dates) {
    if (sum(seq(as.numeric(i),as.numeric(i)+27,1) %in% drug_yes)>=14) {
      return(as.Date(i))
    }
  }
  return(0)
}



print(happy(exercise_data$start_date,exercise_data$duration))
