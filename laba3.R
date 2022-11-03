library('ggplot2')

main()

main <- function(){
  ExplicitedMethod()
  cat("\n");
  ImplicitMethod()
}

ExplicitedMethod <- function(){
  Tmin <- c(0)
  valueYK <- c(1, 1, 1)
  step <- c(0)
  i <- c(0)
  while(Tmin < 1){
      i <- (i + 1)
      newValues <- RecounterValues(valueYK, 2, 1)
      step <- RecountStep(1, newValues, 10e-4)
      valueYK[1] <- (valueYK[1] + step * newValues[1])
      valueYK[2] <- (valueYK[2] + step * newValues[2])
      valueYK[3] <- (valueYK[3] + step * newValues[3])
      Tmin <- (Tmin + step)
      cat(i, valueYK, Tmin, "\n")
      
  }
}

RecounterValues <- function(value, k , a){
  tmp <- c(0, 0, 0)
  tmp[1] <- (((k - a) * value[2] * value[3]) / a)
  tmp[2] <- (((k + a) * value[1] * value[3]) / k)
  tmp[3] <- (((a - k) * value[1] * value[2]) / a)
  return(tmp)
}

RecountStep <- function(maxStep, value, epsilon){
  FirstEQStep <- epsilon / (abs(value[1] + (epsilon / maxStep)))
  SecondEQStep <- epsilon / (abs(value[2] + (epsilon / maxStep)))
  TheardEQStep <- epsilon / (abs(value[3] + (epsilon / maxStep)))
  minStep <- c(0)
  
  if (FirstEQStep < SecondEQStep)
    minStep <- FirstEQStep
  else 
    minStep <- SecondEQStep
  if (TheardEQStep < minStep)
    minStep <- TheardEQStep
  
  return(minStep)
}






ImplicitMethod <- function(){
  
}























