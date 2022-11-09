library('ggplot2')

main()

main <- function(){
  ExplicitedMethod()
  cat("\n");
  readline();
  ImplicitMethod()
}



ExplicitedMethod <- function(){
  Tmin <- c(0)
  valueYK <- c(1, 1, 1)
  step <- c(0)
  GraphicsU1 <- c()
  GraphicsU2 <- c()
  GraphicsU3 <- c()
  Tvec <- c()
  i <- c(0)
  while(Tmin < 1){
      i <- (i + 1)
      newValues <- RecounterValues(valueYK, 2, 1)
      step <- RecountStep(1, newValues, 10e-4)
      valueYK[1] <- (valueYK[1] + step * newValues[1])
      valueYK[2] <- (valueYK[2] + step * newValues[2])
      valueYK[3] <- (valueYK[3] + step * newValues[3])
      Tmin <- (Tmin + step)
      GraphicsU1 <- c(GraphicsU1, valueYK[1])
      GraphicsU2 <- c(GraphicsU2, valueYK[2])
      GraphicsU3 <- c(GraphicsU3, valueYK[3])
      Tvec <- c(Tvec, Tmin)
      cat(i, valueYK, Tmin, "\n")
  }
  dataFrameU1 <- data.frame(
    x = Tvec,
    y = GraphicsU1
  )
  dataFrameU2 <- data.frame(
    x = Tvec,
    y = GraphicsU2
  )
  dataFrameU3 <- data.frame(
    x = Tvec,
    y = GraphicsU3
  )
  ggplot(data = dataFrameU1, aes(x = Tvec, y = GraphicsU1, col = '#FF00F0')) +
    geom_point() +
    geom_point(data = dataFrameU2, aes(x = Tvec, y = GraphicsU2), col = '#00FF0F') +
    geom_point() +
    geom_point(data = dataFrameU3, aes(x = Tvec, y = GraphicsU3), col = '#F000FF')
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
  
  epsilon <- c(10e-4)
  hmax <- c(1)
  h <- c(0.01)
  tmpH <- c(h)
  prevH <- c(h)
  u <- c(1, 1, 1)
  tmpU <- c(0, 0, 0)
  prevU <- c(0, 0, 0)
  discrypancy <- c(0, 0, 0)
  GraphicsU1_IM <- c()
  GraphicsU2_IM <- c()
  GraphicsU3_IM <- c()
  Tvec_IM <- c()
  i <- c(0)
  sighn <- c(T)
  t <- c(0)
  
  while(t < 1){
    tmpT <- c(0)
    while(sighn == T){
      sighn <- F
      i <- (i + 1)
      tmpT <- t + h
      
      tmpU <- c(Newton(tmpU, u, h))
      
      for(i_n in 1 : 3){
        discrypancy[i_n] <- (-1 * (h / (h + prevH)) * (tmpU[i_n] - u[i_n] - (h * (u[i_n] - prevU[i_n])) / (prevH)))
      }
      
      if(abs(discrypancy[1]) > epsilon || abs(discrypancy[2]) > epsilon || abs(discrypancy[3]) > epsilon){
        h <- (h / 2)
        tmpH <- h
        tmpU <- c(u)
        sighn <- T
      }
    }
    
    tmpH <- RecounterImplicitsStep(h, epsilon, discrypancy)
    if(tmpH > hmax){
      tmpH <- hmax
    }
    prevU <- c(u)
    u <- c(tmpU)
    prevH <- h
    h <- tmpH
    t <- tmpT
    sighn <- T
    GraphicsU1_IM <- c(GraphicsU1_IM, u[1])
    GraphicsU2_IM <- c(GraphicsU2_IM, u[2])
    GraphicsU3_IM <- c(GraphicsU3_IM, u[3])
    Tvec_IM <- c(Tvec_IM, t)
    cat(i, u, t, "\n")
  }
  dataFrameU1_IM <- data.frame(
    x = Tvec_IM,
    y = GraphicsU1_IM
  )
  dataFrameU2_IM <- data.frame(
    x = Tvec_IM,
    y = GraphicsU2_IM
  )
  dataFrameU3_IM <- data.frame(
    x = Tvec_IM,
    y = GraphicsU3_IM
  )
  ggplot(data = dataFrameU1_IM, aes(x = Tvec_IM, y = GraphicsU1_IM, col = '#FF00F0')) +
    geom_point() +
    geom_point(data = dataFrameU2_IM, aes(x = Tvec_IM, y = GraphicsU2_IM), col = '#00FF0F') +
    geom_point() +
    geom_point(data = dataFrameU3_IM, aes(x = Tvec_IM, y = GraphicsU3_IM), col = '#F000FF')
}

RecounterImplicitsStep <- function(h, epsilon, vec){
  hmin <- c(0)
  StepVec <- c(0, 0, 0)
  for(i in 1 : 3){
    StepVec[i] <- (((epsilon / abs(vec[i])) ^ 0.5) * h)
  }
  if(StepVec[1] < StepVec[2]){
    hmin <-  StepVec[1]
  } else {
    hmin <- StepVec[2]
  }
  if(StepVec[3] < hmin){
    hmin <- StepVec[3]
  }
  return(hmin)
}


Newton <- function(x, y, h){
  TempX <- c(0, 0, 0)
  deltx <- c(0, 0, 0)
  data <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  epsilon <- c(0.000000001)
  d1 <- c(1)
  d2 <- c(1)
  counter <- c(1)
  
  while((d1 > epsilon || d2 > epsilon) && counter < 100){
    DiscrypancyVec <- RecountImplicitsValues(x, y, h, 1, 2)
    
    Jacobian <- RecountJacobian(x, h, 1, 2)
    
    #print("F: ")
    #print(DiscrypancyVec)
    
    deltx <- SolveGause(Jacobian, 3, DiscrypancyVec)
    
    for(i in 1 : 3){
      TempX[i] <- (x[i] + deltx[i]) 
    }
    d1 <- GetMaxElenet(x, y, h, 1, 2)
    d2 <- GetAbsoluteMaxElenet(x, TempX)
    counter <- (counter + 1)
    x <- TempX
    if(counter >= 100){
      cat("IER = 2!")
    }
  }
  return(x)
}


RecountImplicitsValues <- function(x, y, h, a, k){
  tmp <- c(0, 0, 0)
  tmp[1] <- (x[1] - y[1] - h * ((k - a) * x[2] * x[3]) / a)
  tmp[2] <- (x[2] - y[2] - h * ((k + a) * x[1] * x[3]) / k)
  tmp[3] <- (x[3] - y[3] - h * ((a - k) * x[1] * x[2]) / a)
  return(tmp)
}

RecountJacobian <- function(x, h, a, k){
  data <- c(1, 0, 0, 0, 1, 0, 0, 0, 1)
  Jacobian <- matrix(data, nrow = 3, ncol = 3, byrow = F)
  Jacobian[1, 2] <- ((h * (k - a) * x[3]) / a)
  Jacobian[1, 3] <- ((h * (k - a) * x[2]) / a)
  Jacobian[2, 1] <- ((h * (k + a) * x[3]) / k)
  Jacobian[2, 3] <- ((h * (k + a) * x[1]) / k)
  Jacobian[3, 1] <- ((h * (k + a) * x[2]) / k)
  Jacobian[3, 2] <- ((h * (k + a) * x[1]) / k)
  return(Jacobian)
}

GetMaxElenet <- function(x, y, h, a, k){
  tmp <- RecountImplicitsValues(x, y, h, a, k)
  max <- c(0)
  if(abs(tmp[1]) > abs(tmp[2])){
    max <- abs(tmp[1])
  } else {
    max <- abs(tmp[2])
  }
  if(abs(tmp[3]) > max){
    max <- abs(tmp[3])
  }
  return(max)
}

GetAbsoluteMaxElenet <- function(x, tmpX){
  max <- c(0)
  
  for(i in 1 : 3){
    if(abs(tmpX[i]) < 1){
      if(abs(tmpX[i] - x[i]) > max){
        max <- abs(tmpX[i] - x[i])
      }
    } else {
      if(abs(tmpX[i] - x[i]) > max){
        max <- abs((tmpX[i] - x[i]) / tmpX[i])
      }
    }
  }
  return(max)
}

SolveGause <- function(mat, size, vec){
  tmpvec <- c(vec)
  tmpmatrix <- matrix(mat, nrow = 3, ncol = 3, byrow = F)
  for(i in 1 : size){
    tmpvec[i] <- (-1 * tmpvec[i])
  }
  for(i in 1 : size){
    
    coefficients <- rep(0, size)
    for(i_n in i : size){
      coefficients[i_n] <- tmpmatrix[i_n, i] / tmpmatrix[i, i]
    }
    
    j = i + 1
    for(i_m in i : size){
      for(i_n in j : size){
        if(j > size){
          break
        }
        tmp <- tmpmatrix[i_n, i_m] - (tmpmatrix[i, i_m] * coefficients[i_n])
        tmpmatrix[i_n, i_m] <- tmp
      }
    }
    
    j = i + 1
    for(i_n in j : size){
      if(j > size){
        break
      }
      tmp <- tmpvec[i_n] - (tmpvec[i] * coefficients[i_n])
      tmpvec[i_n] <- tmp
    }
    
    tmp <- tmpvec[i] / tmpmatrix[i, i]
    tmpvec[i] <- tmp
    
    for(i_n in size : i){
      tmp <- tmpmatrix[i, i_n] / tmpmatrix[i, i]
      tmpmatrix[i, i_n] <- tmp
    }
  }
  
  result <- rep(0, size)
  for(i in size : 1){
    value <- tmpvec[i]
    for(i_n in size : 1){
      tmp <- value - (tmpmatrix[i, i_n] * result[i_n])
      value <- tmp
    }
    result[i] <- value / tmpmatrix[i, i]
  }
  
  return(result)
}


