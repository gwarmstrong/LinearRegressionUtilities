newVar <- function(addstep) {return(c(rownames(addstep)[which(addstep$`F value` == (maxF <- max(addstep$`F value`,na.rm=T)))],maxF))}

addLM <- function(lm, full.lm, Fmin){
  ad1 <- add1(lm, full.lm, test='F')
  step2 <- newVar(ad1)
  if(as.numeric(step2[2]) > Fmin){
    step2.lm <- update(lm, paste('.~. + ',step2[1]))
  } else{
    step2.lm <- lm
  }
  return(step2.lm)
}

dropLM <- function(lm, Fmin){
  if(length(lm$coefficients) > 1){
    d1 <- drop1(lm, test='F')
    if(min(d1$`F value`,na.rm=T) < Fmin){
      removeVariable <- rownames(d1)[which(d1$`F value` == min(d1$`F value`,na.rm=T))]
      lm <- update(lm, paste('.~. - ', removeVariable, sep=''))
      lm <- dropLM(lm, Fmin)
    }
  }
  return(lm)
}

stepwiseLM <- function(base, full, Fadd, Fdrop){
  priorLm = base
  nextLm <- addLM(priorLm, full, Fadd)
  nextLm <- dropLM(nextLm, Fdrop)
  while(!setequal(variable.names(priorLm),variable.names(nextLm)) && length(nextLm$coefficients) < length(full$coefficients)){
    priorLm <- nextLm
    nextLm <- addLM(priorLm, full, Fadd)
    nextLm <- dropLM(nextLm, Fdrop)
  }
  return(nextLm)
}