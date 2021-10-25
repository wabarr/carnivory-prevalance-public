summarizeModel <- function(model){
  if("glm" %in% class(model)) {
    data.frame(
      model=as.character(model$call)[2],
      R2=0,
      AIC=0,
      p=0,
      slope=coef(model)[2],
      intercept=coef(model)[1]
    )
  }
  else if(class(model)=="lm"){
    data.frame(
      model=as.character(model$call)[2],
      R2=summary(model)$r.squared,
      AIC=AIC(model),
      p=anova(model)[1,5],
      slope=coef(model)[2],
      intercept=coef(model)[1]
    )
  }
}