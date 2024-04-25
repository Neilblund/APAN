pred_ci = function(model, newdata=NULL, level=.95){
  # function for calculating 95% confidence intervals on the response scale from a glm or svyglm object. 
  
  if(class(model)[1]=="svyglm"){
    preds = predict(model, newdata, type="link", se.fit=T)|>as.data.frame()
    
    result = data.frame(
      fit = model$family$linkinv(preds$link),
      lwr = model$family$linkinv(preds$link + qnorm((1-level)/2, lower.tail=T) * preds$SE),
      upr = model$family$linkinv(preds$link + qnorm((1-level)/2, lower.tail=F) * preds$SE)
      )
  }
  if(class(model)[1]=="glm"){
    preds = predict(model, newdata, type="link", se.fit=T)|>as.data.frame()
    
    result = data.frame(
      fit = model$family$linkinv(preds$fit),
      lwr = model$family$linkinv(preds$fit + qnorm((1-level)/2, lower.tail=T) * preds$se.fit),
      upr = model$family$linkinv(preds$fit + qnorm((1-level)/2, lower.tail=F) * preds$se.fit)
      )
    
  }
  if(class(model)[1]=="lm"){
    warning("This is a linear model, you should probably just use the predict() function")
    result = predict(model, newdata=newdata, interval="confidence",  level=level)
  }
  
  if(!is.null(newdata)){
    # combine the new predictions with the new data frame (just a convenience) 
    result = data.frame("index"= 1:nrow(newdata), cbind(newdata, result))
    }
  
  return(result)
  
  
  
}
