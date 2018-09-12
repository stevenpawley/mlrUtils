#' @export
makeRLearner.classif.isotonic = function() {
  makeRLearnerClassif(
    cl = "classif.isotonic",
    package = "stats",
    par.set = makeParamSet(
      makeLogicalLearnerParam("model", default = TRUE, tunable = FALSE)
    ),
    par.vals = list(
      model = FALSE
    ),
    properties = c("twoclass", "numerics", "prob"),
    name = "Isotonic Calibration",
    short.name = "isotonic",
    note = ""
  )
}

#' @export
trainLearner.classif.isotonic = function(.learner, .task, .subset, ...) {
  positive = getTaskDesc(.task)$positive
  target_name = getTaskTargetNames(.task)
  predictor_names = getTaskFeatureNames(.task)
  data = getTaskData(.task, .subset)
  
  y_pred = data[[which(names(data) %in% predictor_names)]]
  y_true = data[[which(names(data) == target_name)]]
  
  idx = duplicated(y_pred)
  y_pred_unique = y_pred[!idx]
  y_true_unique = y_true[!idx]
  
  y_true_unique = ifelse(y_true_unique == positive, 1, 0)
  
  stats::isoreg(x = y_pred_unique, y = y_true_unique)
}

#' @export
predictLearner.classif.isotonic = function(.learner, .model, .newdata, ...) {
  
  fit_isoreg = function(iso, x0) {
    # Predict an isotonic regression function
    # Parameters
    #   iso : fitted model returned from stats::isoreg
    #   x0 :  numeric, predictions from a base classifier
    #
    # Returns
    #   fits : numeric, predictions from isotonic regression
    #
    # Notes
    # from http://danielnee.com/tag/isotonic-regression/
    
    o = iso$o
    if (is.null(o))
      o = 1:length(x)
    
    # get original x and y data used to fit iso
    x = iso$x[o]
    y = iso$yf
    
    # using x as breaks, cut x0 predictions
    ind = cut(x0, breaks = x, labels = FALSE, include.lowest = TRUE)
    min.x <- min(x)
    max.x <- max(x)
    
    # x values where fitted curve changes, including first point
    adjusted.knots <- iso$iKnots[c(1, which(iso$yf[iso$iKnots] > 0))]
    
    fits = sapply(seq(along = x0), function(i) {
      # function to perform a linear interpolation in between the steps
      
      j = ind[i]
      
      # Handles the case where unseen data is outside range of the training data
      if (is.na(j)) {
        if (x0[i] > max.x) j <- length(x)
        else if (x0[i] < min.x) j <- 1
      }
      
      # Find the upper and lower parts of the step
      upper.step.n <- min(which(adjusted.knots > j))
      upper.step <- adjusted.knots[upper.step.n]
      lower.step <- ifelse(upper.step.n==1, 1, adjusted.knots[upper.step.n -1] )
      
      # Pefrom a linear interpolation between the start and end of the step
      denom <- x[upper.step] - x[lower.step]
      denom <- ifelse(denom == 0, 1, denom)
      val <- y[lower.step] + (y[upper.step] - y[lower.step]) * (x0[i] - x[lower.step]) / (denom)
      
      # Ensure we bound the probabilities to [0, 1]
      val <- ifelse(val > 1, max.x, val)
      val <- ifelse(val < 0, min.x, val)
      val <- ifelse(is.na(val), max.x, val) # Bit of a hack, NA when at right extreme of distribution
      val
    })
    return (fits)
  }
  
  x = fit_isoreg(iso = .model$learner.model, x0 =.newdata[[1]])

  class_levs = .model$task.desc$class.levels
  levs = class_levs[c(which(class_levs != positive), which(class_levs == positive))]
  
  if (.learner$predict.type == "prob") {
    mlr:::propVectorToMatrix(x, levs)
  } else {
    levs = .model$task.desc$class.levels
    p = as.factor(ifelse(x > 0.5, levs[2L], levs[1L]))
    unname(p)
  }
}
