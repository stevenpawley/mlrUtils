# Functions ----

#' Plots a reliability chart of a set of predicitons from a classifier
#'
#' @param y_true Vector of true labels. Should be binary (0 or 1)
#' @param y_pred Vector of predictions from the classifier
#' Should be real number.
#' @param bins The number of bins to use in the reliability plot
#' @param scale Scale the pred to be between 0 and 1 before creating reliability plot
#'
#' @return data.frame of mean prediction value vs fraction of positives
calibration_curve = function(y_true, y_pred, bins=10, scale=T) {

  # convert factor to numeric
  if (is(y_true, 'numeric'))
    stop('y_true requires a factor variable')
  y_true = as.numeric(levels(y_true))[y_true]

  min.pred <- min(y_pred)
  max.pred <- max(y_pred)
  min.max.diff <- max.pred - min.pred
  if (scale) {
    y_pred <- (y_pred - min.pred) / min.max.diff
  }
  bin.pred <- cut(y_pred, bins)
  k <- plyr::ldply(levels(bin.pred), function(x) {
    idx <- x == bin.pred
    c(sum(y_true[idx]) / length(y_true[idx]), mean(y_pred[idx]))
  })
  is.nan.idx <- !is.nan(k$V2)
  k <- k[is.nan.idx,]
  names(k) = c('Fraction_of_positives', 'Mean_prediction_value')
  return(k)
}

#' Fit isotonic regression
#'
#' @param iso model from the stats::isoreg function
#' @param x0 vector of probabilities from a classifier
#'
#' @return A fitted isotonic regression model
#' @export
fit_isoreg <- function(iso, x0) {
  o = iso$o
  if (is.null(o))
    o = 1:length(x)
  x = iso$x[o]
  y = iso$yf
  ind = cut(x0, breaks = x, labels = FALSE, include.lowest = TRUE)
  min.x <- min(x)
  max.x <- max(x)
  adjusted.knots <- iso$iKnots[c(1, which(iso$yf[iso$iKnots] > 0))]
  fits = sapply(seq(along = x0), function(i) {
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

    # Pefrom a liner interpolation between the start and end of the step
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

#' Train a calibrated classifier using platt or isotonic regression
#'
#' @param y_true Vector of true labels. Should be binary (0 or 1)
#' @param y_pred Vector of predicted probabilities from a classifier
#' @param method Character, either "isotonic" (default) or "platt"
#'
#' @return A calibrated_classifier S3 class object
#' @export
calibrated_classifier = function(y_true, y_pred, method='isotonic') {
  if (method == 'isotonic') {
    idx = duplicated(y_pred)
    y_pred_unique = y_pred[!idx]
    y_true_unique = y_true[!idx]

    model = stats::isoreg(y_pred_unique, as.numeric(levels(y_true_unique))[y_true_unique])
  } else if (method == 'platt') {
    model = glm(formula = y_true ~ y_pred,
                data = cbind.data.frame(y_true, y_pred),
                family=binomial)
  } else {
    stop('Method must be either "isotonic" (default) or "platt"')
  }

  structure(model, class=c('calibrated_classifier', class(model)))
}

#' Predict method for the calibrated_classifier S3 class
#'
#' @param model A calibrated_classifier S3 class
#' @param y_pred Vector of probabilities upon which to apply calibration.
#' The y_pred values should be disjoint from the data used to train the
#' calibrated_classifier model
#'
#' @return Vector of calibrated probabilities.
#' @export
predict.calibrated_classifier = function(model, y_pred) {
  if (is(model, "isoreg")) {
    y_pred_cal = fit_isoreg(model, y_pred)

  } else if (is(model, "glm")) {
    class(model) = 'glm'
    y_pred_cal = predict(model, newdata=data.frame(y_pred), type="response")
  }
  return(y_pred_cal)
}


#' Prediction function for raster::predict method to produce classification,
#' probabilities for a binary class, and optionally calibrated probabilities
#'
#' @param model fitted mlr model
#' @param data data passed from the raster predict method
#' @param calmodel calibrated_classifier model
#' @param type predict 'classification', 'probabilities', or 'all'
#' @param label name of class to produce probabilities for
#'
#' @return numeric or data.frame
#' @export
predfun_calibrated = function(model, data, calmodel = NULL, type = 'all',
                              label = 'prob.1') {
  # prediction
  y_pred = predict(model, newdata=data)

  # classification
  y_class = y_pred$data[, 'response']

  # raw probabilities
  y_prob = y_pred$data[, label]

  # probability calibration
  # raw probs -> calibrated probs using predict.calibrated_classifier
  if (!is.null(calmodel)) {
    y_prob = predict(model = calmodel, y_pred = y_prob)
  }

  if (type == 'classification') {
    result = y_class
  } else if (type == 'probabilities') {
    result = y_prob
  } else if (type == 'all') {
    result = data.frame(y_class, y_prob)
  }
  return(result)
}

#' Training function for custom preprocessor wrapper to center and scale data
#'
#' @param data data.frame. Training data including response variable
#' @param target character. Name of target (response) variable
#' @param args list. Arguments to center and/or scale the data
#' @return
#' @export
trainStandardRescaler = function(data, target, args = list(center=T, scale=T)) {
  
  # Identify numerical features
  cns = colnames(data)
  nums = setdiff(cns[sapply(data, is.numeric)], target)
  
  # Extract numerical features from the data set and call scale
  x = as.matrix(data[, nums, drop = FALSE])
  x = scale(x, center = args$center, scale=args$scale)
  
  # Store the scaling parameters in control
  # These are needed to preprocess the data before prediction
  control = args
  if (is.logical(control$center) && control$center)
    control$center = attr(x, "scaled:center")
  if (is.logical(control$scale) && control$scale)
    control$scale = attr(x, "scaled:scale")
  
  # Recombine the data
  data = data[, setdiff(cns, nums), drop = FALSE]
  data = cbind(data, as.data.frame(x))
  
  return(list(data = data, control = control))
}

#' Prediction function for custom preprocessor wrapper to center and scale data
#' 
#' Prediction function that is required for custom preprocessor wrapper in order
#' to apply the wrapper to new data.
#'
#' @param data data.frame. Newdata to predict
#' @param target character. Name of target (response) variable
#' @param args list. Arguments to center and/or scale the data
#' @param control scaling parameters from trainfun to be used in the prediction
#' @return
#' @export
predictStandardRescaler = function(data, target, args, control) {
  
  # Identify numerical features
  cns = colnames(data)
  nums = cns[sapply(data, is.numeric)]
  
  # Extract numerical features from the data set and call scale
  x = as.matrix(data[, nums, drop = FALSE])
  x = scale(x, center = control$center, scale = control$scale)
  
  # Recombine the data
  data = data[, setdiff(cns, nums), drop = FALSE]
  data = cbind(data, as.data.frame(x))
  
  return(data)
}
