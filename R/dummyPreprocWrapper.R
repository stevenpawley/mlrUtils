#' Preprocessing wrapper to merge small factor levels
#'
#' @param learner mlr learner
#' @param min.perc minimum percentage of factor observations
#'
#' @return PreprocWrapper
#' @export
makePreprocWrapperMergeFactors = function(learner, min.perc = 0.01) {

  trainfun = function(data, target, args = list(min.perc)) {

    if (getLearnerType(learner) == "regr") task <- makeRegrTask(data = data, target = target) else
      task <- makeClassifTask(data = data, target = target)

    # get columns with factor datatypes
    cns <- lapply(data, class)
    cns <- cns[names(cns) != target]
    cns <- names(cns[cns == "factor"])

    # merge small factors
    if (length(cns) > 0) {
      task_merged <- mergeSmallFactorLevels(task, min.perc = min.perc)

      # create reclassification table per factor
      merged_levels <- lapply(getTaskData(task_merged), levels)[cns]
      task_levels <- lapply(data, levels)[cns]

      to_merge <- lapply(cns, function(x)
        setdiff(task_levels[[x]], merged_levels[[x]]))
      names(to_merge) <- cns

      data <- getTaskData(task_merged)

    } else {
      to_merge <- NULL
    }

    # create trainfun object
    control = list(to_merge = to_merge)
    return(list(data = data, control = control))
  }

  predictfun = function(data, target, args, control) {
    cns <- lapply(data, class)
    cns <- cns[names(cns) != target]
    cns <- names(cns[cns == "factor"])
    for (x in cns)
      data[[x]] <- forcats::fct_other(
        data[[x]], drop = control$to_merge[[x]], other_level = ".merged")
    return(data)
  }

  makePreprocWrapper(
    learner,
    train = trainfun,
    predict = predictfun,
    par.set = makeParamSet(
      makeNumericLearnerParam("min_sample")),
    par.vals = list(min.perc = min.perc)
  )
}


#' Preprocessing wrapper for one-hot encoding
#'
#' @param learner mlr learner
#' @param method character, one of c("1-of-n", "reference")
#'
#' @return PreprocWrapper
#' @export
makePreprocWrapperDummyFeatures = function(learner, method = "1-of-n") {

  trainfun = function(data, target, args = list(method)) {

    data <- createDummyFeatures(data, target = target, method = method)

    # add additional level in case new factors encounted on prediction
    cns <- lapply(data, class)
    cns <- cns[names(cns) != target]
    cns <- names(cns[cns == "factor"])
    factor_levels <- list()

    for (x in cns) {
      factor_levels[[cns]] <- levels(data[[x]])
      data[[x]] <- forcats::fct_expand(data[[x]], "Missing")
    }

    names(factor_levels) <- cns
    control = list(method = method, cns = cns, factor_levels = factor_levels)

    return(list(data = data, control = control))
  }

  predictfun = function(data, target, args, control) {
    data <- createDummyFeatures(data, method = control$method)
    for (x in control$cns) {
      cns_levels <- levels(data[[x]])
      to_merge <- setdiff(cns_levels, control$factor_levels[[x]])
      data[[x]] <- forcats::fct_expand(data[[x]], "Missing")
      data[[x]] <- forcats::fct_other(
        data[[x]], drop = to_merge, other_level = "Missing")
    }
    return(data)
  }

  lrn <- makePreprocWrapper(
    learner,
    train = trainfun,
    predict = predictfun,
    par.set = makeParamSet(
      makeDiscreteLearnerParam("method", values = c("1-of-n", "reference"))),
    par.vals = list(method = method))

  lrn$id = stringi::stri_replace(
    lrn$id, replacement = ".dummied", regex = "\\.preproc$")
  BBmisc::addClasses(lrn, "DummyFeaturesWrapper")
}

getLearnerProperties.DummyFeaturesWrapper = function(learner) {
  union(getLearnerProperties(learner$next.learner), c("factors", "ordered"))
}
