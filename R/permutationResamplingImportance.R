#' Permutation importances calculated during resampling
#'
#' @param learner Learner, WrappedLearner
#' @param task task
#' @param resampling ResampleDesc object
#' @param measure mlr performance measure
#' @param n_permutations numeric, number of permutations, default = 50
#'
#' @return dataframe, containing permutations scores per feature
#' @export
permutationResamplingImportance = function(learner, task, resampling, measure, n_permutations = 50) {

  # make resampling instance
  resamples = mlr::makeResampleInstance(desc = resampling, task = task)

  # store mean of permutation scores per feature
  features = names(task$env$data)
  features = features[!features == task$task.desc$target]
  perm_scores = data.frame(matrix(data = 0, nrow=resamples$desc$iters, ncol=length(features)))
  names(perm_scores) = features

  for (k in 1:resamples$desc$iters) {
    train_ind = resamples$train.inds[[k]]
    test_ind = resamples$test.inds[[k]]

    clf = mlr::train(learner = learner, task = task, subset = train_ind)
    preds = predict(clf, task = task, subset = test_ind)

    # get best score
    best_score = mlr::performance(pred = preds, measures = measure)

    # permute and score
    permute_and_score = function(X, clf, measure, feature) {
      X[[feature]] = X[sample(1:nrow(X), nrow(X)), feature]
      preds = predict(clf, newdata = X)
      perm_score = mlr::performance(pred = preds, measures = measure)
      return(perm_score)
    }

    # permute each variable n_permutations times and take mean score
    for (j in features) {
      perm_score = sapply(1:n_permutations, function(x) permute_and_score(
        X = task$env$data[test_ind, ],
        clf = clf,
        measure = measure,
        feature = j))
      perm_scores[k, j] = mean(perm_score)
      perm_scores[k, j] = best_score - perm_scores[k, j]
    }
  }

  return (perm_scores)
}
