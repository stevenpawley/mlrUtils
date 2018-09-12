#' Permutation importances calculated during cross validation
#'
#' @param learner Learner, WrappedLearner
#' @param task task
#' @param cv ResampleDesc object
#' @param measure mlr performance measure
#' @param n_permutations numeric, number of permutations
#'
#' @return dataframe, containing permutations scores per feature
#' @export
permutationImportance = function(learner, task, cv, measure, n_permutations = 10) {
  
  # make resampling instance
  desc = mlr::makeResampleDesc(method = 'CV', iters = cv)
  resamples = mlr::makeResampleInstance(desc = desc, task = task)
  
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
      perm_score = replicate(n_permutations, permute_and_score(
        X = task$env$data[test_ind, ], clf = clf, measure = measure, feature = j))
      perm_scores[k, j] = mean(perm_score)
    }
  }
  
  # average scores by n_permutations and subtract from best score
  perm_scores = best_score - perm_scores
  return (perm_scores)
}