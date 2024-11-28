# two level pmm + sum score imputation
imp_2lpmm_sumscore = function(data, tv, iv, x_sum, m) {
  # imputation dataset
  data_use = data %>%
    dplyr::select(id, year, tv, iv, x_sum, y)
  data_use$id = as.integer(data_use$id)
  data_use = data_use %>%
    mutate_all(~ifelse(. == "NaN", NA, .))
  
  # set imputation parameters
  ini = mice(data_use, maxit = 0)
  
  pred = ini$pred
  pred[1:nrow(pred), 1:ncol(pred)] = 0
  pred[tv, ] = c(-2, -1, rep(3, length(tv)), rep(1, length(iv)), 0, 3)
  pred[iv, ] = c(0, 0, rep(0, length(tv)), rep(1, length(iv)), 1, 1)
  pred[x_sum, ] = 0
  pred["y", ] = c(-2, -1, rep(0, length(tv)), rep(1, length(iv)), 3, 0)
  diag(pred) = 0
  
  meth = ini$meth
  meth[c(tv, "y")] = "2l.pmm"
  meth[x_sum] =  sprintf("~I(%s + %s + %s)", tv[1], tv[2], tv[3])
  
  # imputation process
  imp.use = mice(data_use, pred = pred, meth = meth, m = m, maxit = 5, seed = 1018, printFlag = F)
  
  return(imp.use)
}