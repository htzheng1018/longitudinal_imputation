# simple pmm imputation
imp_pmm_sumscore = function(data, tv, iv, x_sum, m) {
  # imputation dataset
  data_use = data %>%
    dplyr::select(id, year, tv, iv, y)
  data_use$id = as.integer(data_use$id)
  data_use = data_use %>%
    mutate_all(~ifelse(. == "NaN", NA, .))
  
  # set imputation parameters
  ini = mice(data_use, maxit = 0)
  
  pred = ini$pred
  pred[tv, x_sum] = 0
  pred[iv, tv] = 0
  pred[x_sum, ] = 0
  pred["y", tv] = 0
  
  meth = ini$meth
  meth[x_sum] = sprintf("~I(%s + %s + %s)", tv[1], tv[2], tv[3])
  
  # imputation process
  imp.use = mice(data_use, pred = pred, meth = meth, m = m, maxit = 5, seed = 1018, printFlag = F)
  
  return(imp.use)
}