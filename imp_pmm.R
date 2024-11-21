# simple pmm imputation
imp_pmm = function(data, tv1, iv1, m) {
  # imputation dataset
  data_use = data %>%
    dplyr::select(id, year, tv1, iv1, y)
  data_use$id = as.integer(data_use$id)
  data_use = data_use %>%
    mutate_all(~ifelse(. == "NaN", NA, .))
  
  # set imputation parameters
  ini = mice(data_use, maxit = 0)
  
  pred = ini$pred
  
  meth = ini$meth

  # imputation process
  imp.use = mice(data_use, pred = pred, meth = meth, m = m, maxit = 5, seed = 1018, printFlag = F)
  
  return(imp.use)
}