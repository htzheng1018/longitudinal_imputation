# two level pmm imputation
imp_2lpmm = function(data, tv1, iv1, m) {
  # imputation dataset
  data_use = data %>%
    dplyr::select(id, year, tv1, iv1, y)
  data_use$id = as.integer(data_use$id)
  data_use = data_use %>%
    mutate_all(~ifelse(. == "NaN", NA, .))
  
  # set imputation parameters
  ini = mice(data_use, maxit = 0)
  
  pred = ini$pred
  pred[1:nrow(pred), 1:ncol(pred)] = 0
  pred[, "id"] = (-2)
  pred[, "year"] = (-1)
  pred[, tv1] = 3
  pred[, "y"] = 3
  pred[iv1, ] = c(0, 0, 1, 1, 0)
  diag(pred) = 0
  
  meth = ini$meth
  meth[c(tv1, "y")] = c("2l.pmm", "2l.pmm")
  
  # imputation process
  imp.use = mice(data_use, pred = pred, meth = meth, m = m, maxit = 5, seed = 1018, printFlag = F)
  
  return(imp.use)
}