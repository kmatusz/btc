max_p <- 20

floating_ar <- rep(NA, max_p)
# floating_ar[c(9,20, 44, 46, 50)] <- NA
floating_ar

models <- vector('list', max_p)

timer <- Sys.time()

models[[1]] <- Arima(btc$btc,  # variable
                     order = c(max_p, 1, max_p),  # (p,d,q) parameters
                     fixed = c(floating_ar, floating_ar)
)

print(Sys.time() - timer) 
drop_order <- c()
for (i in 11:max_p){
  print(i)
  print(Sys.time() - timer) 
  max_p_value <- which.max(coeftest(models[[i-1]])[,4])
  current_to_drop <- str_replace(names(max_p_value), 'ar', '') %>% str_replace('ma', '') %>% as.numeric()
  drop_order <- c(drop_order, current_to_drop)
  floating_ar[c(current_to_drop)] <- 0
  
  models[[i]] <- Arima(btc$btc,  # variable
                       order = c(max_p, 1, max_p),  # (p,d,q) parameters
                       fixed = c(floating_ar, floating_ar)
  )
}


save(models, file = 'data/auto_models.Rdata')
