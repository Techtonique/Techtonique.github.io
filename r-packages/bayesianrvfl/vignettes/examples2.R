
# longley: A macroeconomic data set with 7 economical variables, observed yearly from 1947 to 1962
library(MASS)
print(dim(longley))

X <- as.matrix(longley[, c("Unemployed", "Armed.Forces",
                           "GNP", "Year")])
y <- longley$Population

train_index <- 1:floor(0.7*nrow(X))

X_train <- X[train_index,]
y_train <- y[train_index]
X_test <- X[-train_index,]
y_test <- y[-train_index]

obj <- bayesianrvfl::fit_rvfl(x = X_train,
                              y = y_train,
                              #nb_hidden = floor(10**opt$par[1]),
                              #n_clusters = floor(opt$par[2]),
                              #lambda = 10**opt$par[3],
                              nb_hidden = floor(10**2),
                              n_clusters = floor(3),
                              lambda = 10**1,
                              compute_Sigma = TRUE)
(pred_obj <- bayesianrvfl::predict_rvfl(obj,
                               newx = X_test))
lower <- pred_obj$mean - 1.96*pred_obj$sd
upper <- pred_obj$mean + 1.96*pred_obj$sd
-mean((lower <= y_test)*(upper >= y_test))

par(mfrow=c(1, 2))

#n_preds <- length(pred_obj$mean)
#xx <- c(1:n_preds, rev(1:n_preds))
xx <- c(X_test[,"Year"], rev(X_test[,"Year"]))
yy95 <- c(pred_obj$mean + 1.96*pred_obj$sd,
          rev(pred_obj$mean - 1.96*pred_obj$sd))
yy80 <- c(pred_obj$mean + 1.28*pred_obj$sd,
          rev(pred_obj$mean - 1.28*pred_obj$sd))
plot(xx, yy95, type = "n", main = paste0("Population",
                                         "\n",
                                         "= f(Unemployed, Army,
                           GNP)"),
     ylim = c(110, 135),
     xlab="Year",
     ylab="Population")
polygon(xx, yy95, col = "gray90", border = "gray90")
polygon(xx, yy80, col = "gray", border = "gray")
lines(X_test[,"Year"], y_test, lwd=3)
lines(X_test[,"Year"], pred_obj$mean, col = 'red')

matplot(X_test[,"Year"],
        t(pred_obj$simulate(n = 1000L)),
        main = paste0("Population",
                      "\n" ,
                      "(1000 pred. simulations)"),
        ylim = c(110, 135),
        xlab="Year",
        ylab="Population",
        type='l')
lines(X_test[,"Year"], y_test, lwd=3)
