utils::install.packages("remotes")
remotes::install_github("thierrymoudiki/bayesianrvfl")
library("bayesianrvfl")

# 0 - dataset -----
X <- as.matrix(mtcars[,-1])
y <- mtcars$mpg
(n <- dim(X)[1]); (p <- dim(X)[2])


# 1 - Split data set into 3 parts -----
set.seed(1)
(idx_train <- caret::createDataPartition(y, p = 0.6)$Resample1)
(idx_comp <- generics::setdiff(1:n, idx_train))
(idx_validation <- base::sample(x = idx_comp,
                               size = floor(length(idx_comp)/2),
                               replace = FALSE))
(idx_test <- generics::setdiff(idx_comp, idx_validation))

# check
print(generics::intersect(idx_train, idx_validation))
print(generics::intersect(c(idx_train, idx_validation), idx_test))

X_train <- X[idx_train,]
y_train <- y[idx_train]
X_validation <- X[idx_validation,]
y_validation <- y[idx_validation]
X_test <- X[idx_test,]
y_test <- y[idx_test]


# 2 - Fit the model -----
# choose the 'best' regularization parameter
# (there are many other ways, and there are also
# other parameters, type 'head(bayesianrvfl::fit_rvfl)')
obj_GCV <- bayesianrvfl::fit_rvfl(x = X_train, y = y_train)
(best_lambda <- obj_GCV$lambda[which.min(obj_GCV$GCV)])

(fit_obj <- bayesianrvfl::fit_rvfl(x = X_train,
                                   y = y_train,
                                   method = "solve",
                                   lambda = best_lambda,
                                   compute_Sigma = TRUE))


# 3 - Predict on validation set -----
preds_validation <- bayesianrvfl::predict_rvfl(fit_obj,
                                               newx = X_validation)
level <- 95
multiplier <- qnorm(1 - (100 - level)/200)
summary(preds_validation$mean - y_validation)
preds_upper <- preds_validation$mean + multiplier*preds_validation$sd
preds_lower <- preds_validation$mean - multiplier*preds_validation$sd
# coverage rate
mean((preds_upper >= y_validation)*(preds_lower <= y_validation))


# 4 - Update -----
# add new points in an online fashion
fit_obj2 <- bayesianrvfl::update_params(fit_obj, newx = X[idx_test[1], ],
                                        newy = y[idx_test[1]])
fit_obj3 <- bayesianrvfl::update_params(fit_obj2, newx = X[idx_test[2], ],
                                        newy = y[idx_test[2]])
fit_obj4 <- bayesianrvfl::update_params(fit_obj3, newx = X[idx_test[3], ],
                                        newy = y[idx_test[3]])
fit_obj5 <- bayesianrvfl::update_params(fit_obj4, newx = X[idx_test[4], ],
                                        newy = y[idx_test[4]])
fit_obj6 <- bayesianrvfl::update_params(fit_obj5, newx = X[idx_test[5], ],
                                        newy = y[idx_test[5]])
fit_obj7 <- bayesianrvfl::update_params(fit_obj6, newx = X[idx_test[6], ],
                                        newy = y[idx_test[6]])

mat_coefs <- cbind(fit_obj$coef, fit_obj2$coef,
                    fit_obj3$coef, fit_obj4$coef,
                    fit_obj5$coef, fit_obj6$coef,
                   fit_obj7$coef)

preds_validation2 <- bayesianrvfl::predict_rvfl(fit_obj2,
                                                newx = X_validation)

preds_validation3 <- bayesianrvfl::predict_rvfl(fit_obj7,
                                                newx = X_validation)


# 5 - Plots -----

par(mfrow=c(3, 2))

plot(x = log(obj_GCV$lambda), y = obj_GCV$GCV, type='l',
     main = 'Generalized Cross-validation error',
     xlab = "log(lambda)", ylab = "GCV")

plot(y_validation, type='l', col="red",
     lwd=2,
     ylim = c(min(c(y_validation, preds_lower, preds_validation$mean)),
              max(c(y_validation, preds_upper, preds_validation$mean))),
     main = 'Out-of-sample credible intervals',
     xlab = "obs#", ylab = "prediction")
lines(preds_validation$mean, col="blue", lwd=2)
lines(preds_upper, col="gray")
lines(preds_lower, col="gray")

plot(x = y_validation,
     y = preds_validation$mean,
     ylim = c(min(c(y_validation, preds_lower, preds_validation$mean)),
              max(c(y_validation, preds_upper, preds_validation$mean))),
     main = 'observed vs predicted \n (before updates)',
     xlab = "observed", ylab = "predicted")
abline(a = 0, b = 1, col="green", lwd=2)

matplot(t(mat_coefs), type = 'l', lwd = 2,
        main = 'model coefficients \n after each update',
        xlab = "update#", ylab = "model coefficient")

plot(x = y_validation,
     y = preds_validation2$mean,
     ylim = c(min(c(y_validation, preds_lower, preds_validation$mean)),
              max(c(y_validation, preds_upper, preds_validation$mean))),
     main = 'observed vs predicted \n (after 6 point-updates)',
     xlab = "observed", ylab = "predicted")
abline(a = 0, b = 1, col="green", lwd=2)

matplot(t(preds_validation3$simulate(250)), type='l',
        lwd = 2, main = 'predictive posterior simulation \n (after 6 point-updates)',
        xlab = "obs#", ylab = "prediction")