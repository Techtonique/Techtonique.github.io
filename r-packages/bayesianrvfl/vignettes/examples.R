# load("~/Documents/R_Packages/misc/titanic/data/train_clean.RData")  # 891 obs
# load("~/Documents/R_Packages/misc/titanic/data/test_clean.RData")    # 418 obs
# test <- test[ , match(colnames(train), colnames(test))]
#
# x_train <- rbind(train, test)
# x <- x_train[, -match(c("name", "ticket", "cabin"), colnames(train))]
#
# x <- model.matrix(~ ., data = x[,-1])
# y <- as.numeric(x_train$survived)
# x <- x[,-1]
#
# train_index <- createDataPartition(y, p = 0.7)[[1]]
#
# lams <- 10^seq(-2, 10, length.out = 100)
# vec_nb_hidden <- floor(seq(0, 100, by = 1)[-1])
# res_cv <- cv_rvfl(x = x[train_index, ], y = y[train_index],
#                   k = 10, repeats = 5,
#                   vec_nb_hidden = vec_nb_hidden,
#                   lams = lams,
#                   seed = 1, cl = 4)
#
# coord_min <- which(res_cv == min(res_cv), arr.ind = TRUE)
# res_cv[coord_min[1], coord_min[2]]
# (best_nb_hidden <- vec_nb_hidden[coord_min[1]])
# (best_lam <- lams[coord_min[2]])
# summary(y)
#
# fit_obj <- fit_rvfl(x = x[train_index, ], y = y[train_index],
#                     nb_hidden = best_nb_hidden, lambda = best_lam,
#                     compute_Sigma = TRUE)
# (preds <- predict_rvfl(fit_obj, newx = x[-train_index, ]))
#
#
# fit_obj <- fit_rvfl(x = x[train_index, ], y = y[train_index],
#                     nodes_sim = nodes_sim,
#                     nb_hidden = best_nb_hidden,
#                     lambda = best_lam,
#                     compute_Sigma = TRUE)
# preds <- predict_rvfl(fit_obj, newx = x[-train_index, ])
# preds <- as.numeric(preds$mean)
# preds[which(preds < 1.5)] <- 1
# preds[which(preds >= 1.5)] <- 2
# sum((preds == y[-train_index]))/length(y[-train_index])


# Do random forest

# fit_rf <- randomForest(x = x[train_index, ], y = as.factor(y[train_index]),
#                        ntree = 1000)
# preds_rf <- predict(fit_rf, newdata = x[-train_index, ])
# sum((preds_rf == y[-train_index]))/length(y[-train_index])
#
#
# auc_probability <- function(labels, scores, N=1e7){
#   pos <- sample(scores[labels], N, replace=TRUE)
#   neg <- sample(scores[!labels], N, replace=TRUE)
#   # sum( (1 + sign(pos - neg))/2)/N # does the same thing
#   (sum(pos > neg) + sum(pos == neg)/2) / N # give partial credit for ties
# }
#
# auc_probability(as.logical(y[-train_index]-1), as.numeric(preds_rf))
# auc_probability(as.logical(y[-train_index]-1), preds)

### Try many nn's
# model_grid <- as.matrix(expand.grid(nodes_sim = c("sobol", "halton", "unif"),
# activ = c("relu", "sigmoid", "tanh",
#           "leakyrelu", "elu", "linear")))
#
# #lams <- 10^seq(-2, 10, length.out = 100)
# lams <- 10^seq(0, 3, length.out = 1000)
# vec_nb_hidden <- floor(seq(0, 30, by = 1)[-1])
#
# fit_model_grid <- foreach(i = 1:nrow(model_grid)) %do% {
#   model_params <- as.vector(model_grid[i, ])
#   cat(i," - nodes simulation:", model_params[1], ", activation function: ", model_params[2], "\n")
#   res <- cv_rvfl(x = x[train_index, ], y = y[train_index],
#         k = 10, repeats = 10, nodes_sim =
#         model_params[1], activ = model_params[2],
#         vec_nb_hidden = vec_nb_hidden,
#         lams = lams,
#         seed = 1, cl = 4)
#   print(res)
#   res
# }
#
# best_params <- lapply(fit_model_grid, function (res_cv) {
#   #print(res_cv)
#   coord_min <- which(res_cv == min(res_cv), arr.ind = TRUE)
#   #res_cv[coord_min[1], coord_min[2]]
#   best_nb_hidden <- vec_nb_hidden[coord_min[1]]
#   best_lam <- lams[coord_min[2]]
#   c(best_nb_hidden, best_lam)
# })
#
# fit_objs <- lapply(1:length(best_params),
#                    function(i) fit_rvfl(x = x[train_index, ], y = y[train_index],
#                     nodes_sim = model_grid[i, 1],
#                     activ = model_grid[i, 2],
#                     nb_hidden = best_params[[i]][1],
#                     lambda = best_params[[i]][2],
#                     compute_Sigma = TRUE))
#
# preds_list <- lapply(fit_objs, function(fit_obj) predict_rvfl(fit_obj, newx = x[-train_index, ])$mean)
#
# prefs <- lapply(preds_list, function(preds) {preds[which(preds < 1.5)] <- 1
# preds[which(preds >= 1.5)] <- 2
# c(sum((preds == y[-train_index]))/length(y[-train_index]),
# auc_probability(as.logical(y[-train_index]-1), preds))
# })
# prefs
# # Do majority voting and check again.
# # Do majority voting and check again.
# # Do majority voting and check again.
# # Do majority voting and check again.
# # Do majority voting and check again.
# corr_mat <- matrix(NA, nrow = length(preds_list), ncol = length(preds_list))
# for (i in 1:length(preds_list))
# {
#   for (j in 1:length(preds_list))
#   {
#     corr_mat[i, j] <- cor(preds_list[[i]], preds_list[[j]])
#   }
# }
#
#
# preds_classes <- sapply(preds_list, function(preds) {preds[which(preds < 1.5)] <- 1;
# preds[which(preds >= 1.5)] <- 2; preds})
#
# votes <- round(sapply(1:nrow(preds_classes),
#              function(x) sum(preds_classes[x, ] == 1)/length(preds_classes[x, ]))*100)
#
# preds_majority <- rep(2, length(votes))
# preds_majority[(votes > 50)] <- 1
#
# sum((preds_majority == y[-train_index]))/length(y[-train_index])
# auc_probability(as.logical(y[-train_index]-1), preds_majority)
#
# # # 1 - Exemple longley ---------------------------------------------------------
# #
library(caret)
lams <- 10^seq(1, 2, length.out = 100)
vec_nb_hidden <- seq(600, 850, by = 10)
x <- data.matrix(longley[, 1:6])
y <- longley[, "Employed"]
train_index <- createDataPartition(y, p = 0.7)[[1]]

(res_cv <- cv_rvfl(x = x[train_index, ], y = y[train_index],
                  k = 5, repeats = 1,
                  vec_nb_hidden = vec_nb_hidden,
                  lams = lams,
                  #cl = 4,
                  seed = 1))

fit_obj <- fit_rvfl(x = x[train_index, ], y = y[train_index],
                    nb_hidden = res_cv$best_nb_hidden,
                    lambda = res_cv$best_lam,
                    n_clusters = res_cv$best_n_clusters,
                    compute_Sigma = TRUE)

(preds <- predict_rvfl(fit_obj, newx = x[-train_index, ]))

sqrt(mean((preds$mean - y[-train_index])^2))

qt95 <- qnorm(1-0.05/2)
qt80 <- qnorm(1-0.1/2)
upper <- preds$mean + qt95*preds$sd
lower <- preds$mean - qt95*preds$sd
upper80 <- preds$mean + qt80*preds$sd
lower80 <- preds$mean - qt80*preds$sd

yy <- c(lower, rev(upper))
yy80 <- c(lower80, rev(upper80))
nbyy <- length(upper)
xx <- c(1:nbyy, nbyy:1)

plot(1:nbyy, y[-train_index], type = 'l',
     ylim = c(min(lower), max(upper)), lwd = 2)
polygon(xx, yy, col = "gray80", border = FALSE)
polygon(xx, yy80, col = "gray60", border = FALSE)
lines(1:nbyy, y[-train_index], lwd = 2)
lines(preds$mean, col = "blue", lwd = 2)


# 2 - Exemple SLC14 ---------------------------------------------------------

library(caret)
set.seed(123)
train_dat <- SLC14_1(250)

head(train_dat)
str(train_dat)

x <- train_dat[, -ncol(train_dat)]
y <- train_dat[, ncol(train_dat)]
train_index <- caret::createDataPartition(y, p = 0.7)[[1]]

lams <- seq(1, 2, length.out = 100)
vec_nb_hidden <- seq(900, 1000, by = 25)
res_cv <- cv_rvfl(x = x[train_index, ], y = y[train_index],
                  k = 10, repeats = 5,
                  vec_nb_hidden = vec_nb_hidden,
                  lams = lams,
                  seed = 1)

fit_obj <- fit_rvfl(x = x[train_index, ], y = y[train_index],
                    nb_hidden = res_cv$best_nb_hidden,
                    lambda = res_cv$best_lam,
                    n_clusters = res_cv$best_n_clusters,
                    compute_Sigma = TRUE)
(preds <- predict_rvfl(fit_obj, newx = as.matrix(x[-train_index, ])))

sqrt(mean((preds$mean - y[-train_index])^2))


qt95 <- qnorm(1-0.05/2)
qt80 <- qnorm(1-0.1/2)
upper <- preds$mean + qt95*preds$sd
lower <- preds$mean - qt95*preds$sd
upper80 <- preds$mean + qt80*preds$sd
lower80 <- preds$mean - qt80*preds$sd

yy <- c(lower, rev(upper))
yy80 <- c(lower80, rev(upper80))
nbyy <- length(upper)
xx <- c(1:nbyy, nbyy:1)

plot(1:nbyy, y[-train_index], type = 'l',
     ylim = c(min(lower), max(upper)), lwd = 2)
polygon(xx, yy, col = "gray80", border = FALSE)
polygon(xx, yy80, col = "gray60", border = FALSE)
lines(1:nbyy, y[-train_index], lwd = 2)
lines(preds$mean, col = "blue", lwd = 2)


# 2 - Exemple SLC14_2 ---------------------------------------------------------

library(caret)
set.seed(123)
train_dat <- SLC14_2(250)

head(train_dat)
str(train_dat)

x <- train_dat[, -ncol(train_dat)]
y <- train_dat[, ncol(train_dat)]
train_index <- createDataPartition(y, p = 0.7)[[1]]

lams <- seq(10, 20, length.out = 100)
vec_nb_hidden <- seq(700, 900, by = 25)
res_cv <- cv_rvfl(x = x[train_index, ], y = y[train_index],
                  k = 10, repeats = 5,
                  vec_nb_hidden = vec_nb_hidden,
                  lams = lams,
                  seed = 1, cl = 4)

coord_min <- which(res_cv == min(res_cv), arr.ind = TRUE)
res_cv[coord_min[1], coord_min[2]]
(best_nb_hidden <- vec_nb_hidden[coord_min[1]])
(best_lam <- lams[coord_min[2]])
summary(y)

fit_obj <- fit_rvfl(x = x[train_index, ], y = y[train_index],
                    nb_hidden = best_nb_hidden, lambda = best_lam,
                    compute_Sigma = TRUE)
(preds <- predict_rvfl(fit_obj, newx = x[-train_index, ]))

sqrt(mean((preds$mean - y[-train_index])^2))


qt95 <- qnorm(1-0.05/2)
qt80 <- qnorm(1-0.1/2)
upper <- preds$mean + qt95*preds$sd
lower <- preds$mean - qt95*preds$sd
upper80 <- preds$mean + qt80*preds$sd
lower80 <- preds$mean - qt80*preds$sd

yy <- c(lower, rev(upper))
yy80 <- c(lower80, rev(upper80))
nbyy <- length(upper)
xx <- c(1:nbyy, nbyy:1)

plot(1:nbyy, y[-train_index], type = 'l',
     ylim = c(min(lower), max(upper)), lwd = 2)
polygon(xx, yy, col = "gray80", border = FALSE)
polygon(xx, yy80, col = "gray60", border = FALSE)
lines(1:nbyy, y[-train_index], lwd = 2)
lines(preds$mean, col = "blue", lwd = 2)


  # 3 - Exemple mtcars ---------------------------------------------------------

x <- as.matrix(mtcars[, -1])
y <- as.vector(mtcars[, 1])

train_index <- createDataPartition(y, p = 0.7)[[1]]

lams <- seq(10, 20, length.out = 100)
vec_nb_hidden <- 1:10

nodes_sim <- "halton"
activ <- "relu"
lams <- 10^seq(0, 2, length.out = 200)
vec_nb_hidden <- 1:10

res_cv <- cv_rvfl(x = x[train_index, ], y = y[train_index],
                  nodes_sim = nodes_sim, activ = activ,
                  k = 5, repeats = 10,
                  vec_nb_hidden = vec_nb_hidden,
                  lams = lams,
                  seed = 1)

(coord_min <- which(res_cv == min(res_cv), arr.ind = TRUE))
res_cv[coord_min[1], coord_min[2]]
(best_nb_hidden <- vec_nb_hidden[coord_min[1]])
(best_lam <- lams[coord_min[2]])

fit_obj <- fit_rvfl(x = x[train_index, ], y = y[train_index],
                    nodes_sim = nodes_sim, activ = activ,
                    nb_hidden = best_nb_hidden, lambda = best_lam,
                    compute_Sigma = TRUE)
(preds <- predict_rvfl(fit_obj, newx = x[-train_index, ]))

sqrt(mean((preds$mean - y[-train_index])^2))

qt95 <- qnorm(1-0.05/2)
qt80 <- qnorm(1-0.1/2)
upper <- preds$mean + qt95*preds$sd
lower <- preds$mean - qt95*preds$sd
upper80 <- preds$mean + qt80*preds$sd
lower80 <- preds$mean - qt80*preds$sd

yy <- c(lower, rev(upper))
yy80 <- c(lower80, rev(upper80))
nbyy <- length(upper)
xx <- c(1:nbyy, nbyy:1)

plot(1:nbyy, y[-train_index], type = 'l',
     ylim = c(min(lower), max(upper)), lwd = 2)
polygon(xx, yy, col = "gray80", border = FALSE)
polygon(xx, yy80, col = "gray60", border = FALSE)
lines(1:nbyy, y[-train_index], lwd = 2)
lines(preds$mean, col = "blue", lwd = 2)

# example 1D --------------------------------------------------------------

fw <- function (x)
  10*sin(0.3*x)*sin(1.3*x^2) + 0.00001*x^4 + 0.2*x+80

x <- seq(from = -50, to = 50, length.out = 10)
y <- fw(x)
(best_params <- find_lam_nbhidden(x = x, y = y))

fit_obj <- fit_rvfl(x = x, y = y, nb_hidden = best_params$best_nb_hidden,
                    lambda = best_params$best_lambda,
                    compute_Sigma = TRUE)
preds <- predict_rvfl(fit_obj, newx = as.matrix(x))
upper_bound <- preds$mean + 1.96*preds$sd
lower_bound <- preds$mean - 1.96*preds$sd

plot(x, y, type = 'l',
     ylim = c(min(lower_bound), max(upper_bound)))
lines(x, preds$mean, col = 'red')
lines(x, upper_bound, col = 'blue')
lines(x, lower_bound, col = 'blue')

# fit emcee -----

library(MASS)

par(mfrow=c(2, 3))

set.seed(123)

mtcars
data(mtcars)
train_index <- caret::createDataPartition(mtcars$mpg, p=0.8)[[1]]
obj <- fit_rvfl_mcmc(mtcars[train_index, -1],
                     mtcars$mpg[train_index], compute_Sigma = TRUE)
(pred_obj <- predict_rvfl_mcmc(obj,
                               newx = as.matrix(mtcars[-train_index,-1])))

n_preds <- length(pred_obj$mean)
xx <- c(1:n_preds, rev(1:n_preds))
yy95 <- c(pred_obj$mean + 1.96*pred_obj$sd,
        rev(pred_obj$mean - 1.96*pred_obj$sd))
yy80 <- c(pred_obj$mean + 1.28*pred_obj$sd,
        rev(pred_obj$mean - 1.28*pred_obj$sd))
plot(xx, yy95, type = "n", main = "mtcars")
polygon(xx, yy95, col = "gray90", border = "gray90")
polygon(xx, yy80, col = "gray", border = "gray")
lines(1:n_preds, mtcars[-train_index, 1])
points(1:n_preds, mtcars[-train_index, 1], pch=20)
lines(1:n_preds, pred_obj$mean, col = 'red')
points(1:n_preds, pred_obj$mean, col = 'red', pch=20)
#
# # longley
longley # not the same as the S-PLUS dataset
names(longley)[1] <- "y"
train_index <- caret::createDataPartition(longley$y, p=0.75)[[1]]
obj <- fit_rvfl_mcmc(longley[train_index, -1],
                     longley$y[train_index], compute_Sigma = TRUE)
(pred_obj <- predict_rvfl_mcmc(obj,
                               newx = as.matrix(longley[-train_index, -1])))


n_preds <- length(pred_obj$mean)
xx <- c(1:n_preds, rev(1:n_preds))
yy95 <- c(pred_obj$mean + 1.96*pred_obj$sd,
          rev(pred_obj$mean - 1.96*pred_obj$sd))
yy80 <- c(pred_obj$mean + 1.28*pred_obj$sd,
          rev(pred_obj$mean - 1.28*pred_obj$sd))
plot(xx, yy95, type = "n", main = "longley")
polygon(xx, yy95, col = "gray90", border = "gray90")
polygon(xx, yy80, col = "gray", border = "gray")
lines(1:n_preds, longley[-train_index, 1])
points(1:n_preds, longley[-train_index, 1], pch=20)
lines(1:n_preds, pred_obj$mean, col = 'red')
points(1:n_preds, pred_obj$mean, col = 'red', pch=20)
#
# # quakes
# data(quakes)
#
train_index <- caret::createDataPartition(quakes[, 4], p=0.8)[[1]]
obj <- fit_rvfl_mcmc(quakes[train_index, -4],
                     quakes[train_index, 4],
                     compute_Sigma = TRUE)
(pred_obj <- predict_rvfl_mcmc(obj,
                               newx = as.matrix(quakes[-train_index, -4])))


n_preds <- length(pred_obj$mean)
xx <- c(1:n_preds, rev(1:n_preds))
yy95 <- c(pred_obj$mean + 1.96*pred_obj$sd,
          rev(pred_obj$mean - 1.96*pred_obj$sd))
yy80 <- c(pred_obj$mean + 1.28*pred_obj$sd,
          rev(pred_obj$mean - 1.28*pred_obj$sd))
plot(xx, yy95, type = "n", main = "quakes",
     ylim = c(2, 8))
polygon(xx, yy95, col = "gray90", border = "gray90")
polygon(xx, yy80, col = "gray", border = "gray")
lines(1:n_preds, quakes[-train_index, 4])
lines(1:n_preds, pred_obj$mean, col = 'red')

# Boston
library(MASS)
data("Boston")

train_index <- caret::createDataPartition(Boston[, 1], p=0.8)[[1]]

obj <- fit_rvfl_mcmc(x = Boston[train_index, -1],
                     y = Boston[train_index, 1],
                     compute_Sigma = TRUE, cl=4)
(pred_obj <- predict_rvfl_mcmc(obj,
                               newx = as.matrix(Boston[-train_index, -1])))


n_preds <- length(pred_obj$mean)
xx <- c(1:n_preds, rev(1:n_preds))
yy95 <- c(pred_obj$mean + 1.96*pred_obj$sd,
          rev(pred_obj$mean - 1.96*pred_obj$sd))
yy80 <- c(pred_obj$mean + 1.28*pred_obj$sd,
          rev(pred_obj$mean - 1.28*pred_obj$sd))
plot(xx, yy95, type = "n", main = "Boston")
polygon(xx, yy95, col = "gray90", border = "gray90")
polygon(xx, yy80, col = "gray", border = "gray")
lines(1:n_preds, Boston[-train_index, 1])
lines(1:n_preds, pred_obj$mean, col = 'red')


data("UScrime")
train_index <- caret::createDataPartition(UScrime$y, p=0.8)[[1]]
obj <- fit_rvfl_mcmc(x = UScrime[train_index, -16],
                     y = UScrime[train_index, 16],
                     compute_Sigma = TRUE, cl=4)
(pred_obj <- predict_rvfl_mcmc(obj,
                               newx = as.matrix(UScrime[-train_index, -16])))

n_preds <- length(pred_obj$mean)
xx <- c(1:n_preds, rev(1:n_preds))
yy95 <- c(pred_obj$mean + 1.96*pred_obj$sd,
        rev(pred_obj$mean - 1.96*pred_obj$sd))
yy80 <- c(pred_obj$mean + 1.28*pred_obj$sd,
          rev(pred_obj$mean - 1.28*pred_obj$sd))
plot(xx, yy95, type = "n", main = "UScrime")
polygon(xx, yy95, col = "gray90", border = "gray90")
polygon(xx, yy80, col = "gray", border = "gray")
lines(1:n_preds, UScrime[-train_index, 16], type = 'l')
points(1:n_preds, UScrime[-train_index, 16],  pch=20)
lines(1:n_preds, pred_obj$mean, col = 'red')
points(1:n_preds, pred_obj$mean, col = 'red', pch=20)


data("motors")
train_index <- caret::createDataPartition(motors$time, p=0.8)[[1]]
obj <- fit_rvfl_mcmc(x = motors[train_index, c(1, 3)],
                     y = motors$time[train_index],
                     compute_Sigma = TRUE, cl=4)
(pred_obj <- predict_rvfl_mcmc(obj,
                               newx = as.matrix(motors[-train_index, c(1, 3)])))

n_preds <- length(pred_obj$mean)
xx <- c(1:n_preds, rev(1:n_preds))
yy95 <- c(pred_obj$mean + 1.96*pred_obj$sd,
          rev(pred_obj$mean - 1.96*pred_obj$sd))
yy80 <- c(pred_obj$mean + 1.28*pred_obj$sd,
          rev(pred_obj$mean - 1.28*pred_obj$sd))
plot(xx, yy95, type = "n", main = "motors")
polygon(xx, yy95, col = "gray90", border = "gray90")
polygon(xx, yy80, col = "gray", border = "gray")
lines(1:n_preds, motors[-train_index, 2], type = 'l')
points(1:n_preds, motors[-train_index, 2], pch=20)
lines(1:n_preds, pred_obj$mean, col = 'red')
points(1:n_preds, pred_obj$mean, col = 'red', pch=20)

# fit glmnet -----

par(mfrow = c(2, 2))

fit_obj <- bayesianrvfl::fit_glmnet(x = mtcars[,-1], y = mtcars[,1],
                                    nb_hidden = 50, compute_Sigma = TRUE)
preds <- predict_glmnet(fit_obj, newx = mtcars[,-1], s = 0.05)

plot(preds$mean, ylim = c(0, 40), type = 'l')
lines(mtcars[,1], col = "blue")
lines(preds$mean - 1.96*preds$sd, col = "red")
lines(preds$mean + 1.96*preds$sd, col = "red")



X <- longley[,-1]
y <- longley[,1]
fit_obj <- bayesianrvfl::fit_glmnet(x = X, y = y,
                                     nb_hidden = 100, compute_Sigma = TRUE)
preds <- predict_glmnet(fit_obj, newx = X, s = 0.05)

 plot(preds$mean, ylim = c(50, 150), type = 'l')
 lines(y, col = "blue")
 lines(preds$mean - 1.96*preds$sd, col = "red")
 lines(preds$mean + 1.96*preds$sd, col = "red")

set.seed(123)
n <- 25 ; p <- 2
X <- matrix(rnorm(n * p), n, p) # no intercept!
y <- rnorm(n)
fit_obj <- bayesianrvfl::fit_glmnet(x = X, y = y,
                                     nb_hidden = 200, alpha = 1, compute_Sigma = TRUE)
preds <- predict_glmnet(fit_obj, newx = X, s = 0.1)

 plot(preds$mean, ylim = c(-2, 3), type = 'l')
 lines(y, col = "blue")
 lines(preds$mean - 1.96*preds$sd, col = "red")
 lines(preds$mean + 1.96*preds$sd, col = "red")


set.seed(225)
n <- 25 ; p <- 2
X <- matrix(rnorm(n * p), n, p) # no intercept!
y <- rnorm(n)
fit_obj <- bayesianrvfl::fit_glmnet(x = X, y = y,
                                     nb_hidden = 200, compute_Sigma = TRUE)
preds <- predict_glmnet(fit_obj, newx = X, s = 0.1)

 plot(preds$mean, ylim = c(-2, 3), type = 'l')
 lines(y, col = "blue")
 lines(preds$mean - 1.96*preds$sd, col = "red")
 lines(preds$mean + 1.96*preds$sd, col = "red")
