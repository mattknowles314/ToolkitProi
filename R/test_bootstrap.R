df$id <- seq(1:dim(df)[1])
train <- df %>% sample_frac(1, replace = TRUE)
test <- anti_join(df,train, by = "id") %>% select(-id)

sub_fit <- update(fit, train %>% select(-id))
sub_nb <- naive_bayes(bin_severity ~., data = train %>% select(-id), laplace = 0.05)

sub_predicts <- predict.glm(
    sub_fit,
    newdata = test,
    type = "response"
)

predicts_nb <- predict(
    sub_nb,
    newdata = test,
    type = "class"
)

predictions_glm <- ifelse(sub_predicts <= 0.5, 0, 1)
X <- data.frame(test$bin_severity, predictions_glm, predicts_nb)
X$GLM_Result <- ifelse(X$test.bin_severity == X$predictions_glm, 1, 0)
X$NB_Result <- ifelse(X$test.bin_severity == X$predictions_nb, 1, 0)

X$GLMFalseSevere <- ifelse(X$predictions_glm == 1 & X$test.bin_severity == 0, 1, 0)
X$GLMFalseSlight <- ifelse(X$predictions_glm == 0 & X$test.bin_severity == 1, 1, 0)
X$NBFalseSevere <- ifelse(X$predicts_nb == 1 & X$test.bin_severity == 0, 1, 0)
X$NBFalseSlight <- ifelse(X$predicts_nb == 0 & X$test.bin_severity == 1, 1, 0)

