set.seed(2018)

## Practical 1.1

install.packages("foreign")
library(foreign)
## set working directory where the file is
dat <- read.spss("Cancer.sav", to.data.frame = TRUE)


uniRegr <- function(DF, covariates){
 res <- matrix(NA, length(covariates), 3)
  for (i in 1:length(covariates)){
    form <- as.formula(paste("WEIGHIN ~", covariates[i]))
    fm <- lm(form, data = DF)
    res[i, ] <- summary(fm)$coefficients[2, c(1,2,4)]
  }
 colnames(res) <- c("estimate", "std error", "p-value")
 res
}


results_regr <- uniRegr(dat, c("AGE", "STAGE", "TRT"))
rownames(results_regr) <- c("AGE", "STAGE", "TRT")
results_regr

#form <- as.formula(paste("WEIGHIN ~", "TRT"))
#fm <- lm(form, data = dat)

## Practical 1.2 (extra)

uniLogRegr <- function(DF, covariates){
  res <- matrix(NA, length(covariates), 3)
  for (i in 1:length(covariates)){
    form <- as.formula(paste("TRT ~", covariates[i]))
    fm <- glm(form, data = DF, family = binomial)
    res[i, ] <- summary(fm)$coefficients[2, c(1,2,4)]
  }
  colnames(res) <- c("estimate", "std error", "p-value")
  res
}


results_log_regr <- uniLogRegr(dat, c("AGE", "STAGE", "WEIGHIN"))
rownames(results_log_regr) <- c("AGE", "STAGE", "WEIGHIN")
results_log_regr

#form <- as.formula(paste("TRT ~", "WEIGHIN"))
#fm <- glm(form, data = dat, family = binomial)

