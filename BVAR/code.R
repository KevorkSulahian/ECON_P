set.seed(42)
library(BVAR)

x <- fred_qd[, c("GDPC1", "PCECC96", "GPDIC1",
                 "HOANBS", "GDPCTPI", "FEDFUNDS")]

df <- readxl::read_xlsx("General Data sheet.xlsx")

df <- df[-c(1:8),-c(3,7,9)]
df <- df[-c(71:74),]
# df$`GDP Deflator: YoY: Quarterly: Armenia`[is.na(df$`GDP Deflator: YoY: Quarterly: Armenia`)]<-median(df$`GDP Deflator: YoY: Quarterly: Armenia`,na.rm=TRUE)
# x <- fred_transform(x, codes = c(4,4,4,4,4,1)) 
row_names <- df$Quarter
df <- df[,-c(1)]

df <- sapply(df, as.numeric)
df <- as.data.frame(df)
df <- fred_transform(df,type = "fred_qd",codes = c(4,4,4,4,4))

rownames(df) <- row_names
colnames(df) <- c('GDPnom', 'GDPreal','GFCFnom', 'GFCFreal','ICE')


mn <- bv_minnesota(b = 0,
   lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 0.0001, max = 5),
   alpha = bv_alpha(mode = 2), var = 1e07)


priors <- bv_priors( mn = mn)
# 
# mh <- bv_metropolis(scale_hess = c(0.05, 0.0001, 0.0001),
#                      adjust_acc = TRUE, acc_lower = 0.25, acc_upper = 0.45)

# run <- bvar(df, lags = 1, n_draw = 15000, n_burn = 5000, n_thin = 1,
#              priors = priors, verbose = TRUE)

run <- bvar(df, lags = 1, n_draw = 15000, n_burn = 5000,
                priors = priors, mh = bv_mh(scale_hess = 0.5, adjust_acc = TRUE),
                verbose = TRUE)
print(run)

plot(run)

plot(run, type = "dens",
      vars_response = "GDPnom", vars_impulse = "GDPnom-lag1")

fitted(run, type = "mean")

plot(residuals(run, type = "mean"), vars = c("GDPnom", "PCECC96"))

opt_irf <- bv_irf(horizon = 16, identification = TRUE)
irf(run) <- irf(run, opt_irf, conf_bands = c(0.05, 0.16))

# plot(irf(run), area = TRUE,
#       vars_impulse = c("GDPnom", "ICE"), vars_response = c(1:2, 6))

predict(run) <- predict(run, horizon = 6, conf_bands = c(0.05, 0.16))

plot(predict(run), area = TRUE, t_back = 32,
     vars = c('GDPnom'))

plot(df$GDPnom)

run[["fcast"]][["quants"]]

summary(run)

run$fcast


