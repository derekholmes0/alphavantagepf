# av opt fit

# Total variance w(k, theta) under SSVI, power-law phi
ssvi_w <- function(k, theta, rho, eta, gamma) {
  phi <- eta / (theta^gamma * (1 + theta)^(1 - gamma))
  theta / 2 * (1 + rho * phi * k + sqrt((phi * k + rho)^2 + 1 - rho^2))
}

# Sufficient no-butterfly-arbitrage conditions (Gatheral & Jacquier, Thm 4.2)
# FOr all theta>0
butterfly_ok <- function(theta, rho, eta, gamma) {
  phi <- eta / (theta^gamma * (1 + theta)^(1 - gamma))
  all(theta * phi * (1 + abs(rho)) < 4 &
        theta * phi^2 * (1 + abs(rho)) <= 4)
}


# Implied vol anywhere on the surface; theta linearly interpolated in T
#' @importFrom stats approx
predict_iv <- function(fit, k, T) {
  theta <- approx(c(0, fit$mats), c(0, fit$theta), xout = T, rule = 2)$y
  sqrt(ssvi_w(k, theta, fit$rho, fit$eta, fit$gamma) / T)
}

# Black-Scholes vega in forward terms: F * dnorm(d1) * sqrt(T)
# k = log(K/F), so d1 = (-k + 0.5 * sigma^2 * T) / (sigma * sqrt(T))
#' @importFrom stats dnorm
bs_vega <- function(k, T, iv, F = 1) {
  d1 <- (-k + 0.5 * iv^2 * T) / (iv * sqrt(T))
  F * dnorm(d1) * sqrt(T)
}

# df: columns k, T, iv
# optional columns: F (forward, for cross-expiry price scaling),
#                   w_extra (any extra weight, e.g. 1 / bid-ask spread)
#' @importFrom stats spline optim plogis
fit_ssvi <- function(df, weighting = c("vega", "none"), normalize = TRUE) {
  theta=NULL
  weighting <- match.arg(weighting)
  df$F <- df$F %||% 1
  df$w_extra <- df$w_extra %||% 1
  df$w <- df$iv^2 * df$T

  # ATM total variance per maturity, forced non-decreasing in T
  mats <- sort(unique(df$T))
  theta_T <- sapply(mats, function(Ti) {
    s <- df[abs(df$T-Ti)<0.0001, ]
    spline(s$k, s$w, xout = 0)$y
    #as.numeric(predict(loess(w ~ k,s),data.frame(k=c(0)))) # More stable around illiquid data
  })
  theta_T <- cummax(theta_T)
  good_theta <- which(theta_T>0)
  theta_T <- theta_T[good_theta]
  mats <- mats[good_theta]

  df$theta <- theta_T[match(df$T, mats)]
  df <- df[theta>0]

  # Weights are fixed up front, computed from market IVs.
  # Residual is in vol space, so the weight on the squared residual is vega^2.
  wt <- if (weighting == "vega") bs_vega(df$k, df$T, df$iv, df$F)^2 else 1
  wt <- wt * df$w_extra^2
  if (normalize) wt <- wt / sum(wt)
  obj <- function(p) {
    rho <- tanh(p[1]); eta <- exp(p[2]); gamma <- plogis(p[3])
    if (!butterfly_ok(theta_T, rho, eta, gamma)) return(1e3)
    iv_mod <- sqrt(ssvi_w(df$k, df$theta, rho, eta, gamma) / df$T)
    sum(wt * (iv_mod - df$iv)^2)
  }

  best <- NULL
  for (start in list(c(-0.5, 0, 0), c(-0.2, -1, 1), c(-0.7, 0.5, -1))) {
    o <- optim(start, obj, method = "Nelder-Mead", control = list(maxit = 5000, reltol = 1e-12))
    if (is.null(best) || o$value < best$value) { best <- o }
  }

  rho <- tanh(best$par[1]); eta <- exp(best$par[2]); gamma <- plogis(best$par[3])
  iv_fit <- sqrt(ssvi_w(df$k, df$theta, rho, eta, gamma) / df$T)

  structure(list(rho = rho, eta = eta, gamma = gamma,
                 mats = mats, theta = theta_T,
                 weighting = weighting, objective = best$value,
                 rmse_vol = sqrt(mean((iv_fit - df$iv)^2)),
                 wrmse_vol = sqrt(sum(wt * (iv_fit - df$iv)^2) / sum(wt))),
            class = "ssvi_fit")
}

predict_iv <- function(fit, k, T) {
  theta <- approx(c(0, fit$mats), c(0, fit$theta), xout = T, rule = 2)$y
  sqrt(ssvi_w(k, theta, fit$rho, fit$eta, fit$gamma) / T)
}

df_from_two_dates <- function(dt1,dt2) {
  rt <- the_av$pxd[symbol=="SOFR" & !is.na(close) & timestamp<=dt1,][,.SD[.N]]$close/100
  return(1/(1+rt*as.numeric(dt2-dt1)/360))
}

iv_one_symbol <- function(thissymbol,dtset, minobs=12, winsorlevel=0.05,ds=NULL) {  # Set in calling function
  moneyn=ts=df=mark_call=mark_put=strike=expcode=iv_call=iv_put=delta_call=delta_put=iv=good=k=dtoexp=cexpcode=NULL
  dcat=cutlevel=usegood=bad=NULL
  message_if_green(TRUE,"Implied Vols:",thissymbol," from ",as.Date(min(dtset))," to ",as.Date(max(dtset)), " (",length(dtset)," days)")
  closest_deltamap <- avsd$deltamap[listnm=="closest",][,.(dcat,moneyn,cutlevel)]
  deltaset <- closest_deltamap$cutlevel
  deltalabels <- closest_deltamap[!(moneyn=="NA"),]$moneyn
  ds <- ds %||% arrow::open_dataset(paste0(the_av$cachedir,"/eqopt"),partitioning=c("symbol"))
  u1 <- ds |> filter(symbol==thissymbol) |> dplyr::collect() |> as.data.table()

  one_iv <- function(dt) {
    u2 = u1[ts==dt & expiration>ts,]
    u3 <- dcast(u2,ts+expcode+expiration+strike ~ type, value.var=c("iv","delta","mark"))
    urf = u3[,.N,by=.(ts,expiration)][, df:=df_from_two_dates(ts,expiration), by=.I]
    ufwd = urf[u3,on=.(ts,expiration)][,F:=(mark_call-mark_put)/df+strike]

    # Here, taking half the options
    dfinpall = ufwd[,.(ts,expiration,strike,F)][u2,on=.(ts,expiration,strike)][
      ,.(ts,expcode,strike,type,delta,F,k=log(strike/F),iv=iv/100,T=as.numeric(expiration-ts)/365)]
    dfinpall = dfinpall[,let(use=(F<strike & type=="call") | (F>=strike & type=="put"),bad=(is.na(iv) | iv<=0.02))]
    dfinpall = dfinpall[,usegood:=(use & !bad)]

    if( (nbad<-nrow(dfinpall[usegood==TRUE,]))<=minobs) {
      message_if_red(TRUE,"one_iv(",thissymbol,",",dt,") not enough data (",nbad," obs) to calcluate a surface.  Skipping.")
      return(data.table())
    }
    #message("one_iv(",thissymbol,",",dt,") using ",nrow(dfinpall[usegood==TRUE,]), " data points of ",nrow(dfinpall)," original")
    if(nrow ( dfinpall[usegood==TRUE,] )<=4) { cAssign("thissymbol;dt;dfinpall")}
    fit <- fit_ssvi(dfinpall[usegood==TRUE,],weighting="vega")
    if( length(fit$mats)<=0 ) {
      message_if_red(TRUE,"one_iv(",thissymbol,",",dt,") Got a fit, but not enough mats to cover space")
      return(data.table())
    }

    # Predict by actual maturity
    grida <- dfinpall[,moneyn:=cut(delta,deltaset,labels=deltalabels)][!is.na(moneyn)][,.(ts,expcode,type,strike,T,k,delta, moneyn)]
    grid <- rbindlist(list(grida[type=="call",][,.SD[.N], by=.(ts,expcode,moneyn)],
                           grida[type=="put",][,.SD[1], by=.(ts,expcode,moneyn)]
    ))
    grid$iv <-  predict_iv(fit, grid$k, grid$T)

    # Predict by Constant Maturity
    cmo_mats = grid[,.(dtoexp=365*max(T)), by=.(expcode)][order(dtoexp)]
    ktoget = cmo_mats[ avsd$deltamap[listnm=="cm",.(dtoexp,cexpcode)], on=.(dtoexp), roll=T]
    grid_cmo = grid[ktoget,on=.(expcode),allow.cartesian=TRUE][,T:=dtoexp/365]
    grid_cmo$iv = predict_iv(fit, grid_cmo$k, grid_cmo$T)
    tortn = rbindlist(list(grid[,.(ts,expcode,moneyn,strike,k,iv)],grid_cmo[,.(ts,expcode=cexpcode,moneyn,strike,k,iv)]))
    tortn = tortn[is.finite(iv)] # Sometimes not enough tenors are liquid
    return(tortn)
  }
  alldates = rbindlist(lapply(dtset, \(dt) one_iv(dt)))
  return(alldates[,symbol:=thissymbol])
}

