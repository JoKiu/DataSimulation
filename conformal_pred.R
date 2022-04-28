function (x, y, x0, train.fun, predict.fun, alpha = 0.1, w = NULL, 
          mad.train.fun = NULL, mad.predict.fun = NULL, num.grid.pts = 100, 
          grid.factor = 1.25, verbose = FALSE) 
{
  x = as.matrix(x)
  y = as.numeric(y)
  n = nrow(x)
  p = ncol(x)
  x0 = matrix(x0, ncol = p)
  n0 = nrow(x0)
  check.args(x = x, y = y, x0 = x0, alpha = alpha, train.fun = train.fun, 
             predict.fun = predict.fun, mad.train.fun = mad.train.fun, 
             mad.predict.fun = mad.predict.fun)
  if (length(num.grid.pts) != 1 || !is.numeric(num.grid.pts) || 
      num.grid.pts <= 1 || num.grid.pts >= 1000 || round(num.grid.pts) != 
      num.grid.pts) {
    stop("num.grid.pts must be an integer between 1 and 1000")
  }
  check.pos.num(grid.factor)
  if (is.null(w)) 
    w = rep(1, n + n0)
  if (verbose == TRUE) 
    txt = ""
  if (verbose != TRUE && verbose != FALSE) {
    txt = verbose
    verbose = TRUE
  }
  if (verbose) 
    cat(sprintf("%sInitial training on full data set ...\n", 
                txt))
  out = train.fun(x, y)
  fit = matrix(predict.fun(out, x), nrow = n)
  pred = matrix(predict.fun(out, x0), nrow = n0)
  m = ncol(pred)
  ymax = max(abs(y))
  yvals = seq(-grid.factor * ymax, grid.factor * ymax, length = num.grid.pts)
  lo = up = matrix(0, n0, m)
  qvals = rvals = matrix(0, num.grid.pts, m)
  xx = rbind(x, rep(0, p))
  for (i in 1:n0) {
    if (verbose) {
      cat(sprintf("\r%sProcessing prediction point %i (of %i) ...", 
                  txt, i, n0))
      flush.console()
    }
    xx[n + 1, ] = x0[i, ]
    ww = c(w[1:n], w[n + i])
    for (j in 1:num.grid.pts) {
      yy = c(y, yvals[j])
      if (j == 1) 
        out = train.fun(xx, yy)
      else out = train.fun(xx, yy, out)
      r = abs(yy - matrix(predict.fun(out, xx), nrow = n + 
                            1))
      if (!is.null(mad.train.fun) && !is.null(mad.predict.fun)) {
        for (l in 1:m) {
          if (j == 1 && l == 1) 
            out.mad = mad.train.fun(xx, r[, l])
          else out.mad = mad.train.fun(xx, r[, l], out.mad)
          r[, l] = r[, l]/mad.predict.fun(out.mad, xx)
        }
      }
      qvals[j, ] = apply(r, 2, weighted.quantile, prob = 1 - 
                           alpha, w = ww)
      rvals[j, ] = r[n + 1, ]
    }
    for (l in 1:m) {
      int = grid.interval(yvals, rvals[, l], qvals[, l])
      lo[i, l] = int$lo
      up[i, l] = int$up
      browser()
    }
  }
  if (verbose) 
    cat("\n")
  return(list(pred = pred, lo = lo, up = up, fit = fit))
}