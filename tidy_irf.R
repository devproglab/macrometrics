# Вспомогательная функция для удобства работы с IRF. Не имеет отношения к самой процедуре идентификации
tidy_irf <- function(x, varlist = NULL) {
  if (inherits(x, "varirf")) {
    list_to_long <- function(lst) {
      if (is.null(lst) || length(lst) == 0) return(NULL)
      do.call(rbind, lapply(names(lst), function(shock) {
        mat <- lst[[shock]]
        h_vals <- as.integer(rownames(mat))
        if (is.na(h_vals[1])) h_vals <- 0:(nrow(mat) - 1)
        var_names <- colnames(mat)
        if (is.null(var_names)) var_names <- paste0("V", seq_len(ncol(mat)))
        data.frame(h = rep(h_vals, times = ncol(mat)), shock = shock,
                   var = rep(var_names, each = nrow(mat)), value = as.vector(mat))
      }))
    }
    p <- list_to_long(x$irf)
    l <- list_to_long(x$Lower)
    u <- list_to_long(x$Upper)
    p$value <- p$value
    l$l <- l$value
    u$u <- u$value
    result <- merge(p[, c("h", "shock", "var", "value")], l[, c("h", "shock", "var", "l")], by = c("h", "shock", "var")) %>%
      merge(u[, c("h", "shock", "var", "u")], by = c("h", "shock", "var"))
  }
  if (inherits(x, "PosteriorIR")) {
    if (is.null(dimnames(x)) & is.null(varlist)) {
      stop("Please provide variable names in varlist")
    }
    names <- if (is.null(dimnames(x))) varlist else dimnames(x)[[1]]
    ll <- x %>%
      summary()
    names(ll) <- names
    ll <- lapply(ll, function(j) {
      names(j) <- names
      return(j)
    })
    norm_names <- function(x) {
      nm <- names(x)
      nm <- gsub("\\s+", " ", trimws(nm))        # collapse whitespace
      nm <- gsub("%\\s*quantile", "% quantile", nm)
      names(x) <- nm
      x
    }
    shocks <- names(ll)
    result <- map_dfr(shocks, function(sh) {
      vars <- names(ll[[sh]])
      map_dfr(vars, function(v) {
        X <- ll[[sh]][[v]]
        X <- as.data.frame(X)
        X <- norm_names(X)
        H <- nrow(X)
        tibble(
          h     = seq(0, by = 1, length.out = H),
          shock = sh,
          var   = v,
          value = X[["mean"]],
          l     = X[["5% quantile"]],
          u     = X[["95% quantile"]]
        )
      })
    })
  }
  return(result)
}
