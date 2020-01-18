boott <- function(x, cluster = NULL, R = 250, type = "xy", ..., fix = FALSE, impose.null = NULL, use = "pairwise.complete.obs", applyfun = NULL, cores = NULL)
{
  ## set up return value with correct dimension and names
  cf <- coef(x)
  k <- length(cf)
  n <- nobs(x)
  f <- formula(x$terms)
  dv <- all.vars(update(f, . ~ 1))
  b <- coef(x)[names(coef(x)) %in% impose.null]
  vc <- sandwich::vcovCL(x, cluster = cluster, type = "HC1")
  w <- b / sqrt(diag(vc)[names(diag(vc)) %in% impose.null])

  rval <- matrix(0, nrow = k, ncol = k, dimnames = list(names(cf), names(cf)))
  cf <- matrix(rep.int(NA_real_, k * R), ncol = k, dimnames = list(NULL, names(cf)))
  
  ## cluster can either be supplied explicitly or
  ## be an attribute of the model...FIXME: other specifications?
  if (is.null(cluster)) cluster <- attr(x, "cluster")
  
  ## resort to cross-section if no clusters are supplied
  if (is.null(cluster)) cluster <- 1L:n
  
  ## collect 'cluster' variables in a data frame
  if(inherits(cluster, "formula")) {
    cluster_tmp <- expand.model.frame(x, cluster, na.expand = FALSE)
    cluster <- model.frame(cluster, cluster_tmp, na.action = na.pass)
  } else {
    cluster <- as.data.frame(cluster)
  }
  
  ## handle omitted or excluded observations
  if((n != NROW(cluster)) && !is.null(x$na.action) && (class(x$na.action) %in% c("exclude", "omit"))) {
    cluster <- cluster[-x$na.action, , drop = FALSE]
  }
  
  if(NROW(cluster) != n) stop("number of observations in 'cluster' and 'nobs()' do not match")
  
  ## for multi-way clustering: set up interaction patterns
  p <- NCOL(cluster)
  if (p > 1L) {
    cl <- lapply(1L:p, function(i) combn(1L:p, i, simplify = FALSE))
    cl <- unlist(cl, recursive = FALSE)
    sign <- sapply(cl, function(i) (-1L)^(length(i) + 1L))    
    paste_ <- function(...) paste(..., sep = "_")
    for (i in (p + 1L):length(cl)) {
      cluster <- cbind(cluster, Reduce(paste_, unclass(cluster[, cl[[i]] ]))) ## faster than: interaction()
    }
  } else {
    cl <- list(1)
    sign <- 1
  }
  
  ## bootstrap type: xy vs. residual vs. wild (in various flavors)
  if(is.function(type)) {
    wild <- type
    type <- "user"
  } else {
    wild <- function(n) NULL
  }
  type <- match.arg(gsub("wild-", "", tolower(type), fixed = TRUE),
                    c("xy", "residual", "wild", "webb", "rademacher", "mammen", "norm", "user"))
  if(type == "wild") type <- "rademacher"
  
  ## set up wild bootstrap function
  wild <- switch(type,
                 "rademacher" = function(n) sample(c(-1, 1), n, replace = TRUE),
                 "mammen"     = function(n) sample(c(-1, 1) * (sqrt(5) + c(-1, 1))/2, n, replace = TRUE,
                                                   prob = (sqrt(5) + c(1, -1))/(2 * sqrt(5))),
                 "norm"       = function(n) rnorm(n),
                 "webb"       = function(n) sample(c(-sqrt((3:1)/2), sqrt((1:3)/2)), n, replace = TRUE),
                 wild
  )
  
  ## model information: original response and design matrix or the corresponding fitted/residuals/QR
  y <- if(!is.null(x$y)) {
    x$y
  } else if(!is.null(x$model)) {
    model.response(x$model)
  } else {
    model.response(model.frame(x))
  }
  
  xfit <- model.matrix(x)
  
  b0 <- coef(x)
  b0[!names(b0) %in% impose.null] <-
    lm.fit(as.matrix(xfit[,!colnames(xfit) %in% impose.null]), y, ...)$coefficients
  b0[names(b0) %in% impose.null] <- 0
  
  res <- if(!is.null(impose.null)) {
    y - (model.matrix(x) %*% b0)
  } else {
    x$residuals
  }

  yhat <- if(!is.null(impose.null)) {
    model.matrix(x) %*% b0
  } else {
    x$fitted.values
  }

  df <- data.frame(y, yhat, res)
  df <- cbind(df, xfit)

  ## apply infrastructure for refitting models
  if(is.null(applyfun)) {
    applyfun <- if(is.null(cores)) {
      lapply
    } else {
      if(.Platform$OS.type == "windows") {
        cl_cores <- parallel::makeCluster(cores)
        on.exit(parallel::stopCluster(cl_cores))
        function(X, FUN, ...) parallel::parLapply(cl = cl_cores, X, FUN, ...)
      } else {
        function(X, FUN, ...) parallel::mclapply(X, FUN, ..., mc.cores = cores)
      }
    }
  }
  cat("   Outcome:", dv, "\n")
  cat("   Bootstrapping studentized estimates for", impose.null, "with null imposed...\n")
  pb <- txtProgressBar(min = 0, max = R, style = 3)

  ## bootstrap for each cluster dimension
  for (i in 1L:length(cl))
  {
    ## cluster structure
    cli <- if(type %in% c("xy", "residual")) {
      split(seq_along(cluster[[i]]), cluster[[i]])
    } else {
      factor(cluster[[i]], levels = unique(cluster[[i]]))
    }

    ## check for balanced clusters in residual bootstrap
    if(type == "residual" && length(unique(sapply(cli, length))) > 1L) {
      warning("residual bootstrap is not well-defined for unbalanced clusters")
    }

    ## bootstrap fitting function
    bootfit <- switch(type,
                      "xy" = function(j, ...) {
                        setTxtProgressBar(pb, j)
                        smp <- cli[sample(names(cli), length(cli), replace = TRUE)]
                        cl <- unlist(mapply(function (x, y) rep(x, length(smp[[y]])), x = 1:length(smp), y = names(smp)))
                        j <- unlist(smp)
                        b.fit <- lm(update(f, y ~ .), data = df[j, , drop = FALSE], ...)
                        b.vcov <- sandwich::vcovCL(b.fit, cluster = cl, type = "HC1")
                        b.star <- coef(b.fit)[names(coef(b.fit)) %in% impose.null]
                        se.star <- sqrt(diag(b.vcov)[names(diag(b.vcov)) %in% impose.null])
                        (b.star) / se.star
                      },
                      "residual" = function(j, ...) {
                        setTxtProgressBar(pb, j)
                        smp <- cli[sample(names(cli), length(cli), replace = TRUE)]
                        cl <- unlist(mapply(function (x, y) rep(x, length(smp[[y]])), x = 1:length(smp), y = names(smp)))
                        j <- unlist(smp)                        
                        b.fit <- lm.fit(xfit$x, xfit$fit + xfit$res[j], ...)
                        b.vcov <- sandwich::vcovCL(b.fit, cluster = cl, type = "HC1")
                        b.star <- coef(b.fit)[names(coef(b.fit)) %in% impose.null]
                        se.star <- sqrt(diag(b.vcov)[names(diag(b.vcov)) %in% impose.null])
                        (b.star) / se.star
                      },
                      function(j, ...) {
                        setTxtProgressBar(pb, j)
                        j <- wild(nlevels(cli))
                        df$y.star <- df$yhat + df$res * j[cli]
                        b.fit <- lm(update(f, y.star ~ .), data = df, ...)
                        b.vcov <- sandwich::vcovCL(b.fit, cluster = cli, type = "HC1")
                        b.star <- coef(b.fit)[names(coef(b.fit)) %in% impose.null]
                        se.star <- sqrt(diag(b.vcov)[names(diag(b.vcov)) %in% impose.null])
                        (b.star) / se.star
                      }
    )

    ## actually refit
    bw <- applyfun(1L:R, bootfit, ...)
    bw <- do.call("rbind", bw)
    
    cat("\n")
    close(pb)
    
    
  }

  boot_obj <- list(
    fit = x,
    boot.w = w,
    boot.dist = bw,
    boot.type = type,
    boot.ci = NULL,
    boot.p = 1 - mean(abs(w) > abs(bw))
  )
  
  return(boot_obj)
 
}

