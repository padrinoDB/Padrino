## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## utlity functions -- miscellaneous
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

# 
# inverse logit
# 
inv_logit <- function(x) {
  1.0/(1.0+exp(-x))
}

# 
# build an empty list in array form
# 
mk_list_array <- function(states) {
  dims <- sapply(states, length)
  data <- vector(mode = "list", length = prod(dims))
  array(data, dims, states)
}


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## utlity functions
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

numeric_midpoint <- function(x) {
  weights <- (x[2] - x[1]) / x[3]
  nodes <- seq(x[1] + weights/2, x[2] - weights/2, length.out = x[3])
  rule <- list(nodes, weights, x[3])
  names(rule) <- c("val", "wgt", "num")
  attr(rule, "state") <- "numeric"
  attr(rule, "rule") <- "midpoint"
  rule
}

numeric_integer <- function(x) {
  rule <- list(seq.int(x[1], x[2]), 1, x[2]-x[1]+1)
  names(rule) <- c("val", "wgt", "num")
  attr(rule, "state") <- "numeric"
  attr(rule, "rule") <- "integer"
  rule
}

categorical_integer <- function(x) {
  rule <- list(seq.int(x[1], x[2]), 1, x[2]-x[1]+1)
  names(rule) <- c("val", "wgt", "num")
  attr(rule, "state") <- "categorical"
  attr(rule, "rule") <- "integer"
  rule
}

categorical_nominal <- function(x) {
  rule <- list(x, 1, length(x))
  names(rule) <- c("val", "wgt", "num")
  attr(rule, "state") <- "categorical"
  attr(rule, "rule") <- "nominal"
  rule
}

which_list_part <- function (states) {
  type_info <- sapply(states, function(x) attributes(x)$state)
  type_info == "categorical"
}

new_n_t <- function(...) {
  #
  dots <- list(...) 
  # work out which states are in list form
  is_list_part <- which_list_part(dots)
  is_list_part
  # build the list part of the state vector
  list_part <- dots[is_list_part]
  n_t <- mk_list_array(lapply(list_part, '[[', "val"))
  attrib_n_t <- attributes(n_t)
  # insert the numeric state vector
  vect_part <- dots[!is_list_part]
  size <- prod(sapply(vect_part, '[[', "num"))
  zeros <- vector(mode = "double", size)
  n_t <- lapply(n_t, function(dummy) zeros)
  # make sure the numeric states are 1st in the attributes
  ord <- c(which(!is_list_part), which(is_list_part))
  dots <- dots[ord]
  # 
  state_names <- list(state_names = names(dots))
  #
  state_rules <- sapply(dots, function(x) attributes(x)[c("state","rule")])
  state_rules <- list(state_rules = state_rules)
  # 
  details <- sapply(dots, '[')
  details <- list(details = details)
  #
  attributes(n_t) <- c(attrib_n_t, state_names, state_rules, details)
  n_t
}

marginal_density <- function(n_t, margin = "x") {
  #
  attrib_n_t <- attributes(n_t)
  #
  n_t <- unlist(n_t)
  dim(n_t) <- unlist(attrib_n_t$details["num",])
  m <- which(attrib_n_t$state_names %in% margin)
  n_t <- apply(n_t, m, sum)
  # FIXME
  dlta <- unlist(attrib_n_t$details["wgt",]) # <- only works if one weight
  if (!("x" %in% margin)) n_t <- n_t * dlta["x"]
  n_t
}

# 
# function to extract information about the kernel impied 
# by the supplied state vectors 
# 
kernel_info <- function(state, what = "val", ...) {
  
  dots <- list(...)
  n <- length(dots)
  values <- list()
  suffix <- ""
  
  for (i in seq_along(dots)) {
    attribs <- attributes(dots[[n-i+1]])
    is_type <- attribs$state_rules["state",] == state
    keep <- attribs$details[what, is_type]
    names(keep) <- paste0(attribs$state_names[is_type], suffix)
    values[[n-i+1]] <- keep
    suffix <- paste0(suffix ,"_")
  }
  
  do.call(c, values)
}

# 
# simple version of mapply that works with an expression
# 
mapply_expr <- function(expr, variable_args, fixed_args = list()) {
  #
  mapply_args <- list(
    FUN = function(...) eval(expr, envir = list(...)),
    SIMPLIFY = FALSE, MoreArgs = fixed_args
  ) %>% c(variable_args)
  # 
  do.call(mapply, mapply_args)
}

# 
# calculate expectation required for equivalent density
# 
build_expect_f <- function(x) {
  dx <- x[2]-x[1]
  function(n, theta) sum(n * x^theta) * dx
}

# 
# functional returning a function to compute category-specific ipm function 
# careful -- must include 'a' and 'a_' in the `cat_trans`
# 
ipm_fun <- function(mod_par, f_fix, f_env, f_dmg, cat_trans, ...) {
  #
  dots <- list(n2, n1)
  # extract the information we require from the state atrtributes
  cat_states <- do.call(kernel_info, c("categorical", what = "val", dots))
  num_states <- do.call(kernel_info, c("numeric",     what = "val", dots))
  array_dims <- do.call(kernel_info, c("numeric",     what = "num", dots))
  # list containing objects that is invariant across function evaluations
  fixed_args <- c(do.call(expand.grid, num_states), mod_par)
  # list containing objects that vary across 
  variable_args <- cat_trans
  # FIXME -- hack to make the numeric age states a numeric vector
  variable_args$a_ <- as.numeric(cat_trans$a_)
  variable_args$a  <- as.numeric(cat_trans$a )
  # FIXME -- function to calculate the expectation wrt to density function
  expect_f <- build_expect_f(num_states$x)
  # list-array to store the function evaluations
  eval_func <- mk_list_array(cat_states)
  # subset function for the above list-array
  subset <- function(...) {
    do.call(`[[`, c(x = quote(eval_func), list(...)))
  }
  # matrix to index into list of evaluated functions 
  index <- as.matrix(cat_trans)
  # evaluate the time invariant part of the additive model
  f_fix_eval <- mapply_expr(f_fix[[2]], variable_args, fixed_args)
  # 
  function(...) {
    # 
    f_args <- c(list(...), fixed_args, expect_f = expect_f)
    .additive <- 
      mapply_expr(f_env[[2]], list(0), f_args) %>% 
      mapply(`+`, f_fix_eval, ., SIMPLIFY = FALSE) 
    #
    v_args <- c(.additive = list(.additive), variable_args)
    eval_func[index] <<- 
      mapply_expr(f_dmg[[2]], v_args, fixed_args) %>%
      lapply(function(x) {
        dim(x) <- array_dims
        x
      })
    # FIXME -- the above `add dim` stpe is not very efficient
    return(subset)
  }
}

# 
# functional to compute category-specific ipm function 
# careful -- must include 'a' and 'a_' in the `cat_trans`
# 
ipm_kern <- function(kern, funs, cat_trans, ...) {
  # evaluate set of demographic functions
  fixed_args <- lapply(funs, do.call, list(...))
  # evaluate kernel over categorical states
  mapply_expr(kern[[2]], cat_trans, fixed_args)  
}

# # FIXME -- SHOULD BE POSSIBLE TO AUTOMATICALLY ITERATE A MODEL ...
# iter_kern <- function(kern, funs, cat_trans) {
#   # 
#   force(funs)
#   #
#   trans_from <- as.character(cat_trans$a)
#   to_states <- as.character(unique(cat_trans$a_))
#   trans_to <- factor(cat_trans$a_, levels = to_states)
#   # 
#   iter_expr <- as.call(list(quote(`%*%`), kern[[2]], quote(n_t)))
#   #
#   mapply_args <- list(
#     FUN = function(...) eval(iter_expr, envir = list(...)),
#     SIMPLIFY = FALSE, MoreArgs = NA
#   ) %>% c(cat_trans, n_t = NA)
#   # 
#   function(.nt, ...) {
#     # add the evaluated demographic functions to the list of mapply arguments
#     mapply_args$MoreArgs <<- lapply(funs, do.call, list(...))
#     # pad out the state list so that it matches number of transitions + add
#     mapply_args$n_t <<- .nt[trans_from]
#     # 
#     do.call(mapply, mapply_args) %>% 
#       split(trans_to) %>%
#       lapply(function(x) Reduce(`+`, x))
#   }
# }
