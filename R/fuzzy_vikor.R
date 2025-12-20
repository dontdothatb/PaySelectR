#' Fuzzy VIKOR Method
#'
#' @description Implements Fuzzy VIKOR with BWM integration - decision model for selecting the optimal online payment system based on expert opinions. Returns an object for plotting.
#' @param decision_mat Matrix ($m \times 3n$). PaymentSystemS (rows) x Fuzzy Criteria (cols).
#' @param criteria_types Character vector length $n$. "max" for benefit, "min" for cost.
#' @param weights (Optional) Numeric vector length $3n$ for fuzzy weights.
#' @param bwm_criteria (Optional) If weights are missing, BWM criteria names.
#' @param bwm_best (Optional) BWM best-to-others vector.
#' @param bwm_worst (Optional) BWM others-to-worst vector.
#' @param v Numeric (0-1). Weight for the strategy of maximum group utility.
#' @return An object of class `fuzzy_vikor_res` containing S, R, and Q indices.
#' @import FuzzyMCDM
#' @export
.compute_payment_weights <- function(decision_mat, weights, bwm_criteria, bwm_best, bwm_worst) {

  n_crit <- ncol(decision_mat) / 3

  if (!missing(weights)) {
    if (length(weights) != ncol(decision_mat)) {
      stop("Length of 'weights' must match the total columns in decision matrix (n * 3).")
    }
    return(weights)
  }

  # Calculate using BWM if weights are missing
  if (!missing(bwm_criteria) && !missing(bwm_best) && !missing(bwm_worst)) {
    message("Weights not provided. Calculating using BWM...")
    bwm_res <- calculate_bwm_weights(bwm_criteria, bwm_best, bwm_worst)

    crisp_w <- bwm_res$criteriaWeights

    if (length(crisp_w) != n_crit) {
      stop("Calculated BWM weights do not match the number of criteria in decision matrix.")
    }

    # Convert crisp BWM weights to Triangular Fuzzy Number (w, w, w)
    fuzzy_weights <- rep(crisp_w, each = 3)
    return(fuzzy_weights)
  }

  stop("You must provide either 'weights' vector OR 'bwm_criteria', 'bwm_best', and 'bwm_worst'.")
}

pay_fuzzy_vikor <- function(decision_mat, criteria_types, weights, bwm_criteria, bwm_best, bwm_worst, v = 0.5) {

  if (!is.matrix(decision_mat)) stop("'decision_mat' must be a matrix.")

  final_w <- .compute_payment_weights(decision_mat, weights, bwm_criteria, bwm_best, bwm_worst)

  n_cols <- ncol(decision_mat)
  fuzzy_cb <- character(n_cols)
  k <- 1
  for (j in seq(1, n_cols, 3)) {
    fuzzy_cb[j:(j+2)] <- criteria_types[k]
    k <- k + 1
  }

  # 1. Ideal Solutions
  pos_ideal <- ifelse(fuzzy_cb == "max", apply(decision_mat, 2, max), apply(decision_mat, 2, min))
  neg_ideal <- ifelse(fuzzy_cb == "min", apply(decision_mat, 2, max), apply(decision_mat, 2, min))

  # 2. Linear Normalization
  d_mat <- matrix(0, nrow = nrow(decision_mat), ncol = n_cols)

  for (i in seq(1, n_cols, 3)) {
    if (fuzzy_cb[i] == "max") {
      denom <- pos_ideal[i+2] - neg_ideal[i]
      if(denom == 0) denom <- 1e-9
      d_mat[, i]   <- (pos_ideal[i]   - decision_mat[, i+2]) / denom
      d_mat[, i+1] <- (pos_ideal[i+1] - decision_mat[, i+1]) / denom
      d_mat[, i+2] <- (pos_ideal[i+2] - decision_mat[, i])   / denom
    } else {
      denom <- neg_ideal[i+2] - pos_ideal[i]
      if(denom == 0) denom <- 1e-9
      d_mat[, i]   <- (decision_mat[, i]   - pos_ideal[i+2]) / denom
      d_mat[, i+1] <- (decision_mat[, i+1] - pos_ideal[i+1]) / denom
      d_mat[, i+2] <- (decision_mat[, i+2] - pos_ideal[i])   / denom
    }
  }

  W_diag <- diag(final_w)
  weighted_d <- d_mat %*% W_diag

  # 3. S and R Values
  utility_index <- matrix(0, nrow(decision_mat), 3)
  risk_index  <- matrix(0, nrow(decision_mat), 3)

  utility_index[,1] <- apply(weighted_d[, seq(1, n_cols, 3), drop=FALSE], 1, sum)
  utility_index[,2] <- apply(weighted_d[, seq(2, n_cols, 3), drop=FALSE], 1, sum)
  utility_index[,3] <- apply(weighted_d[, seq(3, n_cols, 3), drop=FALSE], 1, sum)

  risk_index[,1] <- apply(weighted_d[, seq(1, n_cols, 3), drop=FALSE], 1, max)
  risk_index[,2] <- apply(weighted_d[, seq(2, n_cols, 3), drop=FALSE], 1, max)
  risk_index[,3] <- apply(weighted_d[, seq(3, n_cols, 3), drop=FALSE], 1, max)

  # Defuzzify S and R for Q calculation inputs
  # Note: Q calculation in fuzzy VIKOR is complex.
  # Using the crisp conversion of S and R to find Min/Max for formula.

  # 4. Q Index
  # Q_i = v * (S_i - S*) / (S- - S*) + (1-v) * (R_i - R*) / (R- - R*)

  # We calculate fuzzy Q
  s_star <- min(utility_index[,1])
  s_minus <- max(utility_index[,3])
  r_star <- min(risk_index[,1])
  r_minus <- max(risk_index[,3])

  denom_s <- s_minus - s_star
  denom_r <- r_minus - r_star

  if (denom_s == 0) denom_s <- 1
  if (denom_r == 0) denom_r <- 1

  compromise_index <- matrix(0, nrow(decision_mat), 3)

  # Q1 part (based on S)
  term1 <- (utility_index - s_star) / denom_s
  # Q2 part (based on R)
  term2 <- (risk_index - r_star) / denom_r

  compromise_index <- v * term1 + (1 - v) * term2

  # Defuzzification
  def_utility <- (utility_index[,1] + 2*utility_index[,2] + utility_index[,3]) / 4
  def_risk <- (risk_index[,1] + 2*risk_index[,2] + risk_index[,3]) / 4
  def_compromise <- (compromise_index[,1] + 2*compromise_index[,2] + compromise_index[,3]) / 4

  result_df <- data.frame(
    PaymentSystem = 1:nrow(decision_mat),
    UtilityScore = def_utility,
    RiskScore = def_risk,
    FinalScore = def_compromise,
    Ranking = rank(def_compromise, ties.method = "first")
  )

  output <- list(
    results = result_df,
    details = list(utility_index = utility_index, risk_index = risk_index, compromise_index = compromise_index),
    params = list(v = v)
  )

  class(output) <- "fuzzy_vikor_res"
  return(output)
}
