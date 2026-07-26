# =============================================================================
# File: R/KappaAnalyzer.R
# Description: Kappa and attribute agreement analyzer.
# =============================================================================

#' @title KappaAnalyzer
#' @description Calculates Cohen/Fleiss kappa and Minitab-style agreement tables.
#'
#' @param data Data frame of attribute ratings.
#' @param plan Optional plan object or list of parameters.
#' @param kappa Numeric scalar kappa value to interpret.
#'
#' @export
KappaAnalyzer <- R6::R6Class(
  "KappaAnalyzer",
  inherit = IqrAnalyzerBase,
  public = list(
    initialize = function() {
      super$initialize()
    },

    run = function(data, plan = NULL) {
      self$reset()
      if (!is.null(plan)) {
        self$setup(plan)
        if (is.list(plan) && !is.null(plan$meta_data$data)) {
          self$params <- modifyList(self$params, plan$meta_data$data)
        }
      }

      mode <- self$params$comparison_mode %||% "one_way"
      method <- self$params$kappa_method %||% "fleiss"
      results <- switch(mode,
        one_way = self$one_way_analysis(data),
        two_way = self$two_way_wagner(data),
        stop("Unknown comparison mode: ", mode, call. = FALSE)
      )

      self$set_raw_output(results)
      invisible(self)
    },

    one_way_analysis = function(data) {
      method <- self$params$kappa_method %||% "fleiss"
      switch(method,
        cohen = self$cohen_kappa(data),
        fleiss = self$fleiss_kappa(data),
        wagner = self$two_way_wagner(data),
        stop("Unknown kappa method: ", method, call. = FALSE)
      )
    },

    cohen_kappa = function(data) {
      eval1_col <- self$params$eval1_col %||% "Evaluator1"
      eval2_col <- self$params$eval2_col %||% "Evaluator2"
      if (!eval1_col %in% names(data) || !eval2_col %in% names(data)) {
        stop("Data must contain evaluator columns: ", eval1_col, ", ", eval2_col, call. = FALSE)
      }

      d <- data.frame(
        rater1 = data[[eval1_col]],
        rater2 = data[[eval2_col]]
      )
      d <- d[stats::complete.cases(d), , drop = FALSE]
      k <- private$cohen_stats(d$rater1, d$rater2, label = paste(eval1_col, "vs", eval2_col))
      k$method <- "Cohen's Kappa"
      k$data <- d
      k$rating_matrix <- as.matrix(table(d$rater1, d$rater2))
      k$confusion_matrix_table <- private$confusion_table(d$rater1, d$rater2, "Rater 1", "Rater 2")
      k$response_table <- private$response_agreement(d$rater1, d$rater2)

      self$set_statistic("cohen_kappa", k$kappa)
      self$set_statistic("observed_agreement", k$Po)
      self$set_statistic("expected_agreement", k$Pe)
      self$set_statistic("z_value", k$z)
      self$set_statistic("p_value", k$p_value)
      k
    },

    fleiss_kappa = function(data) {
      sample_col <- self$params$sample_col %||% "Sample"
      rater_col <- self$params$rater_col %||% "Appraiser"
      rating_col <- self$params$rating_col %||% "Rating"
      standard_col <- self$params$standard_col %||% private$infer_standard_col(data)
      trial_col <- self$params$trial_col %||% private$infer_trial_col(data)

      if (!all(c(sample_col, rater_col, rating_col) %in% names(data))) {
        stop("Data must contain columns: ", sample_col, ", ", rater_col, ", ", rating_col, call. = FALSE)
      }

      d <- data.frame(
        sample = data[[sample_col]],
        appraiser = data[[rater_col]],
        rating = data[[rating_col]],
        stringsAsFactors = FALSE
      )
      if (!is.null(standard_col) && standard_col %in% names(data)) d$standard <- data[[standard_col]]
      if (!is.null(trial_col) && trial_col %in% names(data)) d$trial <- data[[trial_col]]
      d <- d[stats::complete.cases(d[, c("sample", "appraiser", "rating"), drop = FALSE]), , drop = FALSE]

      samples <- unique(d$sample)
      appraisers <- unique(d$appraiser)
      scale_type <- self$params$scale_type %||% self$params$attribute_scale %||% "nominal"
      category_order <- self$params$category_order %||% self$params$categories %||% NULL
      if (is.null(category_order) && is.factor(d$rating) && is.ordered(d$rating)) {
        category_order <- levels(d$rating)
        scale_type <- "ordinal"
      }
      if (!is.null(category_order)) {
        category_order <- as.character(category_order)
        d$rating <- factor(as.character(d$rating), levels = category_order, ordered = identical(scale_type, "ordinal"))
        if ("standard" %in% names(d)) {
          d$standard <- factor(as.character(d$standard), levels = category_order, ordered = identical(scale_type, "ordinal"))
        }
      }
      categories <- category_order %||% unique(as.character(d$rating))
      rating_matrix <- private$rating_count_matrix(d, samples, categories)

      n_subj <- length(samples)
      n_ij <- rowSums(rating_matrix)
      n_raters <- if (length(unique(n_ij)) == 1) unique(n_ij) else NA_integer_
      if (any(n_ij < 2)) {
        stop("Each sample must have at least two ratings for Fleiss kappa.", call. = FALSE)
      }

      overall_stats <- private$fleiss_stats_from_matrix(rating_matrix)
      P_o <- overall_stats$Po
      P_e <- overall_stats$Pe
      kappa <- overall_stats$kappa
      se <- overall_stats$se
      z <- overall_stats$z
      p_value <- overall_stats$p_value
      ci <- overall_stats$ci

      pairwise <- private$pairwise_appraisers(d)
      vs_standard <- if ("standard" %in% names(d)) private$appraiser_vs_standard(d) else data.frame()
      response_tbl <- private$response_summary_long(d)
      response_kappa <- private$response_kappa(d, categories)
      response_kappa_vs_standard <- if ("standard" %in% names(d)) {
        private$response_kappa_vs_standard(d, categories)
      } else {
        data.frame()
      }
      within <- if ("trial" %in% names(d)) private$within_appraiser(d) else data.frame()
      sample_tbl <- private$sample_disagreement(d)
      ordinal <- if (identical(scale_type, "ordinal")) {
        private$ordinal_agreement(d, category_order = categories)
      } else {
        NULL
      }

      result <- list(
        method = "Fleiss' Kappa",
        scale_type = scale_type,
        category_order = categories,
        kappa = kappa,
        Po = P_o,
        Pe = P_e,
        n_subjects = n_subj,
        n_raters = n_raters,
        n_appraisers = length(appraisers),
        n_categories = length(categories),
        se = se,
        z = z,
        p_value = p_value,
        ci = ci,
        interpretation = self$interpret_kappa(kappa),
        rating_matrix = rating_matrix,
        pairwise_appraisers = pairwise,
        appraiser_vs_standard = vs_standard,
        response_table = response_tbl,
        response_kappa = response_kappa,
        response_kappa_vs_standard = response_kappa_vs_standard,
        within_appraiser = within,
        sample_disagreement = sample_tbl,
        ordinal = ordinal,
        data = d
      )

      self$set_statistic("fleiss_kappa", kappa)
      self$set_statistic("observed_agreement", P_o)
      self$set_statistic("expected_agreement", P_e)
      self$set_statistic("z_value", z)
      self$set_statistic("p_value", p_value)
      if (!is.null(ordinal$between_appraisers) && nrow(ordinal$between_appraisers) > 0) {
        self$set_statistic("kendall_w", ordinal$between_appraisers$Coef[[1]])
      }
      if (!is.null(ordinal$all_appraisers_vs_standard) && nrow(ordinal$all_appraisers_vs_standard) > 0) {
        self$set_statistic("kendall_tau_standard", ordinal$all_appraisers_vs_standard$Coef[[1]])
      }
      result
    },

    two_way_wagner = function(data) {
      eval1_col <- self$params$eval1_col %||% "Evaluator1"
      eval2_col <- self$params$eval2_col %||% "Evaluator2"
      if (!eval1_col %in% names(data) || !eval2_col %in% names(data)) {
        stop("Data must contain evaluator columns: ", eval1_col, ", ", eval2_col, call. = FALSE)
      }
      d <- data.frame(eval1 = data[[eval1_col]], eval2 = data[[eval2_col]])
      d <- d[stats::complete.cases(d), , drop = FALSE]
      n <- nrow(d)
      concordant <- sum(d$eval1 == d$eval2)
      V <- concordant / n
      se <- sqrt(V * (1 - V) / n)
      z <- ifelse(se == 0, NA_real_, V / se)
      result <- list(
        method = "Wagner Method",
        V = V,
        BP = { # Bias Proportion: unify factor levels before numeric conversion
          combined <- factor(c(as.character(d$eval1), as.character(d$eval2)))
          e1_num <- as.numeric(factor(d$eval1, levels = levels(combined)))
          e2_num <- as.numeric(factor(d$eval2, levels = levels(combined)))
          mean(abs(e1_num - e2_num), na.rm = TRUE)
        },
        n = n,
        concordant = concordant,
        se_V = se,
        z = z,
        p_value = ifelse(is.na(z), NA_real_, stats::pnorm(z, lower.tail = FALSE)),
        ci_V = private$.kappa_ci(V, se, self$params$conf_level %||% 0.95),
        grade = if (V >= 0.75) "Acceptable" else if (V >= 0.5) "Conditionally Acceptable" else "Unacceptable",
        data = d
      )
      self$set_statistic("wagner_V", V)
      self$set_statistic("wagner_BP", result$BP)
      result
    },

    interpret_kappa = function(kappa) {
      if (is.na(kappa)) return("Not interpretable")
      if (kappa < 0) return("Poor")
      if (kappa < 0.20) return("Slight")
      if (kappa < 0.40) return("Fair")
      if (kappa < 0.60) return("Moderate")
      if (kappa < 0.75) return("Good")
      if (kappa < 0.90) return("Strong")
      "Excellent"
    }
  ),

  private = list(
    .kappa_ci = function(kappa, se, conf_level) {
      kappa + c(-1, 1) * stats::qnorm(1 - (1 - conf_level) / 2) * se
    },

    infer_standard_col = function(data) {
      candidates <- c("Standard", "standard", "Reference", "Truth")
      candidates[candidates %in% names(data)][1] %||% NULL
    },

    infer_trial_col = function(data) {
      candidates <- c("Trial", "trial", "Trail", "trail", "Replicate", "Replication")
      candidates[candidates %in% names(data)][1] %||% NULL
    },

    fleiss_stats_from_matrix = function(rating_matrix, label = "Fleiss' Kappa") {
      x <- as.matrix(rating_matrix)
      storage.mode(x) <- "numeric"
      x <- x[rowSums(x) > 1, , drop = FALSE]
      n <- nrow(x)
      row_n <- rowSums(x)
      if (n == 0 || any(row_n < 2)) {
        return(list(
          Comparison = label, n = n, raters = NA_integer_, Po = NA_real_, Pe = NA_real_,
          kappa = NA_real_, se = NA_real_, z = NA_real_, p_value = NA_real_,
          ci = c(NA_real_, NA_real_)
        ))
      }
      raters <- if (length(unique(row_n)) == 1) unique(row_n) else NA_real_
      P_i <- rowSums(x^2) - row_n
      P_i <- P_i / (row_n * (row_n - 1))
      Po <- mean(P_i)
      p_j <- colSums(x) / sum(x)
      Pe <- sum(p_j^2)
      kappa <- if (Pe < 1) (Po - Pe) / (1 - Pe) else 1

      se <- NA_real_
      if (!is.na(raters) && raters > 1) {
        q_j <- 1 - p_j
        denom <- sum(p_j * q_j)^2 * (n * raters * (raters - 1))
        numer <- sum(p_j * q_j)^2 - sum(p_j * q_j * (q_j - p_j))
        var_kappa <- if (denom > 0) 2 * numer / denom else NA_real_
        se <- if (!is.na(var_kappa) && var_kappa >= 0) sqrt(var_kappa) else NA_real_
      }
      z <- ifelse(is.na(se) || se == 0, NA_real_, kappa / se)
      p_value <- ifelse(is.na(z), NA_real_, stats::pnorm(z, lower.tail = FALSE))
      ci <- private$.kappa_ci(kappa, se, self$params$conf_level %||% 0.95)
      list(
        Comparison = label,
        n = n,
        raters = raters,
        Po = Po,
        Pe = Pe,
        kappa = kappa,
        se = se,
        z = z,
        p_value = p_value,
        ci = ci
      )
    },

    fleiss_stats_from_long = function(d, categories, label = "Fleiss' Kappa") {
      samples <- unique(d$sample)
      mat <- private$rating_count_matrix(d, samples, categories)
      private$fleiss_stats_from_matrix(mat, label = label)
    },

    binary_response_long = function(d, response) {
      out <- d
      out$rating <- ifelse(as.character(out$rating) == as.character(response), as.character(response), "Other")
      if ("standard" %in% names(out)) {
        out$standard <- ifelse(as.character(out$standard) == as.character(response), as.character(response), "Other")
      }
      out
    },

    cohen_stats = function(x, y, label = "Agreement") {
      d <- data.frame(x = x, y = y)
      d <- d[stats::complete.cases(d), , drop = FALSE]
      cats <- unique(c(as.character(d$x), as.character(d$y)))
      n <- nrow(d)
      Po <- mean(d$x == d$y)
      Pe <- sum(vapply(cats, function(cat) mean(d$x == cat) * mean(d$y == cat), numeric(1)))
      kappa <- if (Pe < 1) (Po - Pe) / (1 - Pe) else 1
      se <- sqrt(max(Po * (1 - Po), 0) / (n * (1 - Pe)^2))
      z <- ifelse(is.na(se) || se == 0, NA_real_, kappa / se)
      p <- ifelse(is.na(z), NA_real_, stats::pnorm(z, lower.tail = FALSE))
      list(
        Comparison = label,
        n = n,
        Po = Po,
        Pe = Pe,
        kappa = kappa,
        se = se,
        z = z,
        p_value = p,
        ci = private$.kappa_ci(kappa, se, self$params$conf_level %||% 0.95),
        interpretation = self$interpret_kappa(kappa)
      )
    },

    two_rater_fleiss_stats = function(x, y, label = "Agreement") {
      d <- data.frame(
        sample = seq_along(x),
        r1 = x,
        r2 = y,
        stringsAsFactors = FALSE
      )
      d <- d[stats::complete.cases(d[, c("r1", "r2"), drop = FALSE]), , drop = FALSE]
      cats <- unique(c(as.character(d$r1), as.character(d$r2)))
      mat <- t(vapply(seq_len(nrow(d)), function(i) {
        as.numeric(table(factor(c(d$r1[i], d$r2[i]), levels = cats)))
      }, numeric(length(cats))))
      colnames(mat) <- cats
      st <- private$fleiss_stats_from_matrix(mat, label = label)
      st$n <- nrow(d)
      st
    },

    confusion_table = function(x, y, x_name, y_name) {
      tab <- as.data.frame.matrix(table(x, y))
      tab <- cbind(Response = rownames(tab), tab)
      rownames(tab) <- NULL
      names(tab)[1] <- x_name
      attr(tab, "y_name") <- y_name
      tab
    },

    response_agreement = function(x, y) {
      responses <- unique(c(as.character(x), as.character(y)))
      do.call(rbind, lapply(responses, function(resp) {
        idx <- x == resp | y == resp
        data.frame(
          Response = resp,
          N = sum(idx, na.rm = TRUE),
          Matches = sum(x[idx] == y[idx], na.rm = TRUE),
          Percent_Agreement = ifelse(sum(idx, na.rm = TRUE) > 0, 100 * mean(x[idx] == y[idx], na.rm = TRUE), NA_real_)
        )
      }))
    },

    rating_count_matrix = function(d, samples, categories) {
      mat <- matrix(0, nrow = length(samples), ncol = length(categories), dimnames = list(samples, categories))
      for (s in samples) {
        vals <- d$rating[d$sample == s]
        counts <- table(factor(vals, levels = categories))
        mat[as.character(s), ] <- as.numeric(counts)
      }
      mat
    },

    ordinal_scores = function(x, category_order) {
      if (is.null(category_order)) {
        if (is.factor(x)) category_order <- levels(x) else category_order <- sort(unique(as.character(x)))
      }
      as.numeric(factor(as.character(x), levels = as.character(category_order), ordered = TRUE))
    },

    kendall_w = function(score_matrix, label = "Kendall's W") {
      score_matrix <- as.matrix(score_matrix)
      keep <- stats::complete.cases(t(score_matrix))
      score_matrix <- score_matrix[, keep, drop = FALSE]
      k <- nrow(score_matrix)
      n <- ncol(score_matrix)
      if (k < 2 || n < 2) {
        return(data.frame(Comparison = label, N = n, Raters = k, Coef = NA_real_, Chi_Sq = NA_real_, DF = NA_integer_, P = NA_real_))
      }
      ranks <- t(apply(score_matrix, 1, rank, ties.method = "average"))
      rank_sums <- colSums(ranks)
      s <- sum((rank_sums - mean(rank_sums))^2)
      tie_sum <- sum(apply(score_matrix, 1, function(row) {
        tab <- table(row)
        sum(as.numeric(tab)^3 - as.numeric(tab))
      }))
      denom <- k^2 * (n^3 - n) - k * tie_sum
      w <- if (denom > 0) 12 * s / denom else NA_real_
      chi_sq <- if (!is.na(w)) k * (n - 1) * w else NA_real_
      df <- n - 1
      p <- if (!is.na(chi_sq)) stats::pchisq(chi_sq, df = df, lower.tail = FALSE) else NA_real_
      data.frame(Comparison = label, N = n, Raters = k, Coef = w, Chi_Sq = chi_sq, DF = df, P = p)
    },

    kendall_tau = function(x, y, label = "Kendall tau-b") {
      d <- data.frame(x = x, y = y)
      d <- d[stats::complete.cases(d), , drop = FALSE]
      n <- nrow(d)
      if (n < 3 || length(unique(d$x)) < 2 || length(unique(d$y)) < 2) {
        return(data.frame(Comparison = label, N = n, Coef = NA_real_, SE_Coef = NA_real_, Z = NA_real_, P = NA_real_))
      }
      coef <- suppressWarnings(stats::cor(d$x, d$y, method = "kendall"))
      test <- suppressWarnings(tryCatch(stats::cor.test(d$x, d$y, method = "kendall", exact = FALSE), error = function(e) NULL))
      p <- if (!is.null(test)) test$p.value else NA_real_
      z <- if (!is.null(test) && !is.null(test$statistic)) as.numeric(test$statistic) else NA_real_
      se <- if (!is.na(z) && z != 0) abs(coef / z) else sqrt(2 * (2 * n + 5) / (9 * n * (n - 1)))
      data.frame(Comparison = label, N = n, Coef = coef, SE_Coef = se, Z = z, P = p)
    },

    ordinal_agreement = function(d, category_order) {
      d$score <- private$ordinal_scores(d$rating, category_order)
      if ("standard" %in% names(d)) {
        d$standard_score <- private$ordinal_scores(d$standard, category_order)
      }

      between <- private$kendall_w(private$wide_score_matrix(d, c("appraiser", "trial")), "Between Appraisers")

      within <- data.frame()
      if ("trial" %in% names(d)) {
        rows <- lapply(unique(d$appraiser), function(app) {
          ad <- d[d$appraiser == app, , drop = FALSE]
          private$kendall_w(private$wide_score_matrix(ad, "trial"), as.character(app))
        })
        within <- do.call(rbind, rows)
        names(within)[names(within) == "Comparison"] <- "Appraiser"
      }

      vs_standard <- data.frame()
      all_standard <- data.frame()
      if ("standard_score" %in% names(d)) {
        rows <- lapply(unique(d$appraiser), function(app) {
          ad <- d[d$appraiser == app, , drop = FALSE]
          if ("trial" %in% names(ad)) {
            trials <- unique(ad$trial)
            trial_rows <- lapply(trials, function(tr) {
              td <- ad[ad$trial == tr, , drop = FALSE]
              private$kendall_tau(td$score, td$standard_score, paste(app, "Trial", tr, "vs Standard"))
            })
            trial_tbl <- do.call(rbind, trial_rows)
            coef <- mean(trial_tbl$Coef, na.rm = TRUE)
            se <- stats::sd(trial_tbl$Coef, na.rm = TRUE) / sqrt(sum(!is.na(trial_tbl$Coef)))
            if (is.nan(se)) se <- NA_real_
            z <- ifelse(is.na(se) || se == 0, NA_real_, coef / se)
            data.frame(
              Comparison = paste(app, "vs Standard"),
              N = length(unique(ad$sample)),
              Coef = coef,
              SE_Coef = se,
              Z = z,
              P = ifelse(is.na(z), NA_real_, 2 * stats::pnorm(abs(z), lower.tail = FALSE))
            )
          } else {
            private$kendall_tau(ad$score, ad$standard_score, paste(app, "vs Standard"))
          }
        })
        vs_standard <- do.call(rbind, rows)
        coef <- mean(vs_standard$Coef, na.rm = TRUE)
        if (is.nan(coef)) coef <- NA_real_
        se <- stats::sd(vs_standard$Coef, na.rm = TRUE) / sqrt(sum(!is.na(vs_standard$Coef)))
        if (is.nan(se)) se <- NA_real_
        z <- ifelse(is.na(se) || se == 0, NA_real_, coef / se)
        all_standard <- data.frame(
          Comparison = "All Appraisers vs Standard",
          N = length(unique(d$sample)),
          Coef = coef,
          SE_Coef = se,
          Z = z,
          P = ifelse(is.na(z), NA_real_, 2 * stats::pnorm(abs(z), lower.tail = FALSE))
        )
      }

      list(
        within_appraiser = within,
        between_appraisers = between,
        appraiser_vs_standard = vs_standard,
        all_appraisers_vs_standard = all_standard
      )
    },

    wide_score_matrix = function(d, rater_cols) {
      rater_id <- if (length(rater_cols) > 1 && all(rater_cols %in% names(d))) {
        do.call(paste, c(d[rater_cols], sep = "_"))
      } else {
        as.character(d[[rater_cols[[1]]]])
      }
      wide <- data.frame(sample = d$sample, rater_id = rater_id, score = d$score)
      wide <- stats::aggregate(score ~ sample + rater_id, wide, stats::median, na.rm = TRUE)
      samples <- unique(wide$sample)
      raters <- unique(wide$rater_id)
      mat <- matrix(NA_real_, nrow = length(raters), ncol = length(samples), dimnames = list(raters, samples))
      for (i in seq_len(nrow(wide))) {
        mat[as.character(wide$rater_id[i]), as.character(wide$sample[i])] <- wide$score[i]
      }
      mat
    },

    pairwise_appraisers = function(d) {
      apps <- unique(d$appraiser)
      if (length(apps) < 2) return(data.frame())
      pairs <- utils::combn(apps, 2, simplify = FALSE)
      rows <- lapply(pairs, function(pair) {
        key_cols <- if ("trial" %in% names(d)) c("sample", "trial") else "sample"
        x <- d[d$appraiser == pair[1], c(key_cols, "rating"), drop = FALSE]
        y <- d[d$appraiser == pair[2], c(key_cols, "rating"), drop = FALSE]
        names(x)[names(x) == "rating"] <- "x"
        names(y)[names(y) == "rating"] <- "y"
        m <- merge(x, y, by = key_cols)
        st <- private$two_rater_fleiss_stats(m$x, m$y, paste(pair, collapse = " vs "))
        data.frame(
          Comparison = st$Comparison,
          N = st$n,
          Percent_Agreement = 100 * st$Po,
          Kappa = st$kappa,
          SE_Kappa = st$se,
          Z = st$z,
          P_vs_gt_0 = st$p_value,
          Interpretation = self$interpret_kappa(st$kappa)
        )
      })
      do.call(rbind, rows)
    },

    appraiser_vs_standard = function(d) {
      apps <- unique(d$appraiser)
      rows <- lapply(apps, function(app) {
        ad <- d[d$appraiser == app, , drop = FALSE]
        st <- private$known_standard_kappa(ad, paste(app, "vs Standard"))
        data.frame(
          Comparison = st$Comparison,
          N = st$n,
          Percent_Agreement = st$Percent_Agreement,
          Kappa = st$kappa,
          SE_Kappa = st$se,
          Z = st$z,
          P_vs_gt_0 = st$p_value,
          Interpretation = self$interpret_kappa(st$kappa)
        )
      })
      all_st <- private$known_standard_kappa(d, "All Appraisers vs Standard")
      rows[[length(rows) + 1]] <- data.frame(
        Comparison = all_st$Comparison,
        N = all_st$n,
        Percent_Agreement = all_st$Percent_Agreement,
        Kappa = all_st$kappa,
        SE_Kappa = all_st$se,
        Z = all_st$z,
        P_vs_gt_0 = all_st$p_value,
        Interpretation = self$interpret_kappa(all_st$kappa)
      )
      do.call(rbind, rows)
    },

    known_standard_kappa = function(d, label = "Appraisers vs Standard") {
      d <- d[stats::complete.cases(d[, c("sample", "rating", "standard"), drop = FALSE]), , drop = FALSE]
      if (nrow(d) == 0) {
        return(list(Comparison = label, n = 0, Percent_Agreement = NA_real_, kappa = NA_real_, se = NA_real_, z = NA_real_, p_value = NA_real_))
      }
      categories <- unique(c(as.character(d$rating), as.character(d$standard)))
      split_id <- if ("trial" %in% names(d)) {
        interaction(d$appraiser %||% "Appraiser", d$trial, drop = TRUE, sep = "\r")
      } else {
        factor(d$appraiser %||% "Appraiser")
      }
      stats <- lapply(split(d, split_id), function(sd) {
        pairs <- sd[, c("sample", "rating", "standard"), drop = FALSE]
        pairs <- pairs[!duplicated(pairs$sample), , drop = FALSE]
        long <- rbind(
          data.frame(sample = pairs$sample, rating = pairs$rating),
          data.frame(sample = pairs$sample, rating = pairs$standard)
        )
        private$fleiss_stats_from_long(long, categories)
      })
      kappa <- mean(vapply(stats, `[[`, numeric(1), "kappa"), na.rm = TRUE)
      se_vals <- vapply(stats, `[[`, numeric(1), "se")
      se <- sqrt(sum(se_vals^2, na.rm = TRUE)) / length(stats)
      z <- ifelse(is.na(se) || se == 0, NA_real_, kappa / se)
      p_value <- ifelse(is.na(z), NA_real_, stats::pnorm(z, lower.tail = FALSE))
      list(
        Comparison = label,
        n = length(unique(d$sample)),
        Percent_Agreement = 100 * mean(as.character(d$rating) == as.character(d$standard), na.rm = TRUE),
        kappa = kappa,
        se = se,
        z = z,
        p_value = p_value,
        ci = private$.kappa_ci(kappa, se, self$params$conf_level %||% 0.95)
      )
    },

    response_kappa = function(d, categories) {
      rows <- lapply(categories, function(resp) {
        bd <- private$binary_response_long(d, resp)
        st <- private$fleiss_stats_from_long(bd[, c("sample", "rating"), drop = FALSE], c(as.character(resp), "Other"), paste("Response", resp))
        data.frame(
          Response = as.character(resp),
          Kappa = st$kappa,
          SE_Kappa = st$se,
          Z = st$z,
          P_vs_0 = st$p_value
        )
      })
      do.call(rbind, rows)
    },

    response_kappa_vs_standard = function(d, categories) {
      rows <- lapply(categories, function(resp) {
        bd <- private$binary_response_long(d, resp)
        st <- private$known_standard_kappa(bd, paste("Response", resp, "vs Standard"))
        data.frame(
          Response = as.character(resp),
          Kappa = st$kappa,
          SE_Kappa = st$se,
          Z = st$z,
          P_vs_0 = st$p_value
        )
      })
      do.call(rbind, rows)
    },

    response_summary_long = function(d) {
      responses <- sort(unique(as.character(d$rating)))
      rows <- lapply(responses, function(resp) {
        total <- sum(d$rating == resp, na.rm = TRUE)
        std_total <- if ("standard" %in% names(d)) sum(d$standard == resp, na.rm = TRUE) else NA_integer_
        correct <- if ("standard" %in% names(d)) sum(d$rating == resp & d$standard == resp, na.rm = TRUE) else NA_integer_
        data.frame(
          Response = resp,
          Ratings = total,
          Standard_Count = std_total,
          Matches_to_Standard = correct,
          Percent_Match_to_Standard = ifelse(is.na(std_total) || std_total == 0, NA_real_, 100 * correct / std_total)
        )
      })
      do.call(rbind, rows)
    },

    within_appraiser = function(d) {
      apps <- unique(d$appraiser)
      rows <- lapply(apps, function(app) {
        ad <- d[d$appraiser == app, ]
        trials <- unique(ad$trial)
        if (length(trials) < 2) return(NULL)
        wide <- reshape(ad[, c("sample", "trial", "rating")], idvar = "sample", timevar = "trial", direction = "wide")
        if (ncol(wide) < 3) return(NULL)
        trial_pairs <- utils::combn(trials, 2, simplify = FALSE)
        app_rows <- lapply(trial_pairs, function(pair) {
          x_col <- paste0("rating.", pair[1])
          y_col <- paste0("rating.", pair[2])
          if (!all(c(x_col, y_col) %in% names(wide))) return(NULL)
          pair_wide <- wide[stats::complete.cases(wide[, c(x_col, y_col), drop = FALSE]), , drop = FALSE]
          if (nrow(pair_wide) == 0) return(NULL)
          st <- private$two_rater_fleiss_stats(pair_wide[[x_col]], pair_wide[[y_col]], paste(app, "Trial", pair[1], "vs", pair[2]))
          data.frame(
            Comparison = st$Comparison,
            N = st$n,
            Percent_Agreement = 100 * st$Po,
            Kappa = st$kappa,
            SE_Kappa = st$se,
            Z = st$z,
            P_vs_gt_0 = st$p_value,
            Interpretation = self$interpret_kappa(st$kappa)
          )
        })
        app_rows <- Filter(Negate(is.null), app_rows)
        if (length(app_rows) == 0) NULL else do.call(rbind, app_rows)
      })
      rows <- Filter(Negate(is.null), rows)
      if (length(rows) == 0) data.frame() else do.call(rbind, rows)
    },

    sample_disagreement = function(d) {
      samples <- unique(d$sample)
      rows <- lapply(samples, function(sample_id) {
        sd <- d[d$sample == sample_id, , drop = FALSE]
        ratings <- as.character(sd$rating)
        tab <- sort(table(ratings), decreasing = TRUE)
        modal <- names(tab)[1] %||% NA_character_
        total <- length(ratings)
        modal_count <- if (length(tab) > 0) as.integer(tab[[1]]) else NA_integer_
        standard <- if ("standard" %in% names(sd)) as.character(sd$standard[1]) else NA_character_
        standard_matches <- if (!is.na(standard)) sum(ratings == standard, na.rm = TRUE) else NA_integer_
        data.frame(
          Sample = as.character(sample_id),
          Standard = standard,
          N_Ratings = total,
          Most_Common_Rating = modal,
          Modal_Count = modal_count,
          Percent_Modal = ifelse(total > 0, 100 * modal_count / total, NA_real_),
          Standard_Matches = standard_matches,
          Percent_Match_to_Standard = ifelse(total > 0 && !is.na(standard_matches), 100 * standard_matches / total, NA_real_),
          Discordant_Ratings = ifelse(total > 0, total - modal_count, NA_integer_)
        )
      })
      out <- do.call(rbind, rows)
      out[order(-out$Discordant_Ratings, out$Percent_Match_to_Standard, out$Sample), , drop = FALSE]
    }
  )
)
