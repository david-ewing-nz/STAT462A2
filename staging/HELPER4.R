```{r  , include=FALSE}
#' Format a data frame as a styled flextable
pretty_df <- function(df,
                      title    = NULL,
                      fontsize = 10,
                      n        = 5) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }
  
  # Optional row slicing
  if (n > 0) df <- head(df, n)
  if (n < 0) df <- tail(df, abs(n))
  
  ft <- flextable::flextable(df) |>
    flextable::fontsize(size = fontsize, part = "all") |>
    flextable::padding(padding = padding, part = "all") |>
    flextable::align(align = "center", part = "all") |>
    flextable::theme_booktabs() |>
    flextable::bold(i = 1, part = "header") |>
    flextable::autofit()
  
  current_width <- sum(ft$body$colwidths)
  
  if (!is.null(title)) {
    estimated_char_width <- 0.07
    title_width <- nchar(title) * estimated_char_width
    if (title_width > current_width) {
      scale_factor <- title_width / current_width
      new_widths <- ft$body$colwidths * scale_factor
      for (j in seq_along(new_widths)) {
        ft <- flextable::width(ft, j = j, width = new_widths[j])
      }
    }
    ft <- flextable::set_caption(ft, caption = title)
  }
  
  return(ft)
}

pretty_df <- function(df,
                      title    = NULL,
                      fontsize = 10,
                      n        = 5) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }
  
  # Optional row slicing
  if (n > 0) df <- head(df, n)
  if (n < 0) df <- tail(df, abs(n))
  
  ft <- flextable::flextable(df)
  
  if (!is.null(title)) {
    ft <- flextable::set_caption(ft, title)
  }
  
  ft <- ft |>
    flextable::fontsize(size = fontsize, part = "all") |>
    flextable::align(align = "center", part = "all") |>
    flextable::theme_booktabs() |>
    flextable::bold(i = 1, part = "header") |>
    flextable::padding(padding = 5, part = "all") |>
    flextable::autofit()
  
  return(ft)
}


#' Format confusion matrix as flextable
pretty_cm <- function(cm, caption = "Confusion Matrix") {
  df <- as.data.frame.matrix(cm)
  pretty_df(df, caption = caption)
}

#' Format classification metrics
pretty_metrics <- function(truth, predicted, caption = "Classification Metrics") {
  tab <- yardstick::metrics_vec(truth = truth, estimate = predicted, estimator = "macro")
  df <- as.data.frame(tab)
  pretty_df(df, caption = caption)
}

#' Summarise train/test splits
pretty_split <- function(train, test, target, caption = "Train/Test Split Summary") {
  s <- function(data) {
    tbl <- table(data[[target]])
    out <- data.frame(
      Total = nrow(data),
      Class_1 = tbl[1],
      Class_2 = tbl[2],
      Prop_1 = round(tbl[1] / sum(tbl), 3),
      Prop_2 = round(tbl[2] / sum(tbl), 3)
    )
    rownames(out) <- NULL
    out
  }
  summary_df <- rbind(
    cbind(Set = "Train", s(train)),
    cbind(Set = "Test", s(test))
  )
  pretty_df(summary_df, caption = caption)
}


# ---- Pretty csv_read with Type Summary and Preview ------
# ---- Pretty csv_read with Type Summary and Preview ------ 
# works with flextable; provides a summary of column types
# and a preview

pretty_read_csv <- \(path, n = 5, col_names = TRUE, show_col_types = FALSE) {   
  df <- readr::read_csv(path, show_col_types = FALSE, col_names = col_names)
  df <- as.data.frame(df)
  ft <- pretty_df(df,glue("CSV:{path}"))
  return(list(df=df,ft=ft))
}

# ---- Pretty Excel Reader with Type Summary and Preview ----
# ---- Pretty Excel Reader with Type Summary and Preview ----
# Requires: readxl, flextable, purrr

pretty_read_xlsx <- \(path, sheet = 1, col_names = TRUE, n = 0) {
  # Read Excel file
  df <- readxl::read_excel(path, sheet = sheet, col_names = col_names)
  df <- as.data.frame(df)  
  # Extract column types
  types <- purrr::map_chr(df, typeof)
  type_df <- data.frame(
    Column = names(df), 
    Type = types,
    stringsAsFactors = FALSE
  )
  
  ft = pretty_df(type_df,glue("XLSX Column Types:{path}"))
  
  return(list(df=df,ft=ft))
}




# ---- Pretty Excel Reader with Type Summary and Preview ----
# ---- Pretty Excel Reader with Type Summary and Preview ----
# Requires: readxl, flextable, purrr

pretty_ggplot <- \(plot, title = "ggplot Summary") {
  if (!inherits(plot, "gg")) stop("Input must be a ggplot object.")
  
  # Extract key components
  plot_data <- tryCatch(plot$data, error = function(e) NULL)
  geoms <- sapply(plot$layers, function(layer) class(layer$geom)[1])
  mappings <- plot$mapping
  
  # Pull out global aesthetics as strings
  global_aes  <- names(mappings)
  global_vals <- sapply(mappings, function(x) rlang::expr_text(x))
  
  # Additional metadata
  p_title <- plot$labels$title %||% title %||% ""
  x_lab   <- plot$labels$x %||% ""
  y_lab   <- plot$labels$y %||% ""
  colour_lab <- plot$labels$colour %||% plot$labels$color %||% ""
  
  # Build a metadata data frame
  df <- data.frame(
    Component = c(
      "Title",
      "X Axis",
      "Y Axis", 
      "Colour Legend",
      "Geoms",
      global_aes),
    Value     = c(
      p_title, 
      x_lab,    
      y_lab,    
      colour_lab,
      paste(geoms , collapse = ","),
      global_vals),
    stringsAsFactors = FALSE
  )
  
  ft = pretty_df(df)
  return(ft)
}






pretty_split_df <- function(df,
                            cols = 6,
                            title = NULL,
                            fontsize = 10,
                            n = 5) {
  if (!is.data.frame(df)) {
    stop(cat("\\textit{Object is not a data frame:}", deparse(df)))
  }
  
  title <- if (is.null(title)) deparse(substitute(df)) else title
  df_show <- if (n > 0) head(df, n) else if (n < 0) tail(df, abs(n)) else df
  col_groups <- split(names(df_show), ceiling(seq_along(df_show) / cols))
  
  # Return a named list of flextables created by pretty_df
  ft_list <- lapply(seq_along(col_groups), function(i) {
    subdf <- df_show[, col_groups[[i]], drop = FALSE]
    pretty_df(
      subdf,
      title = paste0(title, " (", i, ")"),
      fontsize = fontsize,
      n = n
    )
  })
  
  names(ft_list) <- paste0(
    title,
    " (",
    seq_along(ft_list),
    ")"
  )
  
  return(ft_list)
}


#' Generate a full QDA summary with priors, centroids, and equations
pretty_qda <- function(qda_model) {
  # Extract response variable name
  response_var <- as.character(attr(qda_model$terms, "variables")[[2]])
  
  # Extract predictor variable names
  predictor_vars <- attr(qda_model$terms, "term.labels")
  
  # Create dynamic symbolic equation
  qda_equation <- paste0(
    "P(", response_var, " = k | ", paste(predictor_vars, collapse = ", "), 
    ") ∝ π_k × f_k(", paste(predictor_vars, collapse = ", "), ")"
  )
  
  # Create centroids data frame
  centroids_df <- as.data.frame(qda_model$means)
  centroids_df[[response_var]] <- rownames(centroids_df)
  centroids_df <- centroids_df[, c(response_var, setdiff(names(centroids_df), response_var))]
  
  # Create priors data frame
  priors_df <- as.data.frame(qda_model$prior)
  priors_df[[response_var]] <- rownames(priors_df)
  colnames(priors_df) <- c("prior_probability", response_var)
  priors_df <- priors_df[, c(response_var, "prior_probability")]
  
  # Return as a list
  return(list(
    centroids = centroids_df,
    priors = priors_df,
    equation = qda_equation
  ))
}


#' Format classification thresholds summary (e.g. probability cutoff impact)
pretty_thresh <- function(predicted_probs, truth, threshold = 0.5, positive_class = NULL, caption = "Threshold-Based Classification Summary") {
  predicted_class <- ifelse(predicted_probs >= threshold, positive_class, paste0("not_", positive_class))
  cm <- table(Truth = truth, Predicted = predicted_class)
  pretty_cm(cm, caption = caption)
}

#' Summarise variable transformations (e.g., h_ to hp_, c_ to cp_)
pretty_transforms <- function(transform_map, caption = "Variable Transformation Mapping") {
  df <- as.data.frame(transform_map)
  names(df) <- c("Original", "Transformed")
  pretty_df(df, caption = caption)
}

#' Summarise missing value patterns
pretty_missing <- function(df, caption = "Missing Data Summary") {
  missings <- sapply(df, function(x) sum(is.na(x)))
  df_out <- data.frame(Variable = names(missings), Missing_Count = missings)
  df_out <- df_out[df_out$Missing_Count > 0, ]
  pretty_df(df_out, caption = caption)
}

#' Display model object summary (generic use for tree, lm, etc.)
pretty_model <- function(model, caption = "Model Summary") {
  summary_text <- capture.output(summary(model))
  df <- data.frame(Summary = summary_text)
  pretty_df(df, caption = caption)
}

#' Format and preview a CSV file as a flextable
pretty_csv <- function(path, caption = NULL, ...) {
  df <- readr::read_csv(path, ...)
  pretty_df(df, caption = ifelse(is.null(caption), basename(path), caption))
}

#' Format and preview an Excel file (first sheet) as a flextable
pretty_xls <- function(path, sheet = 1, caption = NULL, ...) {
  df <- readxl::read_excel(path, sheet = sheet, ...)
  pretty_df(df, caption = ifelse(is.null(caption), basename(path), caption))
}

#' Pretty summary for numeric columns
pretty_summary <- function(df) {
  ft <- dplyr::select(df, where(is.numeric)) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        .fns = list(
          Mean     = ~mean(.x, na.rm = TRUE),
          Median   = ~median(.x, na.rm = TRUE),
          Min      = ~min(.x, na.rm = TRUE),
          Max      = ~max(.x, na.rm = TRUE),
          IQR      = ~IQR(.x, na.rm = TRUE),
          nNA      = ~sum(is.na(.x))
        )
      )
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = c("Variable", ".value"),
      names_sep = "_"
    ) |>
    dplyr::mutate(dplyr::across(where(is.numeric), round, 2)) |>
    flextable::flextable() |>
    flextable::set_header_labels(
      Variable = "Feature",
      Mean     = "Mean",
      Median   = "Median",
      Min      = "Min",
      Max      = "Max",
      IQR      = "Interquartile Range",
      nNA      = "Missing Values"
    ) |>
    flextable::autofit() |>
    flextable::theme_vanilla()
  ft
}

#' Pretty read_csv with summary and preview
pretty_read_csv <- function(path, n = 5, col_names = TRUE, show_col_types = FALSE) {
  df <- readr::read_csv(path, show_col_types = FALSE, col_names = col_names)
  df <- as.data.frame(df)
  ft <- pretty_df(df, glue::glue("CSV: {path}"))
  list(df = df, ft = ft)
}

#' Pretty Excel Reader with type summary and preview
pretty_read_xlsx <- function(path, sheet = 1, col_names = TRUE, n = 0) {
  df <- readxl::read_excel(path, sheet = sheet, col_names = col_names)
  df <- as.data.frame(df)
  types <- purrr::map_chr(df, typeof)
  type_df <- data.frame(Column = names(df), Type = types, stringsAsFactors = FALSE)
  ft <- pretty_df(type_df, glue::glue("XLSX Column Types: {path}"))
  list(df = df, ft = ft)
}

#' Pretty ggplot metadata summary
pretty_ggplot <- function(plot, title = "ggplot Summary") {
  if (!inherits(plot, "gg")) stop("Input must be a ggplot object.")
  
  plot_data <- tryCatch(plot$data, error = function(e) NULL)
  geoms <- sapply(plot$layers, function(layer) class(layer$geom)[1])
  mappings <- plot$mapping
  
  global_aes  <- names(mappings)
  global_vals <- sapply(mappings, function(x) rlang::expr_text(x))
  
  p_title <- plot$labels$title %||% title %||% ""
  x_lab   <- plot$labels$x %||% ""
  y_lab   <- plot$labels$y %||% ""
  colour_lab <- plot$labels$colour %||% plot$labels$color %||% ""
  
  df <- data.frame(
    Component = c("Title", "X Axis", "Y Axis", "Colour Legend", "Geoms", global_aes),
    Value = c(p_title, x_lab, y_lab, colour_lab, paste(geoms, collapse = ","), global_vals),
    stringsAsFactors = FALSE
  )
  pretty_df(df)
}

#' Pretty glm model summary
pretty_glm <- function(model) {
  if (!inherits(model, "glm")) {
    stop("pretty_glm() expects a glm object.")
  }
  tryCatch({
    df <- data.frame(
      Metric = c(
        "Formula", "AIC", "Null deviance", "Residual deviance",
        "Component names", "Model class"
      ),
      Value = c(
        deparse(formula(model)),
        AIC(model),
        model$null.deviance,
        model$deviance,
        paste(names(model), collapse = ", "),
        paste(class(model), collapse = ", ")
      ),
      stringsAsFactors = FALSE
    )
    pretty_df(df,  "GLM Model Summary")
  }, error = function(e) {
    cat("Error in pretty_glm():", conditionMessage(e), "\n")
    NULL
  })
}

#' Pretty split df into multiple flextables
pretty_split_df <- function(df, cols = 6, title = NULL, fontsize = 10, n = 5) {
  if (!is.data.frame(df)) stop("Object is not a data frame.")
  title <- if (is.null(title)) deparse(substitute(df)) else title
  df_show <- if (n > 0) head(df, n) else if (n < 0) tail(df, abs(n)) else df
  col_groups <- split(names(df_show), ceiling(seq_along(df_show) / cols))
  ft_list <- lapply(seq_along(col_groups), function(i) {
    subdf <- df_show[, col_groups[[i]], drop = FALSE]
    pretty_df(subdf, title = paste0(title, " (", i, ")"), fontsize = fontsize, n = n)
  })
  names(ft_list) <- paste0(title, " (", seq_along(ft_list), ")")
  ft_list
}


pretty_model_comparison <- function(model_results) {
  stopifnot(is.list(model_results))
  
  summary_list <- lapply(names(model_results), function(name) {
    result <- model_results[[name]]
    actual <- result$actual
    predicted <- result$predicted
    
    # Confusion matrix
    cm <- table(Predicted = predicted, Actual = actual)
    
    TP <- cm["DEAD", "DEAD"]
    TN <- cm["LIVE", "LIVE"]
    FP <- cm["DEAD", "LIVE"]
    FN <- cm["LIVE", "DEAD"]
    
    accuracy  <- (TP + TN) / sum(cm)
    precision <- TP / (TP + FP)
    recall    <- TP / (TP + FN)
    f1        <- 2 * precision * recall / (precision + recall)
    
    data.frame(
      Model     = name,
      Accuracy  = round(accuracy, 3),
      Precision = round(precision, 3),
      Recall    = round(recall, 3),
      F1        = round(f1, 3),
      stringsAsFactors = FALSE
    )
  })
  
  summary_df <- do.call(rbind, summary_list)
  pretty_df(summary_df, title = "Model Comparison Summary")
}



pretty_model_comparison <- function(model_list) {
  stopifnot(is.list(model_list))
  
  # Storage
  comparison <- data.frame()
  
  for (name in names(model_list)) {
    entry <- model_list[[name]]
    
    # Required elements in each entry
    actual <- entry$actual
    predicted <- entry$predicted
    
    # Generate confusion matrix
    cm <- make_conf_matrix(predicted, actual)
    metrics <- eval_conf_matrix(cm)
    
    comparison <- rbind(comparison, data.frame(
      Model        = name,
      Accuracy     = round(metrics[2], 3),
      Precision    = round(metrics[3], 3),
      Recall       = round(metrics[4], 3),
      F1_Score     = round(metrics[5], 3),
      MisclassRate = round(metrics[1], 3)
    ))
  }
  
  # Reorder columns to preference
  comparison <- comparison[, c("Model", "Accuracy", "Precision", "Recall", "F1_Score", "MisclassRate")]
  
  # Return as pretty flextable
  pretty_df(comparison, title = "Model Performance Comparison")
}






```



```{r  }
# --- Model Evaluation Utilities ---

model_boolean_eval <- function(model, pred_class, actual_class, train_data, threshold = 0.5) {
  pvals <- broom::tidy(model)$p.value
  sig_all <- all(pvals < 0.05)
  
  test_acc <- mean(pred_class == actual_class)
  base_acc <- max(prop.table(table(actual_class)))
  better_than_random <- test_acc > 0.5
  
  better_than_baseline_mild     <- (test_acc - base_acc) >= 0.01
  better_than_baseline_moderate <- (test_acc - base_acc) >= 0.015
  better_than_baseline_strong   <- (test_acc - base_acc) >= 0.02
  real_world_gain               <- (test_acc - base_acc) >= 0.05
  
  train_probs <- predict(model, newdata = train_data, type = "response")
  train_class <- ifelse(train_probs > threshold, 1, 0)
  train_acc <- mean(train_class == train_data$DEATH)
  well_generalised <- abs(train_acc - test_acc) < 0.05
  
  list(
    sig_all = sig_all,
    test_acc = test_acc,
    base_acc = base_acc,
    better_than_random = better_than_random,
    better_than_mild = better_than_baseline_mild,
    better_than_moderate = better_than_baseline_moderate,
    better_than_strong = better_than_baseline_strong,
    well_generalised = well_generalised,
    real_world_gain = real_world_gain
  )
}

summarise_boolean_eval <- function(eval_list) {
  data.frame(
    Question = c(
      "Statistically significant?",
      "Better than random?",
      "Better than majority guess? (≥ 0.01)",
      "Better than majority guess? (≥ 0.015)",
      "Better than majority guess? (≥ 0.02)",
      "Well-generalised?",
      "Real-world useful?"
    ),
    Result = unlist(c(
      eval_list$sig_all,
      eval_list$better_than_random,
      eval_list$better_than_mild,
      eval_list$better_than_moderate,
      eval_list$better_than_strong,
      eval_list$well_generalised,
      eval_list$real_world_gain
    )),
    Justification = c(
      "all(pvals < 0.05)",
      "test_acc > 0.5",
      "(test_acc - base_acc) >= 0.01",
      "(test_acc - base_acc) >= 0.015",
      "(test_acc - base_acc) >= 0.02",
      "abs(train_acc - test_acc) < 0.05",
      "(test_acc - base_acc) >= 0.05"
    ),
    stringsAsFactors = FALSE
  )
}


#Convert probabilities to labelled factor predictions
predict_classes <- function(probs, threshold = 0.5) {
  factor(ifelse(probs > threshold, 1, 0), levels = c(0, 1), labels = c("LIVE", "DEAD"))
}

# 2. Create a labelled confusion matrix from predicted and actual class labels
make_conf_matrix <- function(pred_class, actual_class) {
  table(
    Predicted = factor(pred_class, levels = c("LIVE", "DEAD")),
    Actual    = factor(actual_class, levels = c("LIVE", "DEAD"))
  )
}

# 3. Compute performance metrics from a 2x2 labelled confusion matrix
# Returns: misclassification, accuracy, precision, recall, F1
eval_conf_matrix <- function(cmtx) {
  tp <- cmtx["DEAD", "DEAD"]
  tn <- cmtx["LIVE", "LIVE"]
  fp <- cmtx["DEAD", "LIVE"]
  fn <- cmtx["LIVE", "DEAD"]
  total <- sum(cmtx)
  
  if (total == 0 || any(is.na(c(tp, tn, fp, fn)))) return(rep(NA, 5))
  
  accuracy  <- (tp + tn) / total
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else NA
  recall    <- if ((tp + fn) > 0) tp / (tp + fn) else NA
  f1        <- if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0)
    2 * precision * recall / (precision + recall) else NA
  misclass  <- 1 - accuracy
  
  c(misclass, accuracy, precision, recall, f1)
}

# 4. Convert a vector of named metrics into a summary data frame
summarise_metrics <- function(metrics_vec) {
  metric_names <- c("Misclassification Rate", "Accuracy", "Precision (DEAD)", "Recall (DEAD)", "F1 Score (DEAD)")
  data.frame(Metric = metric_names, Value = round(metrics_vec, 3), stringsAsFactors = FALSE)
}
```



