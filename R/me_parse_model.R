##' @title Extract a model specification from the measurement error model
##' @param model_syntax a string containing the model specification
##' @param as_list a logical for whether the result is a data frame or a list.
##' Dataframe is set as a default.
##' @param debug_code a logical for whether debugging logs should be printed.
##' @param operators permitted operators to define new lines
##' @return A data frame with the parsed model extracted into different columns
me_parse_model <- function(model_syntax = " ",
                           as_list = FALSE,
                           debug_code = FALSE,
                           operators = c("~", "=")) {

  if (length(model_syntax) == 0) {
    stop("Empty measurement error model_syntax")
  }

  # Delete comments
  model_syntax <- gsub("[#!].*(?=\n)", "", model_syntax, perl = TRUE)
  # Delete lines separated by ;
  model_syntax <- gsub(";", "\n", model_syntax, fixed = TRUE)
  # Delete tabs spaces
  model_syntax <- gsub("[ \t]+", "", model_syntax, perl = TRUE)
  # Delete new spaces of more than 2 to only one
  model_syntax <- gsub("\n{2,}", "\n", model_syntax, perl = TRUE)
  # separate intro vector
  model <- unlist(strsplit(model_syntax, "\n"))

  # Figure out lines which have operators to exclude other lines
  start_idx <-
    grep(
      paste0("[", paste0(operators, collapse = ""), "]", collapse = ""),
      model
    )

  if (length(start_idx) == 0L) {
    stop("Model does not contain measurement error syntax (operators must be one of ", paste0("'", operators, "'", collapse = ", "), ")") #nolintr
  }

  if (start_idx[1] > 1L) {
    for (el in 1:(start_idx[1] - 1L)) {
      if (nchar(model[el]) > 0L) {
        warning("No operator found in this syntax line: ",
                model[el], "\n",
                "This syntax line will be ignored!")
      }
    }
  }

  end.idx <- c(start_idx[-1] - 1, length(model))
  model.orig <- model
  model <- character(length(start_idx))

  for (i in 1:length(start_idx)) {
    model[i] <- paste(model.orig[start_idx[i]:end.idx[i]], collapse = "")
  }

  idx.wrong <-
    which(
      !grepl(paste0("[", paste0(operators, collapse = ""), "]"), model)
    )

  if (length(idx.wrong) > 0) {
    cat("Missing operator in formula(s):\n")
    print(model[idx.wrong])
    stop("Syntax error in measurement error model_syntax")
  }

  idx.wrong <- which(grepl("^\\+", model))
  if (length(idx.wrong) > 0) {
    cat("Some formula(s) start with a plus (+) sign:\n")
    print(model[idx.wrong])
    stop("Syntax error in measurement error model_syntax")
  }

  ind_formula <- grep("^~", model)
  model[ind_formula] <- paste0("X", seq_along(ind_formula), model[ind_formula])
  
  FLAT.lhs <- character(0)
  FLAT.op <- character(0)
  FLAT.rhs <- character(0)
  FLAT.rhs.mod.idx <- integer(0)
  FLAT.std <- logical(0)
  FLAT.idx <- 0L
  MOD.idx <- 0L

  # Remove duplicates
  model <- unique(model)

  for (i in 1:length(model)) {
    x <- model[i]
    if (debug_code) {
      cat("Formula to parse:\n")
      print(x)
      cat("\n")
    }
    
    if (grepl(operators[1], x, fixed = TRUE)) {
      op <- operators[1]
    } else if (grepl(operators[2], x, fixed = TRUE)) {
      op <- operators[2]
    } else {
      stop("Unknown operator in ", model[i], ". Operators must be one of ", paste0("'", operators, "'", collapse = ", "), ".") #nolintr
    }

    # Identify where the operator is
    op.idx <- regexpr(op, x)

    # Separate string
    lhs <- substr(x, 1L, op.idx - 1L)
    rhs <- substr(x, op.idx + attr(op.idx, "match.length"), nchar(x))

    if (substr(rhs, 1, 1) == "+") {
      rhs <- substr(rhs, 2, nchar(rhs))
    }

    LHS <- strsplit(lhs, split = "+", fixed = TRUE)[[1]]
    LHS <- gsub("^\\S*\\*", "", LHS)

    # If it's standardized, replace it because syntax_parse_rhs
    # fails with std() and save that it's std for later
    std_sscore <- grepl("^std\\(.+\\)$", LHS)
    if (op == "=" && std_sscore) {
      LHS <- gsub("^std\\(|\\)$", "", LHS)

      # Used below to detect the formula as std sscore
      is_std <- TRUE
    } else {
      is_std <- FALSE
    }


    if (!all(make.names(LHS) == LHS)) {
      stop("Left hand side (lhs) of this formula:\n    ", 
           lhs,
           " ",
           op,
           " ",
           rhs,
           "\n    contains a reserved word (in R): ", 
           dQuote(LHS[!make.names(LHS) == LHS]),
           "\n    see ?reserved for a list of reserved words in R", 
           "\n    please use a variable name that is not a reserved word in R"
           )
    }

    rhs_empty <- op == "~" & (rhs == "" | rhs == "1")
    if (rhs_empty) {
      stop("Right hand side (rhs) of this formula:\n  ", 
           lhs,
           " ",
           op,
           " ",
           rhs,
           "\n  is empty"
           )
    }

    lhs.names <- LHS

    rhs <- gsub("\\(?([-]?[0-9]*\\.?[0-9]*)\\)?\\?", "start(\\1)\\*", rhs)

    # If it's a sumscore, do not parse the rhs as it would evaluate
    # things likes a * b to interactions when we just want them as
    # operations
    if (op == "=") {
      out <- stats::setNames(list(empty_name = NULL), rhs)      
    } else {
      out <- syntax_parse_rhs(rhs = stats::as.formula(paste0("~", rhs)))
    }
    
    if (debug_code) print(out)
    
    for (l in 1:length(lhs.names)) {
      for (j in 1:length(out)) {
        rhs.name <- names(out)[j]
        
        FLAT.idx <- FLAT.idx + 1L
        FLAT.lhs[FLAT.idx] <- lhs.names[l]
        FLAT.op[FLAT.idx] <- op
        FLAT.rhs[FLAT.idx] <- rhs.name
        FLAT.std[FLAT.idx] <- is_std
        rhs.mod <- 0L

        FLAT.rhs.mod.idx[FLAT.idx] <- rhs.mod
        if (rhs.mod > 0L) {
          MOD.idx <- MOD.idx + 1L
        }
      }
    }
  }

  mod.idx <- which(FLAT.rhs.mod.idx > 0L)
  FLAT.rhs.mod.idx[mod.idx] <- 1:length(mod.idx)
  
  FLAT <- data.frame(lhs = FLAT.lhs,
                     op = FLAT.op,
                     rhs = FLAT.rhs,
                     std = FLAT.std,
                     mod.idx = FLAT.rhs.mod.idx,
                     stringsAsFactors = FALSE
                     )

  sscore_df <- FLAT[FLAT$op == "=", ]
  if (nrow(sscore_df) > 0) sscore_df$type  <- NA

  name_ss <- vapply(
    split(sscore_df, sscore_df$lhs),
    function(x) unique(x$std),
    FUN.VALUE = logical(1)
  )

  cmv_df <- FLAT[FLAT$op == "~", ]

  # If any variable has a standardized sscore, assign it name
  # otherwise a sscore, otherwise just observed
  cmv_df$type <-
    with(cmv_df,
         ifelse(rhs %in% names(name_ss[name_ss]), "ss_std",
                ifelse(rhs %in% names(name_ss[!name_ss]), "ss", "observed")
                )
         )

  final_df <- rbind(sscore_df, cmv_df)

  if (as_list) {
    final_df <- lapply(final_df, identity)
  }

  final_df
}

syntax_parse_rhs <- function (rhs) {
  out <- list()

  all_terms <- attr(stats::terms(rhs), "term.labels")
  out <- stats::setNames(vector("list", length(all_terms)), all_terms)

  if (length(out) > 1L) {
    rhs.names <- names(out)
    while (!is.na(idx <- which(duplicated(rhs.names))[1L])) {
      dup.name <- rhs.names[idx]
      orig.idx <- match(dup.name, rhs.names)
      merged <- c(out[[orig.idx]], out[[idx]])
      if (!is.null(merged)) out[[orig.idx]] <- merged
      out <- out[-idx]
      rhs.names <- names(out)
    }
  }
  
  out
}
