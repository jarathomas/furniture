#' Regression Table
#' 
#' Regression coefficients and statistics printed in a nicely formatted table.
#' 
#' @param ... the unquoted names of lm() object names to be included in the table
#' @param rounding the value passed to \code{round} for the output of both the correlation and p-value; default is 3
#' @param output how the table is output; can be "text" for regular console output, "latex2" for specialized latex output, or any of \code{kable()}'s options from \code{knitr} (e.g., "latex", "markdown", "pandoc").
#' @param booktabs when \code{output != "text"}; option is passed to \code{knitr::kable}
#' @param caption when \code{output != "text"}; option is passed to \code{knitr::kable}
#' @param align when \code{output != "text"}; option is passed to \code{knitr::kable}
#' @param float when \code{output == "latex2"} it controls the floating parameter (h, t, b, H)
#'
#' @seealso stats::lm
#' 
#' @importFrom stats lm
#' @importFrom knitr kable
#' @return a list of class \code{tableR} containing the table with regression results
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' library(furniture)
#' data(nhanes_2010)
#' m1 <- lm(active ~ age + gender, data = nhanes_2010)
#' m2 <- update(m1, . ~ . + asthma + low_int)
#' m3 <- update(m1, . ~ . + asthma + low_energy)
#' m4 <- update(m2, . ~ . + low_energy)
#' m5 <- update(m4, . ~ . + low_energy*gender)
#' tableR(m1, m2, m3, m4, m5)
#' } 
#' @export
tableR <- function(..., 
                   rounding = 3,
                   output = c("text", "latex", "latex2", "markdown", "pandoc")[1],
                   booktabs = TRUE, 
                   caption = NULL, 
                   align = NULL,
                   float = "htb"){
  
  ## Preprocessing ##
  model_list <- list(...) # model_list <- list(m1, m2, m3, m4, m5)
  # model_list <- list(m1, m2, m3, m4, m5, "hi", "error")
  index_not_lm <- which(lapply(model_list, class) != "lm")
  if (length(index_not_lm) > 0) {
    names_not_lm <- paste(unlist(model_list[index_not_lm]), collapse = ", ")
    stop(paste0("The following objects need to be of class lm: ", names_not_lm))
  }
  rnd_coef <- rounding - 1
  n_models <- length(model_list)
  n_terms <- lapply(model_list, function(x) length(x$coef))
  model_order <- order(unlist(n_terms))
  model_max_terms <- which.max(n_terms)
  terms_names <- names(model_list[[model_max_terms]]$coef)
  
  coef_list <- lapply(model_list,
                     function(x) format(coef(x), digits = rnd_coef))
  
  coef_list <- lapply(coef_list, function(x) x[terms_names])
  coef_matrix <- matrix(unlist(coef_list), ncol = n_models)
  coef_matrix <- coef_matrix[, model_order]
  coef_matrix[is.na(coef_matrix)] <- ""
  
  ## add (formatted) row and column names ##
  terms_names <- gsub("\\(Intercept\\)", "intercept", terms_names)
  terms_names <- gsub("factor\\(([A-z]+)\\)([0-9]+)",
                     "\\1:\\2", terms_names)
  rownames(coef_matrix) <- terms_names
  colnames(coef_matrix) <- paste0("**Model ", 1:n_models, "**")
  
  ## Output ##
  if (output != "text"){
    if (output == "latex2"){
      if (is.null(align)){
        l1 <- dim(final)[2]
        align <- c("l", rep("c", (l1-1)))
      }
      tab <- to_latex(final, caption, align, len = dim(final)[2] - 1, splitby = NA, float, booktabs, cor_type)
      return(tab)
      
    } else {
      kab <- knitr::kable(final,
                          format=output,
                          booktabs = booktabs,
                          caption = caption,
                          align = align,
                          row.names = FALSE)
      return(kab)
    }
  } else {
    
    final <- list("coef_matrix" = final)
    class(final) <- c("tableR", "list")
    return(final)
  }
}
