#' Convert number to English prose
#' 
#' @description This takes an arbitrary numbers and prints like a human would transcribe it. Designed to be used within \code{\\Sexpr}.
#' 
#' @name texNum
#' 
#' @param number A single numeric vector.
#' @param sig.figs Significant figures to be displayed
#' @param dollar Logical, should a LaTeX dollar sign (\code{\\$}) be prefixed.
#' @param pre.phrase A length 2 character vector to insert a phrase before the number. The first element is the phrase to be used if the output rounds up before printing (i.e. the original number is smaller than the number printed); the second is the phrase to be used if the number rounds down. The default is \code{NULL}, i.e. no phrase inserted. If either string has a trailing tilde (e.g. \code{"almost~"}), the tilde separates the number and the phrase (as a LaTeX control sequence).
#' @param .suffix Either \code{NULL} for an automatic suffix based on the magnitude of the number, or one of million, billion, trillion for a fixed suffix.
#' @examples 
#' texNum(500e3)
#' texNum(500e3 - 1, pre.phrase = c("almost", "over"))
#' texNum(500e6)
#' @return A character string representing the number as appropriate for English prose.
#' @export 

texNum <- function(number, sig.figs = 3L, dollar = FALSE, pre.phrase = NULL, .suffix = NULL){
  orig.number <- number
  if (length(number) != 1L) {
    stop("`number` had length ", length(number), ". ",
         "Only length-1 is supported. Ensure `number` is a single number.")
  }
  if (!is.numeric(number)) {
    stop("`number` was type ", typeof(number), ", but must be a number.",
         "Ensure `number` is a single number.")
  }
  
  stopifnot(is.numeric(number), length(number) == 1L)
  is.negative <- number < 0
  number <- abs(number)
  if (number == 0){
    warning("Returning 0")
    return(0)
  } else {
    if (is.null(.suffix)){
    n.digits <- ceiling(log10(number))
    
    suffix <- NULL
    suffix_val <- 1
    
    if (n.digits < sig.figs){
      prefix <- signif(x = number, digits = sig.figs)
    } else {
      
      if (n.digits <= 6) {
        prefix_val <- round(number, sig.figs - n.digits - 1)
        prefix <- prettyNum(prefix_val, big.mark = ",", scientific = FALSE)
      } else {
        # Want to show only the number / 10^(multiple of 3) then the suffix multiplier
        suffix_val <- 10 ^ (3 * ((n.digits %/% 3)))
        prefix_val <- signif(number/suffix_val, digits = sig.figs)
        prefix <- prefix_val
        
        if (suffix_val <= 10^12){
          switch(log10(suffix_val) / 3 - 1,
                 suffix <- "~million", 
                 suffix <- "~billion", 
                 suffix <- "~trillion")
        } else {
          prefix <- signif(number / 10^12, digits = sig.figs)
          suffix <- "~trillion"
        }
      }
    }
    } else {
      stopifnot(.suffix %in% c("million", "billion", "trillion"))
      switch(.suffix, 
             "million" = {
              prefix <- signif(number / 10^6, digits = sig.figs)
              suffix <- "~million"
              suffix_val <- 10^6
             }, 
             "billion" = {
               prefix <- signif(number / 10^9, digits = sig.figs)
               suffix <- "~billion"
               suffix_val <- 10^9
             }, 
             "trillion" = {
               prefix <- signif(number / 10^12, digits = sig.figs)
               suffix <- "~trillion"
               suffix_val <- 10^12
             })
      prefix_val <- prefix
    }
    
    if (dollar){
      out <- paste0("\\$", prefix, suffix)
    } else {
      out <- paste0(prefix, suffix)
    }
    
    if (is.negative){
      # General LaTeX
      out <- paste0("\\(-\\)", out)
    }
    # is the displayed number larger than the original?
    if (!is.null(pre.phrase)){
      out_larger <- prefix_val * suffix_val > orig.number
      
        if (out_larger) {
          out <- paste(pre.phrase[1], out, sep = if(grepl("~$", pre.phrase[1])) "" else " ")
        } else {
          if (!isTRUE(all.equal(prefix_val * suffix_val, orig.number, tolerance = .Machine$double.eps)))
            out <- paste(pre.phrase[2], out, sep = if(grepl("~$", pre.phrase[2])) "" else " ")
        }
      
    }
    return(out)
  }
}
