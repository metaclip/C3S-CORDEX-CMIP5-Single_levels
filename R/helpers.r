library(magrittr)
library(metaclipR)
library(igraph)


models.voc <- "https://raw.githubusercontent.com/metaclip/CORDEX-CMIP5/refs/heads/main/CORDEX-CMIP5-models.owx"


#' @title Individual entity IRI checker
#' @description Search for an instance within the specified vocabulary and return matching lines or a warning if missing
#' @param target_instance Candidate instance
#' @param vocab_file Target vocabulary file path (possibly remotely stored if "https://" found)
#' @param silent Logical. Wheter the actual entity IRI is displayed as on-screen message
#' @keywords internal
#' @author juaco
#' @examples 
#' instance <- CanESM2_historical_r1i1p1_CCLM4-8-17
#' voc <- "/home/juaco/workspace/METACLIP/CORDEX-CMIP5/CORDEX-CMIP5-datasets.owx"
#' dataset.check(instance, voc)

entity.check <- function(target_instance = instance,
                         vocab_file = voc,
                         silent = FALSE) {

    stopifnot(is.logical(silent))

	is_remote <- grepl("^https?://", vocab_file)
	if (!is_remote && !file.exists(vocab_file)) {
		stop("No se encontró el archivo de vocabulario: ", vocab_file)
	}

	# Permite tanto archivos locales como URLs remotas en GitHub.
	con <- if (is_remote) url(vocab_file, open = "r") else file(vocab_file, open = "r")
	on.exit(close(con), add = TRUE)

	line_num <- 0L
	found_lines <- integer(0)
	found_text <- character(0)

	repeat {
		line <- readLines(con, n = 1, warn = FALSE)
		if (length(line) == 0) break
		line_num <- line_num + 1L
		if (grepl(target_instance, line, fixed = TRUE)) {
			found_lines <- c(found_lines, line_num)
			found_text <- c(found_text, line)
		}
	}

	if (length(found_lines) == 0) {
		warning("Named Individual ", target_instance, " not found in ", vocab_file)
		return(NULL)
	}

	if (!silent) message(found_text[1])
	# data.frame(line = found_lines, text = found_text, stringsAsFactors = FALSE)
}



#' @title RCP named individual matching
#' @description
#' Maps the RCP labels in master table with the corresponding vocabulary named individuals
#' @param exp Character string. Experiment label as found in master table
#' @return Character string. ipcc_terms named individual instance (ipcc: prefix)
#' @note
#' Compatible with vocabulary ipcc_terms >= 0.7
#' @author juaco
#' @keywords internal

set.rcp <- function(exp) {
    exp <- match.arg(exp, choices = c("historical",
                                      "rcp26",
                                      "rcp45",
                                      "rcp85"))
    switch(exp,
           "historical" = "ipcc:Historical",
           "rcp26" = "ipcc:RCP26",
           "rcp45" = "ipcc:RCP45",
           "rcp85" = "ipcc:RCP85"           
    )
}


#' @title Filter attribute list
#' @description Remove empty or NA attributes from a list of attributes
#' @param attr.list List of attributes to be filtered
#' @return Filtered list of attributes
#' @author juaco
#' @keywords internal
#' @examples   
#' # Example usage in a list with empty and NA values:
#' list("a"="atributoA","b"="","c"=NA,"d"="atributoD") %>% filter_attr_list()
#' # Another example with all valid attributes (the function will return the same list):
#' list("a"="atributoA","d"="atributoD") %>% filter_attr_list()
      

filter_attr_list <- function(attr.list) {
    filtered_list <- Filter(function(v) {
        v_char <- as.character(v)
        if (length(v_char) == 0) return(FALSE)
        v_scalar <- v_char[1]
        
        # Remove control characters
        v_clean <- gsub("[[:cntrl:]]", " ", v_scalar)
        
        # Replace smart quotes and dashes with ASCII equivalents
        v_clean <- gsub("[\u201C\u201D]", '"', v_clean)  # "" → "
        v_clean <- gsub("[\u2018\u2019]", "'", v_clean)  # '' → '
        v_clean <- gsub("[\u2013\u2014]", "-", v_clean)  # – — → -
        v_clean <- gsub("\u00A0", " ", v_clean)          # nbsp → space
        
        # Escape double quotes for JSON
        v_clean <- gsub('"', '\\"', v_clean, fixed = TRUE)
        
        v_trim <- trimws(v_clean)
        !is.na(v_trim) && nzchar(v_trim)
    }, attr.list)
    return(filtered_list)
}


