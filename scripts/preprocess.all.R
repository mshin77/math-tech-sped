preprocess.all <- function(data, text_field = "united_texts", ...) {
    
    # Preprocess data 
    # Construct a corpus.
    library(quanteda)
    corp <- corpus(data, text_field = text_field)
    
    # Change the docnames to a meaningful identifier.
    docnames(corp) <- united_tbl$study_number
    
    # Segment texts in a corpus into tokens (words or sentences) by word boundaries.
    toks <- tokens(corp)
    
    # Preprocess tokens.
    toks_clean <- tokens(
        toks,
        what = "word",
        remove_punct = TRUE,
        remove_symbols = TRUE,
        remove_numbers = TRUE,
        remove_url = TRUE,
        remove_separators = TRUE,
        split_hyphens = TRUE,
        split_tags = TRUE,
        # remove_hyphens = TRUE,
        include_docvars = TRUE,
        padding = FALSE,
        verbose = TRUE) 
    
    # Convert the features of a tokens to lowercase.
    toks_lower <- tokens_tolower(toks_clean, 
                                 keep_acronyms = FALSE)
    
    
    # Load the customized dictionary 1 (compound words).
    source("scripts/dictionary_list_1.R", local = TRUE)
    
    toks_lower_comp <- toks_lower %>% 
        tokens_lookup(dictionary = dictionary(dictionary_list_1),
                      valuetype = "glob",
                      verbose = TRUE,
                      exclusive = FALSE,
                      capkeys = FALSE)
    
    # Load the customized dictionary 2.
    source("scripts/dictionary_list_2.R", local = TRUE)
    
    toks_lower_comp_dict <- toks_lower_comp %>% 
        tokens_lookup(dictionary = dictionary(dictionary_list_2),
                      valuetype = "glob",
                      verbose = TRUE,
                      exclusive = FALSE,
                      capkeys = FALSE)
    
    toks_lower_comp_dict_no_stop <- toks_lower_comp_dict %>%
        tokens_remove(stopwords("en"),
                      valuetype = "glob",
                      window = 0,
                      verbose = TRUE,
                      padding = TRUE)
    
    # Specify the minimum length in characters for tokens (at least 2).
    toks_lower_comp_dict_no_stop_adj <- toks_lower_comp_dict_no_stop %>% 
        tokens_select(min_nchar=2L,
                      verbose = TRUE)
    
    return(toks_lower_comp_dict_no_stop_adj)
    
}

