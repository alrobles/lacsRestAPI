#!/usr/bin/env Rscript

## ---- load packages ----
library(RestRserve)
library(text2vec)
library(glmnet)
library(tidyverse)


model <- readr::read_rds("/home/alrobles/textapi/glmnet_classifier.rds")
vocabulary <-  readr::read_rds("/home/alrobles/textapi/vocab.rds")

## ---- Functions ----

# function to score an abstract given a model

score_abstract <- function(abstract){
  prep_fun = function(x) {
    # make text lower case
    x = str_to_lower(x)
    # remove non-alphanumeric symbols
    x = str_replace_all(x, "[^[:alnum:]]", " ")
    # collapse multiple spaces
    str_replace_all(x, "\\s+", " ")
  }
  
  vocabulary <- vocab
  vectorizer <-  vocab_vectorizer(vocabulary)
  
  classification_score <- if(nchar(abstract) > 10){
    trial_text <- abstract
    trial_text <- prep_fun(trial_text)
    it_test = text2vec::itoken(trial_text, progressbar = FALSE)
    dtm_test = create_dtm(it_test, vectorizer)
    preds = 1 -  predict(model, dtm_test, type = 'response')[,1]
    preds = round(preds, 3)
    return(preds)
  } else {
    return(0)
  }
}

abstract_handler = function(.req, .res){
  abstract = as.character(.req$parameters_query[["abstract"]])
  if (length(abstract) == 0L || is.na(abstract)) {
    raise(HTTPError$bad_request())
  }
  .res$set_body(as.character(score_abstract(abstract = abstract)))
  .res$set_content_type("text/plain")
}

app$add_get(path = "/scoreabstract", FUN = abstract_handler)
request = Request$new(path = "/scoreabstract", parameters_query = list(abstract = "this is an abstract test"))
response = app$process_request(request)

###
app$add_openapi(
  path = "/openapi.yaml",
  file_path = "openapi.yaml"
)

# see details on https://swagger.io/tools/swagger-ui/
app$add_swagger_ui(
  path = "/swagger",
  path_openapi = "/openapi.yaml",
  path_swagger_assets = "/swagger/assets/",
  file_path = tempfile(fileext = ".html"),
  use_cdn = FALSE
)

backend = BackendRserve$new()
backend$start(app, http_port = 8082)

###