define.param = function(param, service = NULL){

  all = eval(parse(text = paste0('param_meta$', service)))

  if(is.null(param)){ stop("No parameters defined. Select from: \n'", paste(all$common.name, collapse = "', '"))}

  param = tolower(param)

  if (!all(param %in% all$common.name)){
    bad.param <- param[!(param %in% all$common.name)]
    stop("'", paste(bad.param, collapse = "', '"), "'",
         ifelse(length(bad.param) > 1, "are", "is"),
         " not a valid parameter for ", toupper(service), " data. Valid options include: \n'",
         paste(all$common.name, collapse = "', '"), "'")
  }

  p = all[which(all$common.name %in% param),]

  return(p)

}

