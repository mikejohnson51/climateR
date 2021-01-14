#' @title Define climateR configuration
#' @description **INTERNAL** Define the parameter configuration to call with climateR. Ensures that the specificed parameters
#' are avialable and structured in the variable sapce of the datasource.
#' @param param the parameter(s) of interest
#' @param service the dataset for which a configuration is needed
#' @author Mike Johnson
#' @return a vector of N GCMs
#' @keywords internal

define.param = function(param, service = NULL){

  all = eval(parse(text = paste0('climateR::param_meta$', service)))

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

