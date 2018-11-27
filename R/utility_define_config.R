.random_model = function(dataset, n = 10){
  m = eval(parse(text = paste0('model_meta$', dataset, "$model")))
  m = unique(m)
  if(n > length(m)){ n = length(m)}
  m = sample(m, n)
  return(m)
}


define.config = function(dataset = NULL, model, ensemble = NA){

  if(!(dataset %in% names(model_meta))){ paste(toupper(dataset), "not parameterized!")}

  if(is.numeric(model)){ model = .random_model(dataset = dataset, n = model) }

  config = expand.grid(model = model, ensemble = tolower(ensemble), stringsAsFactors = FALSE)
  meta = eval(parse(text = paste0("model_meta$", dataset)))

  bad.model = !(tolower(config$model) %in% tolower(meta$model))

  if(sum(bad.model) != 0){
    index = config[bad.model,]
    cat(paste(toupper(unique(index$model)), "not a valid model. Removed from query!\n\n"))
    config = config[!bad.model,]
  }

  if(!is.na(ensemble)){
    for(i in 1:length(unique(config$model))){
    tmp = config[config$model == unique(config$model)[i], ]
    tmp.ensemble = do.call(paste,(meta[which(meta$model == unique(config$model)[i]),grep("ensemble",colnames(meta))]))
    bad.ensemble = !sapply(ensemble, grepl, tmp.ensemble)
    if(sum(bad.ensemble) != 0){
      cat(paste(toupper(names(bad.ensemble)[bad.ensemble]), "not a model ensemble member for", unique(config$model)[i], ". Removed from query!\n\n"))
      index = (config$model == unique(config$model)[i]) + (config$ensemble == names(bad.ensemble)[bad.ensemble])
      config = config[index != 2,]
    }
    }
    row.names(config) = c(1:NROW(config))
  } else {
    config = config$model
  }

  if(NROW(config) == 0){
    cat(paste("No valid model selected.\n\n"))
  }

  return(config)
}
