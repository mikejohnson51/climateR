define.config = function(dataset = NULL,  model = NULL, ensemble = NULL){

if(!(dataset %in% names(model_meta))){ paste(toupper(dataset), "not parameterized!")}

config = expand.grid(model = tolower(model), ensemble = tolower(ensemble), stringsAsFactors = FALSE)
meta = eval(parse(text = paste0("model_meta$", dataset)))

bad.model = !(config$model %in% meta$model)

if(sum(bad.model) != 0){
  index = config[bad.model,]
  cat(paste(toupper(unique(index$model)), "not a valid model. Removed from query!\n\n"))
  config = config[!bad.model,]
}

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

return(config)
}
