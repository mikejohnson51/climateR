## code to prepare `model_meta` dataset goes here

## LOCA
L = readLines("https://cida.usgs.gov/thredds/dodsC/loca_future.html")
LL = L[grepl("tasmax", L)]
LLL = LL[grepl('checkbox', LL)]
yy = gsub('.{1}$', '', gsub("get_dods_tasmax_", "", gsub('.*(get_dods_tasmax_)', "\\1", LLL)))

t1 = do.call(rbind, strsplit(yy, "_r"))
t1[,2] = paste0("r", t1[,2])
t1[,3] = paste0("r", t1[,3])
t1 = data.frame(t1)
colnames(t1) = c("model", "ensemble", "scenario")
t1$model = gsub("_", "-", t1$model)
loca = t1

## bcca
L = readLines("https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/future.html")
LL = L[grepl("tasmax", L)]
LLL = LL[grepl('checkbox', LL)]
yy = unlist(regmatches(LLL,regexec('tasmax_day_(.*?)\\"',LLL)))
fin = yy[!grepl('tasmax', yy)]

t1 = do.call(rbind, strsplit(fin, "_r"))
t2 = data.frame(t1)
colnames(t2) = c("model", "scenario", "ensemble")
t2$scenario = paste0("r", t2$scenario)
t2$ensemble = paste0("r", t2$ensemble)
bcca = t2

## MACA
L = readLines("http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_aggregated_macav2_catalog.html")
LL = L[grepl("pr", L)]
LLL = LL[grepl("2006-2099", LL)]
yy = unlist(regmatches(LLL,regexec("macav2metdata_pr_(.*?)_2006_2099_CONUS",LLL)))
fin = yy[!grepl('maca', yy)]

t1 = do.call(rbind, strsplit(fin, "_"))
colnames(t1) = c("model",  "ensemble", "scenario")

maca = data.frame(t1)

# CABCM
L = readLines("https://cida.usgs.gov/thredds/dodsC/CA-BCM-2014/future.html")
LL = L[grepl("pet", L)]
LLL = LL[grepl('checkbox', LL)]
yy = unlist(regmatches(LLL,regexec("get_dods_(.*?)_pet",LLL)))
fin = yy[!grepl('get_dods_', yy)]
fin = gsub("_Monthly", "", fin)
t1 = do.call(rbind, strsplit(fin, '(_)(?=[^_]+$)', perl=TRUE))
t2 = data.frame(cbind(t1[,1], do.call(rbind, strsplit(paste0(t1[,2]), "_"))), stringsAsFactors = FALSE)
colnames(t2) = c("model", "scenario")

cabcm = t2
  
  
model_meta = list(
  maca = maca,
  cabcm = cabcm,
  loca = loca,
  bcca = bcca
)


usethis::use_data(model_meta, overwrite = TRUE)
