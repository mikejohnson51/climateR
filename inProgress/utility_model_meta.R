
L = readLines("https://cida.usgs.gov/thredds/dodsC/loca_future.html")

head(LLL, 10)
LL = L[grepl("tasmax", L)]
LLL = LL[grepl('checkbox', LL)]

head(LLL)

sub('(?<=\\get_dods_tasmax_).*$', '', LLL)

gsub("Avenue.*$", "", a)

gsub("get_dods_tasmax_.*$", "", LLL)

yy = gsub('.{1}$', '', gsub("get_dods_tasmax_", "", gsub('.*(get_dods_tasmax_)', "\\1", LLL)))

t1 = do.call(rbind, strsplit(yy, "_r"))
t1[,2] = paste0("r", t1[,2])
t1[,3] = paste0("r", t1[,3])
t1 = data.frame(t1)
colnames(t1) = c("model", "ensemble", "scenario")

loca = t1



L = readLines("https://cida.usgs.gov/thredds/dodsC/BCSD_mon_VIC.html")
LL = L[grepl("baseflow", L)]
LLL = LL[grepl('checkbox', LL)]

pattern="get_dods_(.*?)_baseflow"

yy = unlist(regmatches(LLL,regexec("get_dods_(.*?)_baseflow",LLL)))
fin = yy[!grepl('get_dods_', yy)]

t1 = do.call(rbind, strsplit(fin, "_rcp"))
t2 = data.frame(cbind(t1[,1], do.call(rbind, strsplit(paste0("rcp",t1[,2]), "_"))), stringsAsFactors = FALSE)

colnames(t2) = c("model", "scenario", "ensemble")

bcsd = t2


model_meta = list(

  maca = data.frame(
      name = c("bcc-csm1-1",     "bcc-csm1-1-m",   "BNU-ESM",        "CanESM2",
               "CCSM4",          "CNRM-CM5",       "CSIRO-Mk3-6-0",  "GFDL-ESM2M",
               "GFDL-ESM2G",     "HadGEM2-ES",     "HadGEM2-CC",     "inmcm4",
               "IPSL-CM5A-LR",   "IPSL-CM5A-MR",   "IPSL-CM5B-LR",   "MIROC5",
               "MIROC-ESM",      "MIROC-ESM-CHEM", "MRI-CGCM3",   "NorESM1-M" ),

      country = c("China",          "China",          "China",          "Canada",
                  "USA",            "France",        "Australia",      "USA",
                  "USA",            "United Kingdom", "United Kingdom", "Russia",
                  "France",        "France",         "France",         "Japan",
                  "Japan",          "Japan",          "Japan",          "Norway" ),

      agency = c("Beijing Climate Center, China Meteorological Administration",
                 "Beijing Climate Center, China Meteorological Administration",
                 "College of Global Change and Earth System Science, Beijing Normal University, China",
                 "Canadian Centre for Climate Modeling and Analysis",
                 "National Center of Atmospheric Research, USA",
                 "National Centre of Meteorological Research, France",
                 "Commonwealth Scientific and Industrial Research Organization/Queensland Climate Change Centre of Excellence, Australia",
                 "NOAA Geophysical Fluid Dynamics Laboratory, USA",
                 "NOAA Geophysical Fluid Dynamics Laboratory, USA",
                 "Met Office Hadley Center, UK",
                 "Met Office Hadley Center, UK",
                 "Institute for Numerical Mathematics, Russia",
                 "Institut Pierre Simon Laplace, France",
                 "Institut Pierre Simon Laplace, France",
                 "Institut Pierre Simon Laplace, France",
                 "Atmosphere and Ocean Research Institute (The University of Tokyo),\n\t\tNational Institute for Environmental Studies,and Japan Agency for Marine-Earth Science and Technology",
                 "Japan Agency for Marine-Earth Science and Technology, Atmosphere and Ocean Research Institute (The University of Tokyo), and National Institute for Environmental Studies",
                 "Japan Agency for Marine-Earth Science and Technology, Atmosphere and Ocean Research Institute (The University of Tokyo), and National Institute for Environmental Studies",
                 "Meteorological Research Institute, Japan",
                 "Norwegian Climate Center, Norway"),

      AtmoRes_lonlat = c("2.8 deg x 2.8 deg",  "1.12 deg x 1.12 deg",
                         "2.8 deg x 2.8 deg",   "2.8 deg x 2.8 deg",
                         "1.25 deg x 0.94 deg", "1.4 deg x 1.4 deg",
                         "1.8 deg x 1.8 deg",   "2.5 deg x 2.0 deg",
                         "2.5 deg x 2.0 deg",   "1.88 deg x 1.25 deg",
                         "1.88 deg x 1.25 deg", "2.0 deg x 1.5 deg",
                         "3.75 deg x 1.8 deg",  "2.5 deg x 1.25 deg",
                         "2.75 deg x 1.8 deg",  "1.4 deg x 1.4 deg",
                         "2.8 deg x 2.8 deg",   "2.8 deg x 2.8 deg",
                         "1.1 deg x 1.1 deg",   "2.5 deg x 1.9 deg"),

      ensemble = c( "r1i1p1", "r1i1p1", "r1i1p1", "r1i1p1", "r6i1p1",
                    "r1i1p1", "r1i1p1", "r1i1p1", "r1i1p1", "r1i1p1",
                    "r1i1p1", "r1i1p1", "r1i1p1", "r1i1p1", "r1i1p1",
                    "r1i1p1", "r1i1p1", "r1i1p1", "r1i1p1", "r1i1p1"),
      stringsAsFactors = FALSE),



  sarrd = data.frame(

    model = c("ccsm",     "cgcm3_t47",   "cgcm3_t63",        "cnrm",
             "csiro",          "echam5",       "echo",  "gdfl_2-0",
             "gdfl_2-1",     "giss_aom",     "hadcm3",     "hadgem",
             "miroc_hi",   "miroc_med",   "mri_cgcm2",   "pcm"),

     ensemble_1 = c("a1b",  "a1b",   "a1b",   "a1b",
                    "a1b",  "a1b",   "a1b",   "a1b",
                    "a1b",  "a1b",   "a1b",   "a1b",
                    "a1b",  "a1b",   "a1b",   "a1b"),

    ensemble_2 = c("a1fi",  "",   "",   "",
                   "",  "",   "",   "",
                   "a1fi",  "",   "a1fi",   "",
                   "",  "",   "",   "a1fi"),

    ensemble_3 = c("a2",  "a2",   "a2",   "a2",
                   "a2",  "a2",   "a2",   "a2",
                   "a2",  "a2",   "a2",   "a2",
                   "",  "a2",   "a2",   "a2"),

    ensemble_4 = c("b1",  "b1",   "b1",   "b1",
                   "b1",  "b1",   "b1",   "b1",
                   "b1",  "b1",   "b1",   "",
                   "b1",  "b1",   "b1",   "b1"),


    stringsAsFactors = FALSE),

  bcsd = bcsd

  )

save(model_meta, file = "./data/model_meta.rda", compress = "xz")
