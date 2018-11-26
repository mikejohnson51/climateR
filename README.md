# climateR <img src="man/figures/logo.png" width=230 align="right" />

#### IN DEVELOPMENT: FOR THE TIME BEING FUNCTIONS CAN COME, GO and CHANGE
#### Likley will not work yet on Window machines due to OPeNDAP NETCDF issues

Get point and gridded Climate Data in R from multiple sources. 

|**Number**|**Dataset**   | **Description**                                            | **Dates**         |
|----------|--------------| -----------------------------------------------------------|-------------------|
|1         | **GridMET**  | Gridded Meteorological Data.                               | 1979-Yesterday    |
|2         | **Daymet**   | Daily Surface Weather and Climatological Summaries         | 1980-2017.        |
|3         | **TopoWX**   | Topoclimatic Daily Air Temperature Dataset                 | 1948-2016.        |
|4         | **PRISM**    | Parameter-elevation Regressions on Independent Slopes      | 1981-(Yesterday-1)|
|5         | **MACA**     | Multivariate Adaptive Constructed Analogs                  | 1950-2099         |
|6         | **LOCA**     | Localized Constructed Analogs                              | 1950-2100         |
|7         | **BCCA**     | Bias Corrected Constructed Analogs                         | 1950-2100         |
|8         | **NCEP**     | National Centers for Ecological Prediction.                | **In progress**   |
|9         | **SARRD**    | Statistical Asynchronous Regional Regression               | 1960-2100         |
|10        | **BCSD**     | Bias Corrected Spatially Downscaled VIC: Monthly Hydrology | 1950-2099         |
|11        | **OSSEB**    | Operational Simplified Surface Energy Balance              | 2000-2015         |

Operational Simplified Surface Energy Balance

The climateR package works off the [AOI](https://github.com/mikejohnson51/AOI) framework established in the AOI R package. This framework is also used in [HydroData](https://github.com/mikejohnson51/HydroData), and [NWM](https://github.com/mikejohnson51/nwm). 

To get a climate product, an area of interest must be defined:

```r
AOI = getAOI(state = "CA")
```
<p align="center">
<img src="man/figures/ca_AOI.png" width="400">
</p>

Here we are loading a spatial polygon for the state of California. More examples of contruting AOI calls can be found [here](https://mikejohnson51.github.io/AOI/).

With an AOI, we can construct a call to a dataset for a parameter(s) and date(s) of choice. Here we are querting the PRISM dataset for maximum and mimimun temperture estimates for October 29, 2018:

```r
system.time({
p = getPRISM(AOI, param = c('tmax','tmin'), startDate = "2018-10-29")
})

>  user  system elapsed 
  0.057   1.054   4.845 
```

```r
r = raster::stack(p$tmax, p$tmin)
names(r) = c('tmax', 'tmin')
rasterVis::levelplot(r)
```
<p align="center">
<img src="man/figures/prism_ex.png" width="600">
</p>

Some sources are  downscaled Global Climate Models (GCMs). These allow you to query future forecasted ensemble members. One example is from the MACA dataset:

```r
system.time({
m = getMACA(AOI, 
            model = "CCSM4", 
            param = 'prcp', 
            scenario = c('rcp45', 'rcp85'), 
            startDate = "2080-06-29", 
            endDate = "2080-06-30")
})

>  user  system elapsed 
  0.183   1.213   5.998
```

```r
r = raster::stack(m$prcp_rcp45, m$prcp_rcp85)
names(r) = paste0(c(rep("RCP45_", 2), rep("RCP85_", 2)), unique(substring(names(r), 2, 11)))
rasterVis::levelplot(r, par.settings = rasterTheme(region=sequential_hcl(10, power=2.2)))
```
<p align="center">
<img src="man/figures/scenario_ex.png" width="600">
</p>

Large scale data grabs are also quite efficient

```r
system.time({
  g = getAOI(state = "conus") %>%  getGridMET(param = 'srad', startDate = "2017-06-29")
})

>   user  system elapsed 
  0.304   1.618   6.063 
```

```r
raster::plot(g$srad, col = viridis::viridis(100), axes = F, box= F)
title(main = "Solar Radiation 2017-06-29\n4km Resolution")
sp::plot(g$AOI, add = T)
```

<p align="center">
<img src="man/figures/conus_ex.png" width="600">
</p>

Getting ensemble averages is also quite simple:

```r

models = c('ccsm', 'cnrm', 'csiro', 'hadgem', 'pcm')

system.time({
  temp = getAOI(state = "conus") %>% getSARRD(param = 'tmin', model = models, ensemble = 'a2', startDate = "2080-11-29")
})

> user  system elapsed 
0.554   1.798  36.724
```

```r
#stack model outputs
s = stack(temp)

# add mean to stack
s = addLayer(s, mean(s))
names(s) = c(models, "Ensemble")

# Plot
levelplot(s, par.settings = RdBuTheme)
```
<p align="center">
<img src="man/figures/ensemble_ex.png" width="600">
</p>

Statistics are not limited to mean: 

```r 
stats= stack(max(s), min(s), mean(s), max(s) - min(s), calc(s, sd), sum((s - mean(s))^2) / 5)
names(stats) = c("Ensemble Max", "Ensemble Min", "Ensemble Mean",  "Ensemble Range",  "Ensemble SD",  "Ensemble Variance")
```
<p align="center">
<img src="man/figures/stats_ex.png" width="600">
</p>

