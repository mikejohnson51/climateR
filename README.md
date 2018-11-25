# climateR <img src="man/figures/logo.png" width=230 align="right" />

*** IN DVELOPMENT: FOR TIME BEING FUNCTION CAN COME, GO and CHANGE ***

Get point and gridded Climate Data in R from multiple sources. 

1. gridMET
2. daymet
3. TopoWX
4. PRISM
5. MACA
6. LOCA

The climateR package works of the AOI framework established in the AOI R package. This framework is also used in HydroData, FloodMapper, NWM. 

To get a climate product, an area of interest must be defined:

```r
AOI = getAOI(list("UCSB", 100, 100))
```
<img src="man/figures/logo.png" width="100">

Here we are loading a spatial polygon for the state of California. More examples of contruting AOI calls can be found here.

With an AOI, we can construct a call to aa dataset of choice for a parameter(s) and date(s) or choice:

```r
p = getPRISM(AOI, param = c('tmax','tmin'), startDate = "2018-10-29")
```
