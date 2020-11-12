
# Chill projections for South America

This repository contains the code and part of the data generated to
assess the impacts of climate change in South America. After projecting
future temperatures, we computed chill accumulation at each location.

In earlier steps, we identified all available weather stations in South
America. Information regarding these weather stations was obtained from
the Global Surface Summary Of the Day database
([GSOD](https://data.noaa.gov/dataset/dataset/global-surface-summary-of-the-day-gsod))
by accessing functions contained in the `chillR` package (Luedeling
2020). This procedure resulted in about 1,500 available weather
stations.

<a><img src='figures/SA_1500_stations.png' align="center" height="450" /></a>

To keep using those weather stations having a good amount of data, we
filtered the weather stations with 90% of complete records between 1980
and 2017. Additionally, we added a number of weather stations from
different databases (i.e. a Chilean - through the `dormancyR` package
(Fernandez C 2020) - and Argentinean database). This addition yielded
about 157 weather stations.

<a><img src='figures/WS_90_complete_GSOD_CR_AR.png' align="center" height="450" /></a>

The next task is to interpolate chill accumulation between stations and
produce a raster map. However, we need to identify reliable and
plausible proxies to be used in the interpolation procedure.

# References

<div id="refs" class="references hanging-indent">

<div id="ref-R-dormancyR">

Fernandez C, Eduardo. 2020. *DormancyR: Functions to Compute Chill
Metrics*. <https://github.com/EduardoFernandezC/dormancyR>.

</div>

<div id="ref-R-chillR">

Luedeling, Eike. 2020. *ChillR: Statistical Methods for Phenology
Analysis in Temperate Fruit Trees*.
<https://CRAN.R-project.org/package=chillR>.

</div>

</div>
