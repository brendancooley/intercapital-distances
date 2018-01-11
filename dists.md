# Calculating Historical Intercapital Distances

*Last updated 10 January 2018*

Military power degrades with distance. Fighting a war on another continent is, for most militaries, more difficult than fighting at home. Studies of conflict frequently employ capital-to-capital distance (or some transformation of this metric) as one proxy for this loss of strength from power projection (see, for example, @Gartzke2011). Combined with [data on territorial contiguity](http://www.correlatesofwar.org/data-sets/direct-contiguity), these metrics can provide us with a picture of the geographic constraints facing countries contemplating war with one another.

Historical intercapital distance data proved difficult to find however. Most researchers appear to rely on [EuGene](http://eugenesoftware.org/welcome.asp) to generate this data. I wasn't able to track down any documentation on exactly how EuGene does this, however.^[Tips on where to find this documentation are more than welcome.] @Gleditsch2001 generated a *minimum* interstate distance dataset, but their data only covers the post-1875 period. For researchers using [Correlates of War](http://www.correlatesofwar.org/) data, distance data would ideally cover 1816 to the present.

It turns out that it's not too difficult to build intercapital distance data from scratch, however. All that is required is data on the *names* of capital cities for each state system member for every year between 1816 and the present. Paul Hensel's [ICOW Historical State Names dataset](http://www.paulhensel.org/icownames.html) provides this information. We can then use Google's [Geocoding API](https://developers.google.com/maps/documentation/geocoding/start) through the `ggmap` R package to get the coordinates of each historical capital, which can then be used to generate intercapital distance matrices. 

Here, I show how to conduct this exercise in R. A clean version of Hensel's historical capital dataset is available [here](https://github.com/brendancooley/intercapital-distances/blob/master/capitals.csv). I've included all of the data and software necessary to generate this data on [Github](https://github.com/brendancooley/intercapital-distances). Feel free to send along questions, comments, or suggestions for improvement to [bcooley@princeton.edu](mailto:bcooley@princeton.edu).



#### Capital City Coordinates

Start by loading up the packages we'll need for analysis:


```r
libs <- c('readr', 'dplyr', 'tidyr', 'ggmap', 'geosphere', 'leaflet', 'knitr', 'bibtex', 'knitcitations')
sapply(libs, require, character.only = TRUE)
```

```
##         readr         dplyr         tidyr         ggmap     geosphere 
##          TRUE          TRUE          TRUE          TRUE          TRUE 
##       leaflet         knitr        bibtex knitcitations 
##          TRUE          TRUE          TRUE          TRUE
```

```r
write.bib(libs)
bib <- read.bib('Rpackages.bib')
```

I started by cleaning up Hensel's data a bit in excel. Each capital city is listed as its own observation, along with the country, its COW code, and the first and last year the city served as a capital. Because the Hensel data is coded annually, I take the country's capital at the start of any given year to be its capital for that entire year. For countries that no longer exist (e.g. Mecklenburg-Schwerin) I provide a contemporary alternative country name (aName) to help Google locate the city's coordinates. The assumption underlying this procedure is that the cities that served as capitals historically have not moved from their historical location (if I'm missing any instances where this occured please let me know). We can take a look at the data below:


```r
# load data
dists <- read_csv('capitals.csv')

# Hensel's data run from 1800-2016, set bounds
dists$startDate <- ifelse(is.na(dists$startDate), 1800, dists$startDate)
dists$endDate <- ifelse(is.na(dists$endDate), 2016, dists$endDate)

head(dists)
```

```
## # A tibble: 6 x 6
##   ccode Name                     aName startDate endDate Capital        
##   <int> <chr>                    <chr>     <dbl>   <dbl> <chr>          
## 1     2 United States of America <NA>       1800    2016 Washington D.C.
## 2    20 Canada                   <NA>       1800    1841 Ottawa         
## 3    20 Canada                   <NA>       1841    1843 Kingston       
## 4    20 Canada                   <NA>       1843    1849 Montreal       
## 5    20 Canada                   <NA>       1849    1859 Toronto        
## 6    20 Canada                   <NA>       1859    1865 Quebec City
```

Now we need to get the cities' coordinates. I simply feed the City, Country tuples to `ggmap`'s `geocode` function, which returns lat, lng coordinates if it can find a match in Google's database. If the country has a contemporary name, I use this name for the search in lieu of its old name. I save these coordinates to the `dists` data frame when they are found. The `geocode` API imposes some query limits, so it sometimes throws an `OVER_QUERY_LIMIT` error. If cities remain uncoded, I simply run the loop again, skipping over those that already have coordinates. We can check that the coding worked with the `dists %>% filter(is.na(lat))`, which should return an empty data frame if all cities have been geocoded.


```r
# initialize coordinates
dists$lat <- NA
dists$lng <- NA

# run until all cities have been coded
while(nrow(dists %>% filter(is.na(lat))) > 0) {
  # for each city
  for (i in 1:nrow(dists)) {
    # skip if it's already been coded
    if (is.na(dists[i,]$lat)) {
      if (is.na(dists[i,]$aName)) {
        # query City, Country for each capital
        query <- paste0(dists[i, ]$Capital, ", ", dists[i, ]$Name)
        latlng <- geocode(query)
        dists[i, ]$lat <- latlng$lat
        dists[i, ]$lng <- latlng$lon
      }
      else {
        # use alternative country name
        query <- paste0(dists[i, ]$Capital, ", ", dists[i, ]$aName)
        latlng <- geocode(query)
        dists[i, ]$lat <- latlng$lat
        dists[i, ]$lng <- latlng$lon
      }
    }
  }
}
```

We can check to make sure the cities were coded correctly by plotting them on a map. Clicking on the city will show a popup with its name. I poked around this map a bit and everything seemed to land in the right spot. 


```r
leaflet(data=dists) %>% addProviderTiles(providers$CartoDB.Positron, options=providerTileOptions(minZoom = 1)) %>%
  addCircleMarkers(~lng, ~lat, popup=~Capital, radius=1) %>%
  setMaxBounds(-180, -90, 180, 90)
```

<!--html_preserve--><div id="htmlwidget-35be7245b752daad9e94" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-35be7245b752daad9e94">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["CartoDB.Positron",null,null,{"errorTileUrl":"","noWrap":false,"zIndex":null,"unloadInvisibleTiles":null,"updateWhenIdle":null,"detectRetina":false,"reuseTiles":false,"minZoom":1}]},{"method":"addCircleMarkers","args":[[38.9071923,45.4215296,44.2311717,45.5016889,43.653226,46.8138783,45.4215296,25.0479835,23.1135925,18.594395,18.4860575,18.0178743,10.6549013,13.0968511,15.3091676,12.0560975,14.0101094,13.1600249,17.1274104,17.3026058,19.4326077,17.5045661,17.2510114,14.6349149,14.0722751,13.6929403,12.1149926,9.9280694,8.9823792,4.7109886,10.4805937,6.8012793,5.8520355,-0.1806532,-12.0463731,-22.9068467,-15.7942287,-19.0195852,-25.2637399,-33.4488897,-34.6036844,-34.9011127,51.5073509,53.3498053,52.3702157,50.8503463,49.611621,48.856614,46.131859,48.856614,43.7384176,47.1410303,46.9479739,39.4699075,40.4167754,42.5063174,-22.9093236,38.7222524,52.3758916,48.1351253,52.5200066,50.73743,52.5196077,49.0068901,51.0504088,48.7758459,51.3127114,49.8728253,53.3244914,53.6355022,52.2296756,48.210496,48.2081743,47.497912,50.0755381,50.0755381,48.1485965,41.9027835,45.070312,43.7695604,41.9027835,41.9027835,40.8517746,43.9355907,44.647128,44.801485,43.7695604,35.8989085,41.3275459,40.4660668,41.3275459,42.3930959,42.4304196,41.9973462,45.8150108,44.786568,43.8562586,42.6629138,46.0569465,37.9838096,37.5673173,37.9838096,35.1855659,42.6977082,47.0104529,44.4267674,59.9342802,55.755826,59.4369608,56.9496487,54.8985207,54.6871555,50.4501,53.9045398,40.1791857,41.7151377,40.4092617,60.4518126,60.1698557,59.3293235,59.9138688,55.6760968,64.1265206,14.93305,0.3301924,11.7431904,11.8816553,3.7504118,13.4548761,12.6392316,14.716677,6.4968574,18.0735299,13.5115963,6.8276228,5.3599517,9.6411855,12.3714277,6.2907432,8.4656765,5.6037168,6.1724969,3.8480325,6.5243793,9.0764785,0.4161976,4.3946735,12.1348457,-4.2633597,-4.4419311,0.3475964,-1.2920659,-6.162959,-6.165917,-3.361378,-1.9705786,2.0469343,11.5720765,8.9806034,15.3228767,-8.8399876,-25.891968,-15.3875259,-17.8251657,-13.9626121,-25.7478676,-22.5608807,-29.3632188,-24.6282079,-26.3054482,-18.8791902,-11.7172157,-20.1608912,-4.619143,33.9715904,36.753768,36.8064948,32.8872094,15.5006544,4.859363,35.6891975,39.9333635,33.3128057,30.0444196,33.5138073,33.8937913,31.9453666,31.768319,24.7135517,15.3694451,15.3694451,12.7854969,29.375859,26.2285161,25.2854473,24.453884,23.58589,34.5553494,37.9600766,38.5597722,42.8746212,41.2994958,43.2220146,51.1605227,39.9041999,47.8863988,25.0329694,37.566535,39.0392193,37.566535,35.0116363,35.6894875,22.572646,28.6139391,27.4727924,25.0700428,33.5651107,33.6600365,23.810332,16.8660694,19.7413519,6.9270786,4.1754959,27.7172453,13.7563309,11.5448729,17.9757058,21.0277644,10.8230989,3.139003,1.352083,4.9030522,14.6760413,14.5995124,-6.17511,-8.5568557,-37.8136276,-35.2809368,-9.4780123,-35.2623103,-36.8484597,-41.2864603,-17.7332512,-9.4456381,1.433333,-8.5211471,-18.1248086,-21.1393418,-0.5466857,7.1164214,7.3410628,6.9147118,-13.8506958],[-77.0368707,-75.6971931,-76.4859544,-73.567256,-79.3831843,-71.2079809,-75.6971931,-77.355413,-82.3665956,-72.3074326,-69.9312117,-76.8099041,-61.5019256,-59.6144819,-61.3793554,-61.7487996,-60.9874687,-61.2248157,-61.846772,-62.7176924,-99.133208,-88.1962133,-88.7590201,-90.5068824,-87.192136,-89.2181911,-86.2361744,-84.0907246,-79.5198696,-74.072092,-66.9036063,-58.1551255,-55.2038278,-78.4678382,-77.042754,-43.1728965,-47.8821658,-65.2619615,-57.575926,-70.6692655,-58.3815591,-56.1645314,-0.1277583,-6.2603097,4.8951679,4.3517211,6.1319346,2.3522219,3.425488,2.3522219,7.4246158,9.5209277,7.4474468,-0.3762881,-3.7037902,1.5218355,-43.1690483,-9.1393366,9.7320104,11.5819805,13.404954,7.0982068,13.4027331,8.4036527,13.7372621,9.1829321,9.4797461,8.6511929,11.4969238,11.4012499,21.0122287,16.3631031,16.3738189,19.040235,14.4378005,14.4378005,17.1077478,12.4963655,7.6868565,11.2558136,12.4963655,12.4963655,14.2681244,12.4472806,10.9252269,10.3279036,11.2558136,14.5145528,19.8186982,19.491356,19.8186982,18.9115964,19.2593642,21.4279956,15.9819189,20.4489216,18.4130763,21.1655028,14.5057515,23.7275388,22.8015531,23.7275388,33.3822764,23.3218675,28.8638102,26.1025384,30.3350986,37.6172999,24.7535747,24.1051865,23.9035965,25.2796514,30.5234,27.5615244,44.4991029,44.827096,49.8670924,22.2666302,24.9383791,18.0685808,10.7522454,12.5683372,-21.8174392,-23.5133267,6.733343,-14.2134347,-15.6177942,8.7371039,-16.5790323,-8.0028892,-17.4676861,2.6288523,-15.9582372,2.1253854,-5.2893433,-4.0082563,-13.5784012,-1.5196603,-10.7605239,-13.2317225,-0.1869644,1.2313618,11.5020752,3.3792057,7.398574,9.4672676,18.5581899,15.0557415,15.2428853,15.2662931,32.5825197,36.8219462,35.7516069,39.202641,29.3598782,30.1044288,45.3181623,43.1456475,38.7577605,38.9250517,13.2894368,32.6051351,28.3228165,31.03351,33.7741195,28.2292712,17.0657549,27.5143603,25.9231471,31.1366715,47.5079055,43.2473146,57.5012222,55.4513149,-6.8498129,3.0587561,10.1815316,13.1913383,32.5598994,31.57125,51.3889736,32.8597419,44.3614875,31.2357116,36.2765279,35.5017767,35.9283716,35.21371,46.6752957,44.1910066,44.1910066,45.0186548,47.9774052,50.5860497,51.5310398,54.3773438,58.4059227,69.207486,58.3260629,68.7870384,74.5697617,69.2400734,76.8512485,71.4703558,116.4073963,106.9057439,121.5654177,126.9779692,125.7625241,126.9779692,135.7680294,139.6917064,88.363895,77.2090212,89.6392863,67.2847875,73.0169135,73.2293542,90.4125181,96.195132,96.2004383,79.861243,73.5093474,85.3239605,100.5017651,104.8921668,102.6331035,105.8341598,106.6296638,101.686855,103.819836,114.939821,121.0437003,120.9842195,106.8650395,125.5603143,144.9630576,149.1300092,147.1506542,174.1223544,174.7633315,174.776236,168.3273245,159.9728999,173,179.1961926,178.4500789,-175.204947,166.9210913,171.1857736,134.4771596,158.1610274,-171.7513551],1,null,null,{"lineCap":null,"lineJoin":null,"clickable":true,"pointerEvents":null,"className":"","stroke":true,"color":"#03F","weight":5,"opacity":0.5,"fill":true,"fillColor":"#03F","fillOpacity":0.2,"dashArray":null},null,null,["Washington D.C.","Ottawa","Kingston","Montreal","Toronto","Quebec City","Ottawa","Nassau","Havana","Port-au-Prince","Santo Domingo","Kingston","Port-of-Spain","Bridgetown","Roseau","St. George's","Castries","Kingstown","St. John's","Basseterre","Mexico City","Belize City","Belmopan","Guatemala City","Tegucigalpa","San Salvador","Managua","San Jose","Panama City","Bogota","Caracas","Georgetown","Paramaribo","Quito","Lima","Rio de Janeiro","Brasilia","Sucre","Asuncion","Santiago","Buenos Aires","Montevideo","London","Dublin","Amsterdam","Brussels","Luxembourg City","Paris","Vichy","Paris","Monaco","Vaduz","Bern","Valencia","Madrid","Andorra la Vella","Rio de Janeiro","Lisbon","Hannover","Munich","Berlin","Bonn","East Berlin","Karlsruhe","Dresden","Stuttgart","Kassel","Darmstadt","Ludwigslust","Schwerin","Warsaw","Vienna","Vienna","Budapest","Prague","Prague","Bratislava","Rome","Torino","Florence","Rome","Rome","Naples","San Marino","Modena","Parma","Florence","Valletta","Tirana","Vlore","Tirana","Cetinje","Podgorica","Skopje","Zagreb","Belgrade","Sarajevo","Pristina","Ljubljana","Athens","Nafplion","Athens","Nicosia","Sofia","Chisinau","Bucharest","Saint Petersburg","Moscow","Tallinn","Riga","Kaunas","Vilnius","Kiev","Minsk","Yerevan","Tbilisi","Baku","Turku","Helsinki","Stockholm","Oslo","Copenhagen","Reykjavik","Praia","Sao Tome","Boe","Bissau","Malabo","Banjul","Bamako","Dakar","Porto-Novo","Nouakchott","Niamey","Yamoussoukro","Abidjan","Conakry","Ouagadougou","Monrovia","Freetown","Accra","Lome","Yaounde","Lagos","Abuja","Libreville","Bangui","N'Djamena","Brazzaville","Kinshasa","Kampala","Nairobi","Dodoma","Zanzibar City","Bujumbura","Kigali","Mogadishu","Djibouti City","Addis Ababa","Asmara","Luanda","Maputo","Lusaka","Harare","Lilongwe","Pretoria","Windhoek","Maseru","Gaborone","Mbabane","Antananarivo","Moroni","Port Louis","Victoria","Rabat","Algiers","Tunis","Tripoli","Khartoum","Juba","Tehran","Ankara","Baghdad","Cairo","Damascus","Beirut","Amman","Jerusalem","Riyadh","Sanaa","Sanaa","Aden","Kuwait City","Manama","Doha","Abu Dhabi","Muscat","Kabul","Ashgabat","Dushanbe","Bishkek","Tashkent","Almaty","Astana","Beijing","Ulaanbaatar","Taipei","Seoul","Pyongyang","Seoul","Kyoto","Tokyo","Calcutta","New Delhi","Thimphu","Karachi","Rawalpindi","Islamabad","Dhaka","Rangoon","Pyinmana","Colombo","Male","Kathmandu","Bangkok","Phnom Penh","Vientiane","Hanoi","Saigon","Kuala Lumpur","Singapore","Bandar Seri Begawan","Quezon City","Manila","Jakarta","Dili","Melbourne","Canberra","Port Moresby","Russell","Auckland","Wellington","Port Vila","Honiara","South Tarawa","Funafuti","Suva","Nuku'alofa","Yaren","Majuro","Koror","Palikir","Apia"],null,null,null,null]},{"method":"setMaxBounds","args":[-90,-180,90,180]}],"limits":{"lat":[-41.2864603,64.1265206],"lng":[-175.204947,179.1961926]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

I then export the geocoded capitals as a csv. You can find these data [here](https://github.com/brendancooley/intercapital-distances/blob/master/dists.csv).


```r
write_csv(dists, 'dists.csv')
```

#### Intercapital Distances

Remember that the point of all this was to generate intercapital distance data for all countries in the COW data. To do this, we want to convert our geocoded capital city data into a dyadic time series that gives the distance between any two countries' capitals for a given year. If there are $N$ countries in the system in a given year $t$, we want to be able to generate an $N \times N$ matrix for that year where each entry is the distance between countries $i$ and $j$.

I start by loading up COW's state system membership data, so we know which countries were members of the system for each year. The field "styear" denotes the year the country entered the system, and the field "endyear" gives the year it exited. I then append this data to the distance data. The output is shown below.


```r
dists <- read_csv('dists.csv')

# get state system membership (COW)
sysMemUrl <- "http://www.correlatesofwar.org/data-sets/state-system-membership/states2016/at_download/file"

sysMem <- read_csv(sysMemUrl) %>% select(ccode, styear, endyear)
dists <- left_join(dists, sysMem, by="ccode")

head(dists)
```

```
## # A tibble: 6 x 10
##   ccode Name         aName startD… endDa… Capital    lat   lng stye… endy…
##   <int> <chr>        <chr>   <int>  <int> <chr>    <dbl> <dbl> <int> <int>
## 1     2 United Stat… <NA>     1800   2016 Washing…  38.9 -77.0  1816  2016
## 2    20 Canada       <NA>     1800   1841 Ottawa    45.4 -75.7  1920  2016
## 3    20 Canada       <NA>     1841   1843 Kingston  44.2 -76.5  1920  2016
## 4    20 Canada       <NA>     1843   1849 Montreal  45.5 -73.6  1920  2016
## 5    20 Canada       <NA>     1849   1859 Toronto   43.7 -79.4  1920  2016
## 6    20 Canada       <NA>     1859   1865 Quebec …  46.8 -71.2  1920  2016
```

From this dataframe, I can build the $N \times N$ intercapital distance matrix for any year. I wrap this in a function `coord2DistM`, which takes the desired year and the dists dataframe as arguments. It filters the distance data to include only states that were active in that year. It then feeds the latitude and longitude coordinates to the `distm` function from the `geosphere` package, which retuns the desired distance matrix. I convert all distances to kilometers.

The function `distM2dydist` takes this distance matrix and a pair of COW country codes and returns the dyadic distance. Below, I show how to use these functions to build the intercapital distance matrix for the year 1816 and get the distance between Britain and Saxony.


```r
# Note: assigns capital to city that was capital at beginning of year

coord2DistM <- function(dists, year) {
  # filter by system membership, then relevant capital
  distsY <- dists %>% filter(styear <= year, endyear >= year) %>% filter(startDate < year & endDate >= year)
  # check that one capital returned per country
  if (length(unique(distsY$Capital)) != length(unique(distsY$Name))) {
    print('error: nCountries != nCapitals, check underlying coordinate data')
  }
  else {
    # get distance matrix for selected year
    latlng <- distsY %>% select('lng', 'lat') %>% as.matrix()
    distsYmatrix <- distm(latlng, latlng, fun=distVincentySphere)
    distsYmatrix <- distsYmatrix / 1000  # convert to km
    rownames(distsYmatrix) <- colnames(distsYmatrix) <- distsY$ccode
    return(distsYmatrix)
  }
}

# get distance between i and j
distM2dydist <- function(distM, ccode1, ccode2) {
  return(distM[ccode1, ccode2])
}

# application
year <- 1816
Britain <- "200"
Saxony <- "269"

distM1816 <- coord2DistM(dists, year)
distM2dydist(distM1816, Britain, Saxony)
```

```
## [1] 965.3628
```

These functions can be used in tandem to grab intercapital distances for arbitrary year, dyad pairings.

#### Merge with COW War Data

Conflict researchers often want this data to analyze wars. Now I show how to merge intercapital distance data with COW's inter-state-war data. I clean up the COW war data a bit, which you can see below post-cleaning. In the most basic leve, the data give information about the belligerents in every war and how long each war lasted.


```r
# append to COW wars data
warUrl <- "http://www.correlatesofwar.org/data-sets/COW-war/inter-state-war-data/at_download/file"
cowWars <- read_csv(warUrl)

# for simplicity, ignore armistices
cowWars$StartYear <- cowWars$StartYear1
cowWars$EndYear <- ifelse(cowWars$EndYear2 == -8, cowWars$EndYear1, cowWars$EndYear2)
cowWars <- cowWars %>% select(WarName, ccode, StateName, Side, StartYear, EndYear)

head(cowWars)
```

```
## # A tibble: 6 x 6
##   WarName             ccode StateName                 Side StartYear EndY…
##   <chr>               <int> <chr>                    <int>     <int> <int>
## 1 Franco-Spanish War    230 Spain                        2      1823  1823
## 2 Franco-Spanish War    220 France                       1      1823  1823
## 3 First Russo-Turkish   640 Ottoman Empire               2      1828  1829
## 4 First Russo-Turkish   365 Russia                       1      1828  1829
## 5 Mexican-American       70 Mexico                       2      1846  1847
## 6 Mexican-American        2 United States of America     1      1846  1847
```

We want to know the intercapital distance between each pair of belligerents between 1816 and the present. We could use the `coord2DistM` and `distM2dydist` but this would require calculating the intercapital distance matrix for every year in which there was a war. A simpler solution is to build a dataframe of capital-years, along with their coordinates, merge this with the war data, and calculate the distance between belligerents, given their capitals' coordinates.

I used the procedure in this [stackoverflow post](https://stackoverflow.com/questions/28553762/expand-year-range-in-r) to create the capital-year dataframe. Once merged, I use `geosphere`'s `distVicentySphere` function to calculate the distances. The resulting data is shown below the code.


```r
# convert capital data to yearly observations
# https://stackoverflow.com/questions/28553762/expand-year-range-in-r
distsYear <- dists
distsYear$year <- mapply(seq, distsYear$startDate, distsYear$endDate, SIMPLIFY=FALSE)
distsYear <- distsYear %>% 
  unnest(year) %>% 
  select(ccode, year, lat, lng)

# get capital in start year
cowWars$year <- cowWars$StartYear

# append coords for each side
cowWars1 <- cowWars %>% filter(Side == 1) %>% left_join(distsYear, by=c("ccode", "year")) %>% rename(State1 = StateName, ccode1 = ccode, lat1 = lat, lng1 = lng) %>% select(-Side)
cowWars2 <- cowWars %>% filter(Side == 2) %>% left_join(distsYear, by=c("ccode", "year")) %>% rename(State2 = StateName, ccode2 = ccode, lat2 = lat, lng2 = lng) %>% select(WarName, State2, ccode2, lat2, lng2)

cowWarsDyadic <- left_join(cowWars1, cowWars2, by="WarName") %>% select(WarName, ccode1, State1, ccode2, State2, year, lng1, lat1, lng2, lat2)

# calculate distance
latlng1 <- cowWarsDyadic %>% select(lng1, lat1)
latlng2 <- cowWarsDyadic %>% select(lng2, lat2)
cowWarsDyadic$distance <- distVincentySphere(latlng1, latlng2) / 1000  # convert to km

# export data
write_csv(cowWarsDyadic, 'cowWarsDist.csv')

cowWarsDyadic %>% select(WarName, ccode1, State1, ccode2, State2, distance)
```

```
## # A tibble: 813 x 6
##    WarName                  ccode1 State1          ccode2 State2    dista…
##    <chr>                     <int> <chr>            <int> <chr>      <dbl>
##  1 Franco-Spanish War          220 France             230 Spain       1054
##  2 Franco-Spanish War          220 France             230 Spain       1054
##  3 First Russo-Turkish         365 Russia             640 Ottoman …   2233
##  4 Mexican-American              2 United States …     70 Mexico      3035
##  5 Austro-Sardinian            300 Austria            337 Tuscany      632
##  6 Austro-Sardinian            300 Austria            325 Italy        765
##  7 Austro-Sardinian            300 Austria            332 Modena       575
##  8 First Schleswig-Holstein    255 Prussia            390 Denmark      356
##  9 First Schleswig-Holstein    255 Prussia            390 Denmark      356
## 10 First Schleswig-Holstein    255 Prussia            390 Denmark      356
## # ... with 803 more rows
```

The resulting data can be found [here](https://github.com/brendancooley/intercapital-distances/blob/master/cowWarsDist.csv). 

#### R Packages

Wickham H, Hester J and Francois R (2017). _readr: Read Rectangular Text Data_. R package
version 1.1.1, <URL: https://CRAN.R-project.org/package=readr>.

Wickham H, Francois R, Henry L and Müller K (2017). _dplyr: A Grammar of Data Manipulation_. R
package version 0.7.4, <URL: https://CRAN.R-project.org/package=dplyr>.

Wickham H and Henry L (2017). _tidyr: Easily Tidy Data with 'spread()' and 'gather()'
Functions_. R package version 0.7.2, <URL: https://CRAN.R-project.org/package=tidyr>.

Kahle D and Wickham H (2013). “ggmap: Spatial Visualization with ggplot2.” _The R Journal_,
*5*(1), pp. 144-161. <URL: http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf>.

Hijmans RJ (2016). _geosphere: Spherical Trigonometry_. R package version 1.5-5, <URL:
https://CRAN.R-project.org/package=geosphere>.

Cheng J, Karambelkar B and Xie Y (2017). _leaflet: Create Interactive Web Maps with the
JavaScript 'Leaflet' Library_. R package version 1.1.0, <URL:
https://CRAN.R-project.org/package=leaflet>.

Xie Y (2017). _knitr: A General-Purpose Package for Dynamic Report Generation in R_. R package
version 1.18, <URL: https://yihui.name/knitr/>.

Xie Y (2015). _Dynamic Documents with R and knitr_, 2nd edition. Chapman and Hall/CRC, Boca
Raton, Florida. ISBN 978-1498716963, <URL: https://yihui.name/knitr/>.

Xie Y (2014). “knitr: A Comprehensive Tool for Reproducible Research in R.” In Stodden V,
Leisch F and Peng RD (eds.), _Implementing Reproducible Computational Research_. Chapman and
Hall/CRC. ISBN 978-1466561595, <URL: http://www.crcpress.com/product/isbn/9781466561595>.

Francois R (2017). _bibtex: Bibtex Parser_. R package version 0.4.2, <URL:
https://CRAN.R-project.org/package=bibtex>.

Boettiger C (2017). _knitcitations: Citations for 'Knitr' Markdown Files_. R package version
1.0.8, <URL: https://CRAN.R-project.org/package=knitcitations>.

#### References
