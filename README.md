# rtraveltime
R bindings to the traveltimeplatform.com isochrone API

A very rudimetary package to generate travel time isochrones.

To use the isochrone service you have to [obtain API keys from TravelTimePlatform](http://docs.traveltimeplatform.com/overview/introduction) and
specify them as 

```
options("rtraveltime.id"="<your API id>")
options("rtraveltime.key"="<your API key>")
```

The example vignette requires `rmapzem` for the vector data.

## Stacked isochrones cycling from UBC
![Vancouver cycling](images/cycling_ubc.png)

## 45 minute cycling ranges from UCB vs SFU
![Vancouver cycling](images/cycling_example.png)
