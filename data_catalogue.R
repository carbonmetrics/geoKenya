# set up =====================

## load libraries -----------

if(!require("pacman")){
  install.packages("pacman")
}

pacman::p_load(data.table,terra,sf,colorspace,basf,tmap,foreach,mapmisc,stringr,XML,httr)
pacman::p_load(dplyr) # install dplyr if not installed
pacman::p_unload(dplyr) # immediate unload to keep work environment tidy 

# set working directory - change this for your computer
setwd("/media/henk/henkharmsen/Documents/GIS/")

# projections
utm36S = "+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
epsg4326="+proj=longlat +datum=WGS84 +no_defs"

# colors
cols=colorspace::sequential_hcl(200,"Blues",rev=T)

# Plot figures ================

# Figure 1 - Kenya, administrative units ------------------

# load files for Kenya, downloaded from https://gadm.org/download_country_v3.html
x0=readRDS("gadm36_KEN_0_sf.rds")
x1=readRDS("gadm36_KEN_1_sf.rds")
x2=readRDS("gadm36_KEN_2_sf.rds")
x3=readRDS("gadm36_KEN_3_sf.rds")

# overview of admin units
pdf(file="admin_units.pdf")
par(mfrow=c(2,2),mai = c(0.01, 0.01, 0.01, 0.01))
plot(x0)
plot(x1)
plot(x2)
plot(x3)
par(mfrow=c(1,1))
dev.off()

# Figure 2 - population increase --------------------

# extract population from Counties according to Wikipedia from web
url="https://en.wikipedia.org/wiki/Counties_of_Kenya"
r=GET(url)
doc=readHTMLTable(
  doc=content(r, "text"))

counties=doc[[3]] %>% setDT
setnames(counties,c("code","county","former.province","area.km2","pop.2009","pop.2019","capital"))
counties=counties[-1,]
nums=c("area.km2","pop.2009","pop.2019")

# clean up county names - they must match the names in the sf object
counties[,(nums):=lapply(.SD,function(x) str_replace_all(x,",","")),.SDcols=nums]
counties[,(nums):=lapply(.SD,as.numeric),.SDcols=nums]
counties[county=="Nairobi (County)",county:="Nairobi"]
counties[county=="Mombasa (County)",county:="Mombasa"]
counties[county=="Taita–Taveta",county:="Taita Taveta"]
counties[county=="Trans-Nzoia",county:="Trans Nzoia"]

# further clean-up; calculate population difference as percentage 2009-2019
nums=c("pop.2009","pop.2019")
counties[,(nums):=lapply(.SD,function(x) x/1e6),.SDcols=nums]

counties[,increase.pop:=pop.2019-pop.2009]
counties[,increase.f:=(pop.2019/pop.2009)-1]
counties=counties[code!=""]

counties.x1=x1$NAME_1 %>% sort
counties.counties=counties[,as.character(county)] %>% sort

# test
compare=data.table(counties.x1,counties.counties)
compare[,check:=counties.x1==counties.counties,]

x1$county=x1$NAME_1
test=dplyr::inner_join(x1,counties)  # must use tidyverse as data.table and sf don't work together

# make the plot in tmap
t1=tm_shape(test)+
  tm_polygons(col="increase.f",midpoint=NA,palette=diverging_hcl(10,"Blue-Red",rev=T),title="pop.2019/pop.2009")
t2=tm_shape(test)+
  tm_polygons(col="pop.2009",palette="Blues",title="pop 2009 (millions)")
t3=tm_shape(test)+
  tm_polygons(col="pop.2019",palette="Blues",title="pop 2019 (millions)")

t4=tmap_arrange(t3,t1)
tmap_save(t4,filename="popKenya.pdf")
# system("pdfcrop popKenya.pdf popKenya.pdf") # in Linux you can use pdfcrop to reduce unnecessary margins

# Figure 3: water, places and roads --------------------

# download from http://download.geofabrik.de/africa/kenya.html
water=read_sf("kenya-latest-free.shp/gis_osm_water_a_free_1.shp")
waterways=read_sf("kenya-latest-free.shp/gis_osm_waterways_free_1.shp")
places=read_sf("kenya-latest-free.shp/gis_osm_places_free_1.shp")
roads=read_sf("kenya-latest-free.shp/gis_osm_roads_free_1.shp")

# plot to pdf
pdf(file="kenya_roads_water.pdf")
par(mfrow=c(2,2),mai = c(0.01, 0.01, 0.01, 0.01))
plot(water)
plot(x0,add=T)
plot(waterways)
plot(x0,add=T)
plot(places,pch=1,cex=0.5)
plot(x0,add=T)
plot(roads)
plot(x0,add=T)
par(mfrow=c(1,1))
dev.off()

# Figure 4: rainfall -------------------------------

# download from https://landportal.org/node/79264
pdf(file="rainfall.pdf")
plot(raster("ke_totann/ke_totann/w001001.adf"),col=cols,box=F,bty="n")
dev.off()

# Figure 5: protected areas -------------------------
# protected areas
pa=st_read("ke_protected-areas/ke_protected-areas.shp")

# plot to pdf
pdf(file="protected_areas.pdf")
plot(x0,add=F)
plot(pa,col="green",add=T)
scaleBar(crs=epsg4326,pos="bottomleft",seg.len=6,box.lty=0,outer=F)
dev.off()

# Figure 6: rangelands ----------------------------------

pdf(file="rangelands.pdf")
plot(x0)
plot(st_read("ke_rangeland/ke_rangeland.shp"),add=T,col="darkgreen")
scaleBar(crs=epsg4326,pos="bottomleft",seg.len=6,box.lty=0,outer=F)
dev.off()

# Figure 7: floodplains --------------------------------

pdf("floodplains.pdf")
plot(x0)
plot(st_read("ke_floodplains/ke_floodplains.shp"),add=T,col="blue")
scaleBar(crs=epsg4326,pos="bottomleft",seg.len=6,box.lty=0,outer=F)
dev.off()

# Figure 8: Mbeere South administrative overview -------------------

# zoom in on a sub county
mbeere=subset(x3,NAME_1=="Embu" & NAME_2=="Mbeere South")  # with further admin subdivision
mbeere_outline=subset(x2,NAME_1=="Embu" & NAME_2=="Mbeere South")  # outline of Mbeere South only

# make overview of places and wards
places=read_sf("kenya-latest-free.shp/gis_osm_places_free_1.shp")
mbeere_places=st_intersection(mbeere_outline,places)  # reduce places dataset to Mbeere South only

# make data.table for names of locations
coords=st_coordinates(mbeere_places) %>% as.data.table
labels=mbeere_places$name
dt=cbind(labels,coords)

# plot
pdf(file="mbeere_overview.pdf")
plot(mbeere_outline)
plot(mbeere_places,add=T,pch=1)
plot(mbeere,add=T)
with(dt,text(X,Y,labels,cex=0.5,pos=1))
scaleBar(crs=epsg4326,pos="bottomleft",seg.len=6,box.lty=0,outer=F)
dev.off()

# Figure 9: Mbeere South with road network -----------------------

# make overview with roads
mbeere_roads=st_intersection(mbeere_outline,roads)

pdf(file="mbeere_roads.pdf")
plot(mbeere_outline)
plot(mbeere_roads,add=T,col="grey30")
scaleBar(crs=epsg4326,pos="bottomleft",seg.len=6,box.lty=0,outer=F)
dev.off()

# Figure 10: Overview with water ---------------------------

water_mbeere=st_intersection(mbeere_outline,water)
waterways=read_sf("kenya-latest-free.shp/gis_osm_waterways_free_1.shp")
waterways_mbeere=st_intersection(mbeere_outline,waterways)

pdf(file="mbeere_water.pdf")
plot(mbeere_outline)
plot(water_mbeere,add=T,col="blue")
plot(waterways_mbeere,add=T,col="blue")
plot(mbeere_places,add=T,pch=1)
scaleBar(crs=epsg4326,pos="bottomleft",seg.len=6,box.lty=0,outer=F)
dev.off()

# Figure 11: Land use -------------------------------------

landuse=read_sf("kenya-latest-free.shp/gis_osm_landuse_a_free_1.shp")
landuse_mbeere=st_intersection(mbeere_outline,landuse)

pdf(file="mbeere_landuse.pdf")
plot(mbeere_outline)
subset(landuse_mbeere,fclass=="farmland") %>% plot(add=T,col="lightgrey")
subset(landuse_mbeere,fclass=="residential") %>% plot(add=T,col="red")
subset(landuse_mbeere,fclass=="forest") %>% plot(add=T,col="tomato2")
scaleBar(crs=epsg4326,pos="bottomleft",seg.len=6,box.lty=0,outer=F)
dev.off()

# Figure 12: Water proximity to villages ------------------

# uses buffer in meters: convert to a projection (instead of longitude-latitude)
f=function(x){st_transform(x,32736)}

# base plot
mbeere_outline |> f() |> plot()
water_mbeere |> f() |> plot(col="blue",add=T)
waterways_mbeere |> f() |> plot(col="blue",add=T)
mbeere_places |> f() |> st_buffer(2000) |> plot(add=T,lty="dotted")

# the size of the dot is the length of waterways divided by buffer size
# establish bufer areas (2 km around places)
buffer_area=f(mbeere_places) |> st_buffer(2000) 

# for each of the places calculate area, length of waterways and their ratio
dotsize=foreach(i=1:nrow(buffer_area),.combine=c) %do% {
  b=buffer_area[i,]
  a=st_area(b)
  o=st_intersection(b,f(waterways_mbeere))
  l=sum(st_length(o))
  idx=(l/a)*1000
}

# plot
mbeere_places$dotsize=dotsize
f(mbeere_places) |> plot(cex=mbeere_places$dotsize*5,add=T)

# Figure 13: distance calculations --------------------------

# this is done with the raster package. 
# create a raster

r=rast(crs=utm36S,
     resolution=100,
     extent=extent(f(mbeere_outline)),
     vals=0)

# project the sf object, vectorize and rasterize
v=function(r,vec){
  v1=st_transform(vec,32736)
  v2=vect(v1)
  rv=rasterize(v2,r,field=1)
  return(rv)
}

# clip to Mbeere South polygon
outline.r=v(r,mbeere_outline)
r=mask(r,outline.r)

# rasterize vector objects and put them together
waterways.r=v(r,waterways_mbeere)
water.r=v(r,water_mbeere)
places.r=v(r,mbeere_places)

brick=c(waterways.r,water.r,places.r)
names(brick)=c("waterways","water","places")

# calculate distances and clip to raster
distances=sapp(brick,distance)
distances=mask(distances,r)

# and plot
terra::plot(distances, col=cols,bty="n")
