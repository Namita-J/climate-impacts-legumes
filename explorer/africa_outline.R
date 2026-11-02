library(sf); library(rnaturalearth)
a <- ne_countries(scale="medium", continent="Africa", returnclass="sf")
a <- st_simplify(st_make_valid(a[,c("name")]), dTolerance=0.05, preserveTopology=TRUE)
st_write(a, commandArgs(TRUE)[1], driver="GeoJSON", delete_dsn=TRUE, quiet=TRUE)
