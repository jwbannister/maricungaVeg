feid_spatial <- rgdal::readOGR("./data-raw/FEIDs", "FEID_points")
feid_df <- data.frame(feid=feid_spatial$FEID, x=feid_spatial$X, y=feid_spatial$Y)

vega_spatial <- rgdal::readOGR("./data-raw/FEIDs", "Vega")
vega_df <- build_polygon_df(vega_spatial)

feid_df <- assign_points_to_polygons(points=feid_df, polygons=vega_df)

slai_df <- readr::read_csv("./data-raw/Seasonal_LAI_FEID_All_East.txt")
colnames(slai_df) <- tolower(colnames(slai_df))
slai_df <- dplyr::select(slai_df, -y1985, -y1986, -y1987)

maricunga_df <- dplyr::inner_join(feid_df, slai_df, by="feid")
maricunga_df <- maricunga_df[c(2, 3, 1, 4:ncol(maricunga_df))]

save(maricunga_df, vega_df, file="./data/maricunga_slai.RData")
