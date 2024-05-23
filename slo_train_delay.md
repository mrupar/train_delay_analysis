Slovenske železnice train delay
================

- [Data import](#data-import)
- [Network visualization](#network-visualization)

## Data import

``` r
delays <- read_csv(unzip("train_delay.zip", "delays.csv"))
```

    ## Rows: 3211359 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): train_type, delay_type
    ## dbl (4): timestamp, train_no, station_id, delay
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
rail_edge <- read_csv(unzip("train_delay.zip", "rail_edgelist.csv"), col_names = FALSE)
```

    ## Rows: 536 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (2): X1, X2
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
rail_node <- read_csv(unzip("train_delay.zip", "rail_nodes.csv"))
```

    ## Rows: 266 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): id, station_name
    ## dbl (2): lat, lng
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
rail_node$id <- as.character(rail_node$id)
rail_edge$X1 <- as.character(rail_edge$X1)
rail_edge$X2 <- as.character(rail_edge$X2)

rail_edge$X1[nchar(rail_edge$X1)==4] <- rail_edge$X1[nchar(rail_edge$X1)==4] %>% paste0("0",.)
rail_edge$X2[nchar(rail_edge$X2)==4] <- rail_edge$X2[nchar(rail_edge$X2)==4] %>% paste0("0",.)
```

## Network visualization

``` r
g <- graph.data.frame(rail_edge, directed=FALSE, vertices=rail_node)
```

    ## Warning: `graph.data.frame()` was deprecated in igraph 2.0.0.
    ## ℹ Please use `graph_from_data_frame()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
gg <- as_data_frame(g, "both")
vert <- gg$vertices
coordinates(vert) <- ~lng+lat

edges <- gg$edges

edges <- lapply(1:nrow(edges), function(i) {
  as(rbind(vert[vert$name == edges[i, "from"], ], 
           vert[vert$name == edges[i, "to"], ]), 
     "SpatialLines")
})

for (i in seq_along(edges)) {
  edges[[i]] <- spChFIDs(edges[[i]], as.character(i))
}

edges <- do.call(rbind, edges)

leaflet(vert) %>% addTiles() %>% 
  addCircleMarkers(
    data = vert,
    radius = 2,
    color = "blue", 
    fill = TRUE, 
    fillOpacity = 1, 
    opacity = 1) %>%
  addPolylines(
    data = edges, 
    color = "red",
    weight = 2)
```

![](slo_train_delay_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
