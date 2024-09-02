# glosmurb

This package has been developped for the GloUrb project. It gathers functions that help collect OpenStreetMap (OSM) data inside a polygon (provided as an spatial features -sf- object). 

Its main functions are:
- **get_osmdata()**, which collects OSM data corresponding to a key-value inside a bounding shape. If a file already exists corresponding to "{data/osmdata}/{area}_{key}_{value}, the data is not retrieved through a new query but through reading this file.
- **pick_osmdata()**, which picks one geometry (points, lines, polygons) for the object returned by get_osmdata() and returns it as an sf object. 
