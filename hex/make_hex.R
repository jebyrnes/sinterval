library(magick)
library(jpeg)
library(ggplot2)

pitch_jpeg <- image_read("hex/pitchfork.jpg") %>%
  image_fill("none", "+0+0")
image_background(pitch_jpeg, "lightblue")


pitch <- readJPEG("hex/pitchfork.jpg")
pitch_r <- as.raster(pitch)
pitch_r[pitch_r == "#FFFFFF"] <- NA
#pitch_r <- gsub("#", "", pitch_r)
plot.new()
grid::grid.raster(pitch_r)
