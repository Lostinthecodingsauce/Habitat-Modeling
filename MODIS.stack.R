### MODIS download ####

library("MODISTools")
setwd("C:/Users/bfsco/Desktop/Masters Research/UCEs/Geospaital/MODIS")

# read in MODIS bands
MODIS <- mt_bands(product = "MOD13Q1")

df <- read.csv("Cardinalis_sinuatus.csv")

mt_batch_subset(
  df,
  MOD13Q1,
  "m_16_days_NDVI",
  start = "2000-01-01",
  end = format(Sys.time(), "%Y-%m-%d"),
  km_lr = 0,
  km_ab = 0,
  out_dir = tempdir(),
  internal = TRUE,
  ncores = "auto"
)


# cs is the vector of complex points to convert
convert.fft <- function(cs, sample.rate=1) {
  cs <- cs / length(cs) # normalize
  
  distance.center <- function(c)signif( Mod(c),        4)
  angle           <- function(c)signif( 180*Arg(c)/pi, 3)
  
  df <- data.frame(cycle    = 0:(length(cs)-1),
                   freq     = 0:(length(cs)-1) * sample.rate / length(cs),
                   strength = sapply(cs, distance.center),
                   delay    = sapply(cs, angle))
  df
}

convert.fft(fft(1:4))
