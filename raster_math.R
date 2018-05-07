# Summarize all images into one set of final rasters

# testing

p1 <- sclass3
p2 <- focal(sclass3, w=matrix(1/9, nc=3, nr=3))

p.mean <- mean(p1,p2)
p.max <- max(p1,p2)
p.min <- min(p1,p2)
# p.med <- median(c(p1,p2))

# ============================================

img.names <- scan(file="classified_image_log.txt", what="character") # get file names of classified images
imgs <- sapply(imgs, function(x) raster(x)) # load rasters

# remove gray classes

# summarize

