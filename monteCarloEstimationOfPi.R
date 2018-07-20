####################################################################################
# monte carlo simulation
####################################################################################

# generate random points between (0,0) and (1,1)
nrOfPoints <- 100000
points <- data.frame(x = sample(nrOfPoints), y = sample(nrOfPoints))/nrOfPoints

# classify points so that:
# points inside a circle with radius 1 is labelled as "within"
# points outside a circle with radius 1 is labelled as "outside"
points$thresh <- ifelse(sqrt(points$x**2 + points$y**2) <= 1, "within", "outside")

# estimation of pi
4*(nrow(points[points$thresh == "within",])/nrow(points))

# plot points with colored labels
plot(points[points$thresh == "within","x"], points[points$thresh == "within","y"], col = "red")
points(points[points$thresh == "outside","x"], points[points$thresh == "outside","y"], col = "blue")
