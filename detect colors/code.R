library(imager)

im <- load.image("image1.png")
plot(im)

temp <- imsub(im,y %inr% c(600,750),x %inr% c(800,1800))

plot(temp)

temp_c <- color.at(temp)

temp_df <- as.data.frame(temp_c)
