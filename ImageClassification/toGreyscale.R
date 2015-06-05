#Convert images from Color to Greyscale

require("jpeg")
require("EBImage")
require("ripa")

convertToGreyscale <- function(file) {
  orig <- readImage(file)
  
  #convert to greyscale with one channel
  grey <- channel(orig,"gray")
  
  #resize the image
  grey <- resize(grey,50,50)
  
  #pull the image name out of the file string
  name = strsplit(file,"/")[[1]][3]
  
  #write to the greyscaled folder
  writeImage(grey, file.path("images","greyscaled",name))
}

#read in images
filenames <- list.files("images/images_training_rev1/",
                        pattern="*.jpg",full.names=TRUE)

#convert all of them to greyscale
print("Converting to greyscale!")
#count <- 0
for (f in filenames[count:length(filenames)]) {
  convertToGreyscale(f)
  count = count + 1
  if(count %% 100 == 0) {
    print(count)
  }
}