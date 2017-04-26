# This is one program to determing if there are same boat 


library(data.table)
library(gtools)
library(fpc)
library(dbscan)
library(factoextra)
library(imager)
#options(width = 100)
#options(max.print = 5000)

#--------------------------------------------------------------------

feat_1<- function(folderpath, samp, res) {
  
  std_data= list()
  df= data.frame()
  piclist<- sample((list.files(folderpath, pattern=".jpg", full.names=TRUE, recursive = T)) 
                   , ((samp/100)* length(list.files(folderpath, pattern=".jpg", full.names=TRUE, recursive = T
                   ) )))
  
  for (i in 1:length(piclist)) {
    
    im<- load.image(piclist[i])
    
    im_res <- imager::resize(im, res, res) 
    im_std<-(im_res-mean(im_res))/sd(im_res)
    
    
    species<- paste0(basename(dirname(piclist[i])))
    img_name<-paste0(basename(piclist[i]))
    width<- paste0(dim(im)[1])
    height<- paste0(dim(im)[2])
    
    img_std_data <- as.data.frame(im_std)
    img_std_vector <- as.vector(img_std_data$value)
    vec_std <- c(species = species,img_name=img_name,wid=width,ht=height,img_std_vector)
    std_data[[i]]<-vec_std
    
    print(paste0( "Pic - ",i , " successfully converted ", piclist[i]))
  }
  
  df<- as.data.frame(do.call(rbind, std_data))
  return(df)
  
}

d.1<- feat_1(folderpath = "../input/train", samp= 10, res=128)
cat ("image conversion to flat file - done")
#cat(paste("Size of d.1 ", format(object.size(d.1), unit="Gb"))) # just ensuring its created

#_________________________________ Coverting data into numeric 


temp.matrix<- as.matrix(d.1[ , !(names(d.1) %in% c("species","img_name"))])
mode(temp.matrix) = "numeric"

d.2<- data.frame(temp.matrix)
cat ("file conversion to numeric matrix - done")
#table(sapply(d.2,is.numeric)) # ensuring all converted to numeric


#_________________________________ Find knee for determining optimal 'eps'

#k=kNNdistplot(d.2, k = 2)
# I have found it eps = 160 on normalized data through trail and error

#_________________________________ Creating density based clusters


set.seed(1234)
res.fpc <- fpc::dbscan(d.2, eps = 160, MinPts =4)
cat ("Cluster formation  - done")
# table(res.fpc$cluster) # just checking the number of final cluster before drawing clsuter map
fviz_cluster(res.fpc, d.2, geom = "point")
cat ("Plotting of cluster - done")
#_________________________________ Writing the cluster  to the image data


final_data<- as.data.frame(d.1[,1:2])
final_data$cluster<- res.fpc$cluster
cat ("Cluster assigned to images- done")

#_________________________________ Showing pictures that are closer to each other

evl.1<-setorder(setDT(final_data), cluster)[, head(.SD, 4), keyby = cluster]
evl.2<-evl.1[order(-cluster),] 

evl.2<- as.matrix(evl.2)
cat ("Ordered segment tables generation- done")

head(evl.2)


write.csv(final_data,"cluster.csv")


par(mfrow=c(2,2))
for (i in 1:nrow(evl.2)) {
  
  plot((load.image(paste0("../input/train/",evl.2[i,2],"/",evl.2[i,3] ))))
}

cat ("Plotting 4 closer images- done")


#  last images that you see are not assigned to any segments , they are supposed to  unique  images

# Please upvote if  you like it. Thanks , enjoy !!







