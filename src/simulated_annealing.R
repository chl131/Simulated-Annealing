ggmap::register_google(key = "AIzaSyC4Fvn6-BIUt_f78-ZVTZ9naTsiLLUfSnc")
has_google_key()
locations <- c("NCKU",
               "No. 92, Huanhe Street, West Central District, Tainan City, 700",
               "No. 241, Dongping Road, East District, Tainan City, 701",
               "No. 542, Dongning Road, East District, Tainan City, 701",
               "No. 141, Qianfeng Road, North District, Tainan City, 704",
               "No. 353, Section 2, Dongmen Road, East District, Tainan City, 701",
               "No. 60, Section 2, Minzu Road, West Central District, Tainan City, 700",
               "No. 335, Jianping Road, Anping District, Tainan City, 708",
               "No. 92, Section 2, Dongmen Road, East District, Tainan City, 701",
               "No. 332, Section 3, Zhonghua East Road, East District, Tainan City, 701",
               "No. 11, Lane 18, Daxue Road, East District, Tainan City, 701",
               "No. 210, Qianfeng Road, East District, Tainan City, 701",
               "No. 255, Dongfeng Road, North District, Tainan City, 701",
               "No. 933, Yunong Road, East District, Tainan City, 701",
               "No. 658, Section 1, Ximen Rd, Tainan City, 700",
               "No. 246, Section 1, Minquan Road, West Central District, Tainan City, 700",
               "No. 277, Section 2, Lin'an Road, North District, Tainan City, 704",
               "No. 183 , Changrong Rd, East District, Section 1,  Tainan City, 701",
               "No. 26, Dongxing Road, East District, Tainan City, 701",
               "No. 63, Longshan Street, East District, Tainan City, 701")


n.locations <- length(locations)
distance      <- matrix(NA, n.locations, n.locations)    # construct a matrix for distance of point to point
routedistance <- rep(NA, n.locations)    # construct a vector to calculate a route's distance
rownames(distance) <- c(1:n.locations)
colnames(distance) <- c(1:n.locations)

for (i in 1:n.locations) {
  for (j in 1:n.locations) {
    r <- route(locations[i], locations[j], structure = "route")
    d <- sum(r$km, na.rm = TRUE)
    distance[i, j] <- d
  }
}

f <- function(x){
  for (i in 1:n.locations) {
    routedistance[i] <- distance[x[i], x[i+1]]
  }
  return(sum(routedistance))                   #calculate a route's distance (which we want to minimize)
}


###swap & reverse###
swap <- function(pointsorder){
  ss <- sample(2:n.locations, 2)
  pointsorder[ss[2:1]] <- pointsorder[ss]
  return(pointsorder)
  }
reverse <- function(pointsorder){
  ss <- sort(sample(2:n.locations, 2))
  reverse <- sum(ss) - min(ss):max(ss)
  pointsorder[ss[1]:ss[2]] <- pointsorder[reverse]
  return(pointsorder)
}

inner.loop <- function(n, temp, init.order){
  res <- matrix(NA, n, length(init.order)) # to record all order
  total.dis <- rep(NA, n)   # to record total distance from all order
  res[1,] <- init.order
  total.dis[1] <- f(res[1,])
  for(k in 2:n){
    pro.x1 <- if(runif(1) < 0.7){  # random select swap and reverse 
      swap(res[k-1,])
    } else{
     reverse(res[k-1,])
    }
    alpha1 <- exp(-(f(pro.x1)-f(res[k-1, ]))/(temp)) 
    alpha <- min(c(1, alpha1))
    u <- runif(1)
    res[k,] <- if(u <= alpha){
      pro.x1
    } else{
      res[k-1, ]
    }
    total.dis[k] <- f(res[k,])
  }
  return(list(res = res, total.dis = total.dis))
}


cooling <- 100
minf <- matrix(NA, 3000, cooling)
temp1 <- c()
temp <- 20
x0 <- c(1:n.locations, 1) ##init.order
old.xT <- x0
for(i in 1:cooling){
  res <- inner.loop(3000, temp, x0)
  new.xT <- res$res[3000,]
  x0 <- if(f(new.xT) <= f(old.xT)){
    new.xT
  } else{
    old.xT
  }
  old.xT <- x0
  minf[, i] <- res$total.dis
  temp1 <- c(temp1, rep(temp, 3000))
  temp <- temp*0.9
#  if(length(table(res$total.dis)) == 1) {
#   break
#  }
}
plot(as.vector(minf), type = "l", ylab = "Total Distance")

### plot the path ###
last.locations <- locations[old.xT]
routedata <- list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
for(i in 1:(length(old.xT)-1)){
  routedata[[i]] <- route(last.locations[i], last.locations[i+1], structure = "route", mode = "driving")
}

points=NULL
for (i in 1:(length(old.xT)-1)){
  lon <- routedata[[i]]$lon[1]
  lat <- routedata[[i]]$lat[1]
  vec <- data.frame(lon, lat)
  points <- rbind(points, vec)
}

world <- map_data("world")
world <- data.frame(world)
map <- get_googlemap("tainan", zoom = 13, extent = "device")
ggmap(map)+
geom_polygon(data = world, aes(long, lat, group = group), fill = "grey") +
  geom_point(data = points, aes(lon, lat), colour = "red", size = 3) +
  geom_path(data = routedata[[1]], aes(x = lon, y = lat), size = 1) +
  geom_path(data = routedata[[2]], aes(x = lon, y = lat), size = 1) +
  geom_path(data = routedata[[3]], aes(x = lon, y = lat), size = 1) +
  geom_path(data = routedata[[4]], aes(x = lon, y = lat), size = 1) +
  geom_path(data = routedata[[5]], aes(x = lon, y = lat), size = 1) +
  geom_path(data = routedata[[6]], aes(x = lon, y = lat), size = 1) +
  geom_path(data = routedata[[7]], aes(x = lon, y = lat), size = 1) +
  geom_path(data = routedata[[8]], aes(x = lon, y = lat), size = 1) +
  geom_path(data = routedata[[9]], aes(x = lon, y = lat), size = 1) +
  geom_path(data = routedata[[10]], aes(x = lon, y = lat), size = 1) +
  geom_path(data = routedata[[11]], aes(x = lon, y = lat), size = 1) +
  geom_path(data = routedata[[12]], aes(x = lon, y = lat), size = 1) +
  geom_path(data = routedata[[13]], aes(x = lon, y = lat), size = 1) +
  geom_path(data = routedata[[14]], aes(x = lon, y = lat), size = 1) +  
  geom_path(data = routedata[[15]], aes(x = lon, y = lat), size = 1) +
  geom_path(data = routedata[[16]], aes(x = lon, y = lat), size = 1) +
  geom_path(data = routedata[[17]], aes(x = lon, y = lat), size = 1) +
  geom_path(data = routedata[[18]], aes(x = lon, y = lat), size = 1) +
  geom_path(data = routedata[[19]], aes(x = lon, y = lat), size = 1) +
  geom_path(data = routedata[[20]], aes(x = lon, y = lat), size = 1) +
  theme_void()

