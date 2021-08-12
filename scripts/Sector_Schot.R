Sector_Schot <- function(d.sector = c(1, 0.5, 1, 1, 2, 0.5, 1, 1, 2, 1), text.sector, x.centre = 0, y.centre = 0, scale = 1, scale.x = 1, r.limit = 5, 
                         col.schot = c("#00FF00", "#1919FF", "#A020F0", "#454545", "#CCCCCC", "#A6A6A6", 
                                       "#454545", "#FF0000", "#FFA500", "#DDDD00"), alfa="AA",
                         angle.0 = 90, n.sectors=10, grid.circle=T, n.rings=c(1, 2, 5, 10, 20, 50), labeltext=T, label_numbers = F, labelsize = 0.5) {
  
  ver <- 0.01 # 2014-03-03 Michael Silberbauer plot sectors by brute force
  ver <- 0.02 # 2014-03-03 Michael Silberbauer apply functions to the task
  ver <- 0.03 # 2014-03-06 Michael Silberbauer apply latest suggestions by Paul Schot
  ver <- 0.04 # 2014-03-19 Michael Silberbauer pastel rainbow using R function
  ver <- 0.11 # 2014-03-21 Michael Silberbauer move symbol code into a function
  ver <- 0.12 # 2014-03-21 Michael Silberbauer add option to scale and shift symbol
  ver <- 0.13 # 2019-06-14 Jack Beard add functionality for plotting, allow for adaptive sizing of plots
  ver <- 0.14 # 2019-07-19 Jack Beard add suggestions proposed Paul Schot
  
  print(d.sector)																																	#prints the values of the variables
  
  # functions to convert degrees to radians
  deg2rad.x <- function (angle.deg, angle.0 = 90, radius) radius * cos ((angle.deg + angle.0) * pi / 180)
  deg2rad.y <- function (angle.deg, angle.0 = 90, radius) radius * sin ((angle.deg + angle.0) * pi / 180)

  # funtion to plot all or parts of a circle manually
  circular <- function(x.centre=0, y.centre=0, scale=1, scale.x=1, deg.1, deg.2, radius, 
                       r.col="red", width = 4, r.txt="") {
    for(angle.deg in deg.1:deg.2) {
      angle.rad0 <- (angle.deg + angle.0) * pi / 180
      x.p0 <- scaleshiftx(radius * cos(angle.rad0), x.centre, scale, scale.x)
      y.p0 <- scaleshifty(radius * sin(angle.rad0), y.centre, scale)
      angle.rad1 <- (angle.deg + angle.0 - 1) * pi / 180
      x.p1 <- scaleshiftx(radius * cos(angle.rad1), x.centre, scale, scale.x)
      y.p1 <- scaleshifty(radius * sin(angle.rad1), y.centre, scale)
      lines(c(x.p0, x.p1), c(y.p0, y.p1), col = r.col, lwd = width, lty = 2)
      if(angle.deg == 0) text(x.p0, y.p0, r.txt, col = r.col, cex = 0.3)
    }
  }
  
  #col.schot <- paste0(col.schot, alfa)
  
  # plot settings options
  #maxdata = max(sqrt(d.sector))
  #r.max <- maxdata * 1.1
  #plot(1:1, xlim=c(-r.max, r.max), ylim=c(-r.max, r.max), type="n", 
  #     xaxt="n", yaxt="n", xlab="", ylab="")
  
  scaleshiftx <- function(x, x.centre, scale, scale.x) {
    x.centre + (x * scale * scale.x)
  }
  scaleshifty <- function(y, y.centre, scale) {
    y.centre + (y * scale)
  }
  deg.sec <- 360 / n.sectors
  
  # plot circular grid
  if(grid.circle) for(r.circ in n.rings) {
    circular(x.centre, y.centre, scale, scale.x, 0, 360, sqrt(r.circ * scale), 
             r.col="grey70", r.txt=r.circ)
  }
  
  # plotting Sectors
  for(n.s in 1:n.sectors) {
    # defining angles for each sector
    deg.1 <- (n.s - 1) * deg.sec
    deg.2 <- n.s * deg.sec
    radiu <- as.numeric(d.sector[n.s])
    # print(paste(n.s, radiu, r.limit)) # debug print
    if(radiu > r.limit) r.xceed <- TRUE else r.xceed <- FALSE
    radius <- sqrt(radiu) * scale
    if(r.xceed) radius <- sqrt(r.limit) * sqrt(scale)
    #x.centre <- 0
    #y.centre <- 0
    svectr <- c(0, deg.1:deg.2, 0)
    svectr <- data.frame(svectr)
    svectr$x <- deg2rad.x (svectr$svectr, angle.0, radius)
    svectr$y <- deg2rad.y (svectr$svectr, angle.0, radius)
    svectr$x[1] <- 0
    svectr$x[length(svectr$x)] <- 0
    svectr$y[1] <- 0
    svectr$y[length(svectr$y)] <- 0
    svectr$x <- scaleshiftx(as.numeric(svectr$x), x.centre, scale, scale.x)
    svectr$y <- scaleshifty(as.numeric(svectr$y), y.centre, scale)
    polygon(svectr$x, svectr$y, col=col.schot[n.s], border=NA )
    #text(svectr$x, svectr$y, svectr$svectr, cex=0.5)
    deg.t <- mean(c(deg.1, deg.2))
    #print(paste(deg.1, deg.2, deg.t))
    if(r.xceed) { # tag the sector as too large:
      x.ar1 <- scaleshiftx(deg2rad.x (deg.t, angle.0, radius * 0.82), x.centre, scale, scale.x)
      x.ar2 <- scaleshiftx(deg2rad.x (deg.t, angle.0, radius * 1.05), x.centre, scale, scale.x)
      y.ar1 <- scaleshifty(deg2rad.y (deg.t, angle.0, radius * 0.82), y.centre, scale)
      y.ar2 <- scaleshifty(deg2rad.y (deg.t, angle.0, radius * 1.05), y.centre, scale)
      arrows(x.ar1, y.ar1, x.ar2, y.ar2, length=0.3, lwd=4, code=2, col="#FF0000AA")
      x.ar.t <- scaleshiftx(deg2rad.x (deg.t, angle.0, radius * 0.55), x.centre, scale, scale.x)
      y.ar.t <- scaleshifty(deg2rad.y (deg.t, angle.0, radius * 0.55), y.centre, scale)
      #text(x.ar.t, y.ar.t, "*", cex=4, col="#FF0000AA")
    }
  }
  # Plotting text
  for(n.s in 1:n.sectors) {
    # defining angles for each sector
    deg.1 <- (n.s - 1) * deg.sec
    deg.2 <- n.s * deg.sec
    radiu <- as.numeric(d.sector[n.s])
    if (radiu >= 1) {
      if(radiu > r.limit) r.xceed <- TRUE else r.xceed <- FALSE
      if(r.xceed == FALSE) radius <- sqrt(radiu) * scale
      if(r.xceed) radius <- sqrt(r.limit) * sqrt(scale)
      }
    if (radiu < 1)       radius <- sqrt(0.95) * scale
    deg.t <- mean(c(deg.1, deg.2))
    x.t <- scaleshiftx(deg2rad.x (deg.t, angle.0, radius * 1.1), x.centre, scale, scale.x)
    y.t <- scaleshifty(deg2rad.y (deg.t, angle.0, radius * 1.1), y.centre, scale)
    if(labeltext) {
      if(label_numbers == T) {
        text(x.t, y.t, paste(signif(radiu, 3), " [", text.sector[n.s], "]", sep=""), cex=labelsize)
      } else{
        text(x.t, y.t, text.sector[n.s], cex=labelsize)
      }
    }
  }
  # add box around plot
  #box(col="grey")
}
