Stiff_Schot <- function(scale=1, xscale=1, xm=0, ym=0, range_x = NA, # xscale=210/297 xm=28.349, ym=-25.627,
                  #unit=c("mEq/L","umol/L","umol/L","mEq/L","mEq/L","mEq/L","mEq/L","mEq/L","umol/L","umol/L"),
                  vals=c(1, 0.5, 1, 1, 2, 0.5, 1, 1, 2, 1),
                  vars=c("K","NH4","Fe","Ca","Na","Cl","HCO3","SO4","NO3","PO4"), #clockwise from lower left
                  cols=c("orange","orange","orange","grey","grey","grey","grey","orange","orange","orange"),
                  form="A", nbows=3, pointbars = FALSE, point_length = 0.2,
                  range.cat = c(0, ceiling(max(-vals[1:5]))), range.an = c(0, ceiling(max(vals[6:10]))), axis.y=TRUE,
                  name="C2H140Q01 C23B 90688 1996-05-01 10:07", label=TRUE, 
                  col.pol="#7F7F7FAA", col.out="grey33", cex.label=0.8, col.label="grey33") {

  ver <- 0.1 # Stiff diagram, Stiff, H. A., Oct. 1951. The interpretation of chemical water analysis by means of patterns. Journal of Petroleum Technology 3 (10), section 1:15,16-section 2:3. Michael Silberbauer 2015-07-06
  ver <- 0.2 # Michael Silberbauer 2015-07-06 modifications of Schot: anions on the left
  ver <- 0.3 # Michael Silberbauer 2015-07-06 set of default values from DWS site C2H140Q01 1996-05-01 10:07
  ver <- 0.4 # Michael Silberbauer 2015-07-07 generalise polygon calculation to allow different numbers of variables
  ver <- 0.5 # Michael Silberbauer 2015-07-07 add tail with bows: now function can serve as A or B type
  ver <- 0.6 # Jack Beard 13-01-19 colour scheme changed, spacing changed, ranges for axes made variable according to values
  ver <- 0.7 # Jack Beard 24-04-19 modified script to serve as A, B type and to plot with no bows if wanted
  ver <- 0.8 # Jack Beard 19-07-19 modified script to allow for pointed ends of bars for minor ions
  
  print(vals)																																	#prints the values of the variables
  print(vars)																																	#prints the names of the variables
  print(cols)																																	#prints the color of the variables
  
  par(mar = c(4,1,3,1)) # b,l,t,r
  
  if(length(vals) != length(vars) || (length(vals) %% length(vars)) > 0) {
    print(paste("n values =", length(vals), "and variables =", length(vars), ": differ or odd"))
    print(vals)
    print(vars)
    return()
  }
  
  if(nbows > 0) tail = TRUE else tail = FALSE
  
  # Subset values/variables which will be drawn as polygon
  valsk <- vals[(nbows+1) : (length(vals)-nbows)]
  varsk <- vars[(nbows+1) : (length(vars)-nbows)]
  
  nvals <- length(vals)
  nv2 <- nvals / 2
  vals[1:nv2] <- -vals[1:nv2]
  
  nvalsk <- length(valsk)
  nvk2 <- nvalsk / 2
  valsk[1:nvk2] <- -valsk[1:nvk2]
  
  if(tail == FALSE){
    # form: single polygon shape
    if (is.na(range_x[1])) {  # adaptive vs. user defined x axis
      range.x <- c(-(max(range.cat)+0.5), max(range.an)+0.5) #allows space for variable labelling
    } else {
      range.x = range_x
    }
    range.y <- c(-(max(range.cat) + max(range.an)), 0)
    interval.y <- min(range.y) / (nv2 + 1)
    
    x.pt <- (vals * scale * xscale) + xm
    y.pt1 <- c(min(range.y) - (1:nv2 * interval.y))
    y.pt2 <- rev(y.pt1)
    y.pt <- (append(y.pt1, y.pt2) * scale * xscale) + ym
    
    plot(1:14, xlim=range.x, ylim=range.y, xlab="", ylab="", 
         xaxt="n", yaxt="n", type="n", axes=FALSE)
    t.pos <- c(rep(2, nv2), rep(4, nv2))
    if(axis.y) axis(3, col="grey", cex.axis = cex.label*1.25, lwd = 2)
    #   print(x.pt)
    #   print(y.pt)
    polygon(x.pt, y.pt, col=col.pol)
  }
  
  # form: polygon with another polygon for subset
  else if (form == "A") {
    if (is.na(range_x[1])) {  # adaptive vs. user defined x axis
      range.x <- c(-(max(range.cat)+0.5), max(range.an)+0.5) #allows space for variable labelling
    } else {
      range.x = range_x
    }
    range.y <- c(-(max(range.cat) + max(range.an)), 0)
    interval.y <- min(range.y) / (nv2 + 1)
    int.y <- interval.y / 3
    
    x.pt <- (vals * scale * xscale) + xm
    
    y.pt1 <- c(min(range.y) - (1:nv2 * interval.y))
    y.pt1[4] <- y.pt1[5] + 1.5*interval.y # reduces gap between box and bars
    y.pt1[1:3] <- c(min(range.y+0.2*interval.y) - (1:nbows * interval.y)) # reduces gap between box and bars
    
    y.pt2 <- rev(y.pt1)
    y.pt <- (append(y.pt1, y.pt2) * scale * xscale) + ym
    
    plot(1:14, xlim=range.x, ylim=range.y, xlab="", ylab="", 
         xaxt="n", yaxt="n", type="n", axes=FALSE)
    t.pos <- c(rep(2, nv2), rep(4, nv2))
    if(axis.y) axis(3, col="grey", cex.axis = cex.label*1.25, lwd = 2)
    #   print(x.pt)
    #   print(y.pt)
    x.ptk <- x.pt[(nbows+1) : (length(x.pt)-nbows)]
    y.ptk <- y.pt[(nbows+1) : (length(y.pt)-nbows)]
    x.ptt <- c(x.pt[0:nbows], x.pt[(length(x.pt)-nbows+1):length(x.pt)])
    y.ptt <- c(y.pt[0:nbows], y.pt[(length(y.pt)-nbows+1):length(y.pt)])
    col.t <- c(cols[0:nbows], cols[(length(y.pt)-nbows+1):length(y.pt)])
    # segments(x.ptt, y.ptt, rep(0+xm, length(y.ptt)), y.ptt, lwd=4)
    polygon(x.ptt, y.ptt, col=col.t, border=col.out)
    polygon(x.ptk, y.ptk, col=col.pol, border=col.out)
  }  
  
  # form: polygon with bars for subset
  else if(form == "B") {
    if (is.na(range_x[1])) {  # adaptive vs. user defined x axis
      range.x <- c(-(max(range.cat)+0.5), max(range.an)+0.5) #allows space for variable labelling
    } else {
      range.x = range_x
    }
    range.y <- c(-(max(range.cat) + max(range.an)), 0)
    interval.y <- min(range.y) / (nv2+0.8)
    int.y <- interval.y / 3
    
    x.pt <- (vals * scale * xscale) + xm
    
    y.pt1 <- c(min(range.y) - (1:nv2 * interval.y))
    y.pt1[4] <- y.pt1[5] + 1.5*interval.y # reduces gap between box and bars
    y.pt1[1:3] <- c(min(range.y+0.2*interval.y) - (1:nbows * interval.y)) # reduces gap between box and bars
    
    y.pt2 <- rev(y.pt1)
    y.pt <- (append(y.pt1, y.pt2) * scale * xscale) + ym
    
    plot(1:14, xlim=range.x, ylim=range.y, xlab="", ylab="", 
         xaxt="n", yaxt="n", type="n", axes=FALSE)
    t.pos <- c(rep(2, nv2), rep(4, nv2))
    if(axis.y) axis(3, col="grey", cex.axis = cex.label*1.25, lwd = 2)
    
    x.ptk <- x.pt[(nbows+1) : (length(x.pt)-nbows)]
    y.ptk <- y.pt[(nbows+1) : (length(y.pt)-nbows)]
    x.ptt <- c(x.pt[0:nbows], x.pt[(length(x.pt)-nbows+1):length(x.pt)])
    y.ptt <- c(y.pt[0:nbows], y.pt[(length(y.pt)-nbows+1):length(y.pt)])
    col.t <- c(cols[0:nbows], cols[(length(y.pt)-nbows+1):length(y.pt)])
    polygon(x.ptk, y.ptk, col=col.pol, border=col.out)
    
    # pointed ends for minor ions
    if(pointbars == TRUE) {
      for (i in 1:length(x.ptt)) {
        if (x.ptt[i] > point_length) {
          x.point <- c(x.ptt[i]-point_length, x.ptt[i], x.ptt[i]-point_length, xm, xm)
        } else if (x.ptt[i] < point_length & x.ptt[i] > -point_length) {
          x.point <- c(xm, xm + x.ptt[i], xm, xm, xm)
        } else if (x.ptt[i] < -point_length) {
          x.point <- c(x.ptt[i]+point_length, x.ptt[i], x.ptt[i]+point_length, xm, xm)
        }
        y.point <- c(y.ptt[i]-1.1*int.y, y.ptt[i], y.ptt[i]+1.1*int.y, y.ptt[i]+1.1*int.y, y.ptt[i]-1.1*int.y)
        poly <- polygon(x.point, y.point, col = "orange", border = col.out)
      }
      
    # rectangular ends
    } else {
      rect(x.ptt, y.ptt+int.y, rep(0+xm, length(y.ptt)), y.ptt-int.y, col=col.t, border=col.out)
    }
  }
  
  # ablines
  abline(v=0, col="grey60", lwd = 2)
  abline(v=-1, col="grey60", lty="dashed", lwd = 2)
  abline(v=1, col="grey60", lty="dashed", lwd = 2)
  
  # labelling
  if(label) text(x.pt, y.pt, pos=t.pos, vars, cex=cex.label, col=col.label, xpd = TRUE)
  #if(label) text(0, 0, name, cex=cex.label*1.25)
  if(label) text(0, min(range.y)*1.2, name, cex=cex.label*1.4, xpd = TRUE)
  #axis(2, labels=FALSE, pos=0, tick=FALSE)
}

