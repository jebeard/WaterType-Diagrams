Maucha_Schot <- function(scale=1, xscale=1, xm = 0, ym = 0,							#xscale = 210/297 size of figure for A4, xm and ym = starting location of the first figure
                   #vars=c("K", "Na", "Ca", "Mg", "SO4", "Cl", "HCO3", "CO3"),
                   vals=c(1, 0.5, 1, 1, 2, 0.5, 1, 1, 2, 1),
                   vars=c("Na", "K" ,"CO3" ,"HCO3", "Cl", "SO4" ,"Mg", "Ca"),
                   cols=c("#00FF00", "#1919FF", "#A020F0", "#454545", "#CCCCCC", 
                          "#A6A6A6", "#454545", "#FF0000", "#FFA500", "#DDDD00"),	
                   alfa="AA", startAngle = 90, r.limit = 5,
                   name="90283 A2R009Q09 Roodeplaat Dam on Pienaars River: Point 9 in Dam: median for 1980-07-04 to 2013-04-22",
                   label=T, label_numbers = F, cex.label=0.5, col.label="grey", transform="log10") {
					
  ver <- 0.0 # Maucha graphics adapted to R by Susanne Walford 2010-04-09
  ver <- 0.1 # Maucha standalone function Michael Silberbauer 2013-12-02
  ver <- 0.2 # generalised radial symbol with input from Paul Schot 2015-03-12
  ver <- 0.3 # Jack Beard adapts sizing to max values, adjusts according to input from Paul Schot 2019-07-19
  
  print(vals)																																	#prints the values of the variables
  print(vars)																																	#prints the names of the variables
  print(cols)																																	#prints the color of the variables
  
  #   Plotting starts at 9 o'clock and proceeds anti-clockwise
  #   Total Alkalinity is expressed as CaCO3 but should be used in the HCO3 variable, 
  #   because at the pH of natural surface waters in South Africa, most alkalinity is bicarbonate, i.e. HCO3-.
  #   The default xscale factor is for A4 landscape.
  
  #   cols=c("yellow2", "purple", "black", "cyan", "green", "blue", "grey", "red")
  #   cols=c("#EEEE00", "#A020F0", "#000000", "#00FFFF", "#00FF00", "#0000FF", "#C0C0C0", "#FF0000")
  #   cols <- paste(cols, "AA", sep="")
  #   vars=c("Na", "K", "-", "TAL", "Cl", "SO4", "Mg", "Ca")
  #   ion <- c(K, Na, Ca, Mg, SO4, Cl, HCO3, CO3)
  #   ion  <- c (Na, K, CO3, HCO3, Cl, SO4, Mg, Ca)
  #   ion <- vals
  #   print(ion)
  #   anions  <- SO4 + Cl + HCO3 + CO3
  #   cations <- K + Na + Ca + Mg
  #   total.ions <- cations + anions
  
  #cols <- paste0(cols, alfa)																										#adds alpha(AA) to the colors
  cols <- cols
  # total.ions = sum(vals)
  total.ions <- 10																						               		#average value of variables Meq
  ps <- total.ions * scale																											#ps is the total value times a scaling factor
  sectorAngle <- 360 / (length(vals) * 2)																				#the angle of every sector for 180degrees circle
  if (transform == "log10") {A <- log10(ps + 1)} else {A <- ps}					      	#if the transformation is log10, scale
  
  sector <- sectorAngle * ( pi / 180 )																					#sector calculations
  angadj <- startAngle * (pi / 180)
  R <- ( (A / (length(vals) * 2)) * 2 / sin(sector) ) ^ 0.5                 
  R <- ( (A / (length(vals) * 2)) * 2 / sin(sector) ) ^ 0.5
  circleSector <- sector / (length(vals) / 2)
  # Plot circle
  x.circ <- array(1 : length(vals) ^ 2)
  y.circ <- array(1 : length(vals) ^ 2)
  for (i in 1 : (length(vals) ^ 2) ) {
    x.circ[i] <- R * cos(circleSector * i) * xscale
    y.circ[i] <- R * sin(circleSector * i)
  }
  x.circ <- x.circ + xm
  y.circ <- y.circ + ym
  
  #plot(1,1, xlim = c(range(x.circ)), ylim = c(range(y.circ)), xlab="", ylab="", xaxt="n", yaxt="n", type="n", axes=FALSE)
  
  polygon(x.circ, y.circ, col="#FFFFFFAA", border="grey80", lwd = 4)
  # Plot Maucha symbol
  count <- 1
  for (i in 1:length(vals) ) {
    vals.conc <- vals[i]
    if (vals.conc >= r.limit) r.exceed <- TRUE else r.exceed <- FALSE
    if (r.exceed) Ai <- r.limit / total.ions * A
    if (r.exceed == FALSE) Ai <- vals.conc / total.ions * A 
    h <- count - 1
    j <- count + 1
    x.c <- R * cos((sector * h) + angadj) * xscale
    y.c <- R * sin((sector * h) + angadj) 
    x.d <- R * cos((sector * j) + angadj) * xscale
    y.d <- R * sin((sector * j) + angadj)
    a <- Ai / ( R * sin(sector) )
    x.b <- a * cos((sector * count) + angadj) * xscale
    y.b <- a * sin((sector * count) + angadj)      
    x.ar1 <- x.b * 0.82 + xm
    y.ar1 <- y.b * 0.82 + ym
    x.ar2 <- x.b * 1.05 + xm
    y.ar2 <- y.b * 1.05 + ym
    x.ar.t <- x.b * 0.55 + xm 
    y.ar.t <- y.b * 0.55 + ym
    #print(paste(0, x.d, x.b, x.c, 0, "+", xm))
    x <- c(0, x.d, x.b, x.c, 0) + xm
    y <- c(0, y.d, y.b, y.c, 0) + ym
    polygon( x, y, col=cols[i], border=NA)
    
    
    # Plot red arrow and truncate 
    if (r.exceed) {
      arrows(x.ar1, y.ar1, x.ar2, y.ar2, length=0.3, lwd=4, code=2, col="#FF0000AA")
      #text(x.ar.t, y.ar.t, "*", cex=4, col="#FF0000AA")
    }
    
    count = count + 2
  }
  # Plot labels
  for (i in 1:length(vals) ) {
    vals.conc <- vals[i]
    if (vals.conc >= r.limit) Ai <- r.limit / total.ions * A
    if (vals.conc < r.limit) Ai <- vals.conc / total.ions * A
    a <- Ai / ( R * sin(sector) )
    x.t <- 1.1 * max(a, R) * cos((sector * count) + angadj) * xscale
    y.t <- 1.1 * max(a, R) * sin((sector * count) + angadj)
    x.t <- x.t+xm
    y.t <- y.t+ym
    #if(label) print(paste(x.t, y.t, label, vars[i]))
    if(label) {
      if(label_numbers == T) {
        text(x.t, y.t, paste(signif(vals[i], 3), " [", vars[i], "]", sep=""), cex=cex.label, col="black")
      } else{
        text(x.t, y.t, vars[i], cex=cex.label, col="black")
      }
    }
    
    count = count + 2
  }
}

