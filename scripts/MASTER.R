#-------------------------------------------------------------------------------------------------
### MASTER SCRIPT TO CREATE WATER QUALITY DIAGRAMS FROM INPUT EXCEL! 
#-------------------------------------------------------------------------------------------------
# Author: Jack Beard
# Date: 12/12/2020

# automatically install, load libraries
rm(list=ls(all=TRUE))
list.packages = c("readxl", "XLConnect")
new.packages = list.packages[!(list.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.packages, require, character.only = TRUE)

in_excel <- "./data/input_data.xlsx"

#########################################################################
### !!!USER INPUT SETTINGS!!!
#########################################################################

# put names of worksheets from input_data.xlsx you wish to plot here!
# NOTE: these will also be used to name output figures!!
plot_worksheets <- c("multi_figure")

# put units of data you have input to input_data.xlsx
units_dat <- "mg" # "meq"

# choose plotting as series (one figure) or seperate figures
combine <- F #T

# choose output format
format <- "tif" #"pdf"

# plot Stiff/Sector/Maucha?
Stiff_A <- T #F
Stiff_B <- T #F
Sector <- T #F
Maucha <- T #F

#########################################################################
### !!!PLOTTING/SCALING FACTORS - MAY NEED CHANGING DEPENDING ON OBS!!!
#########################################################################

# limit extreme values cutoff
r.limit = 10

# use to adjust diagram widths TO MAKE CIRCULAR
xscale_Sector = c(8.7) # length of all figures you want to make
xscale_Maucha = c(8.6)
text_scaling = c(1,1)
# good to leave y constant and alter x if needed!
yscale_Sector = 10 
yscale_Maucha = 10

# pointy ends to stiff bars?
pointy = TRUE

# tif scaling by pixels (higher =  better quality)
tif_pix = 100

#########################################################################

#-------------------------------------------------------------------------------------------------
# IMPORT FUNCTIONS
#-------------------------------------------------------------------------------------------------

# list scripts in directory
source_functions <- list.files("./scripts", full.names = T)
source_functions <- source_functions[!grepl("MASTER", source_functions)]
# source files to read functions
lapply(source_functions, source)

#-------------------------------------------------------------------------------------------------
# READING DATA, sorting/scaling
#-------------------------------------------------------------------------------------------------

# read in data from excel
data_list <- lapply(plot_worksheets, function(worksheet){
  read_excel(in_excel, sheet=worksheet)
}) 
plotting_codes <- lapply(plot_worksheets, function(worksheet){
  data <- read_excel(in_excel, sheet=worksheet)
  data$Plotting_Code
}) 

# apply sorting/scaling
data_ss <- lapply(data_list, function(data){
  apply_sort_scale(data, units=units_dat)
})
names(data_ss) <- plot_worksheets

#-------------------------------------------------------------------------------------------------
### PLOTTING - SETUP 
#-------------------------------------------------------------------------------------------------

# start loop over data list
for(i in seq_along(data_ss)){
  
# select data, naming convention
data <- data_ss[[i]]
nam <- names(data_ss)[[i]]
  
# no. of plots in series
no_obs <- nrow(data)
width_1 <- no_obs * 10

# file naming function
file_out <- function(plot_type,nam_con=nam,driv="pdf") {
  name <- paste0("./figures_out/",plot_type,"/",nam,"_",plot_type,".",driv)
  return(name)
}

# file naming function
file_out_1 <- function(plot_type,nam_con=nam,code,driv="pdf") {
  name <- paste0("./figures_out/",plot_type,"/",code,"_",plot_type,".",driv)
  return(name)
}

# copy of df with nas replaced with -1
data_nonas <- data.frame(data)
data_nonas[is.na(data_nonas)] <- 0.00001
data_max <- ceiling(max(data_nonas)) + 0.5

#-------------------------------------------------------------------------------------------------
### PLOTTING - STIFF 
#-------------------------------------------------------------------------------------------------

if (Stiff_A) {
  
  if(combine){
    
    # setting up pdf
    if(format == 'tif'){
      tiff(file_out("Stiff_A", driv = 'tif'), height = 5.5*tif_pix*2, width = width_1*tif_pix*2, pointsize = 35 *tif_pix/50)
      par(mar=c(1,1,1,1))
    } else if (format == 'pdf'){
      pdf(file_out("Stiff_A"), height = 5.5, width = width_1, pointsize = 28)
    }
    
    # row config    
    par(mfrow = c(1,no_obs), bg = NA)
    
    # plotting data
    for (j in 1:nrow(data)) {
      series <- data[j,]
      Stiff_Schot(vals = as.numeric(series), range_x = c(-data_max, data_max), form = "A", name = plotting_codes[[i]][j], pointbars = pointy)
    }
    
    dev.off()  
    
  } else {
    
    # plotting data
    for (j in 1:nrow(data)) {
      
      # setting up pdf
      if(format == 'tif'){
        tiff(file_out_1("Stiff_A", code = plotting_codes[[i]][j], driv = 'tif'), height = 5.5*tif_pix, width = 10 * tif_pix, pointsize = 15*tif_pix/50)
        par(mar=c(1,1,1,1))
      } else if (format == 'pdf'){
        pdf(file_out_1("Stiff_A", code = plotting_codes[[i]][j]), height = 5.5, width = 10, pointsize = 15)
      }
      
      series <- data[j,]
      Stiff_Schot(vals = as.numeric(series), range_x = c(-data_max, data_max), form = "A", name = plotting_codes[[i]][j], pointbars = pointy)
      
      dev.off()  
    }
    
  }
    
}


if (Stiff_B) {
  
  if(combine){
    
    # setting up pdf
    if(format == 'tif'){
      tiff(file_out("Stiff_B", driv = 'tif'), height = 5.5*tif_pix*2, width = width_1*tif_pix*2, pointsize = 35*tif_pix/50)
      par(mar=c(1,1,1,1))
    } else if (format == 'pdf'){
      pdf(file_out("Stiff_B"), height = 5.5, width = width_1, pointsize = 28)
    }
    
    # row config    
    par(mfrow = c(1,no_obs), bg = NA)
    
    # plotting data
    for (j in 1:nrow(data)) {
      series <- data[j,]
      Stiff_Schot(vals = as.numeric(series), form = "B", range_x = c(-data_max, data_max), name = plotting_codes[[i]][j], pointbars = pointy)
    }
    
    dev.off()    
    
  } else {
    
    # plotting data
    for (j in 1:nrow(data)) {
      
      # setting up pdf
      if(format == 'tif'){
        tiff(file_out_1("Stiff_B", driv = 'tif', code = plotting_codes[[i]][j]), height = 5.5*tif_pix, width = 10*tif_pix, pointsize = 15*tif_pix/50)
        par(mar=c(1,1,1,1))
      } else if (format == 'pdf'){
        pdf(file_out_1("Stiff_B", code = plotting_codes[[i]][j]), height = 5.5, width = 10, pointsize = 15)
      }
      
      series <- data[j,]
      Stiff_Schot(vals = as.numeric(series), form = "B", range_x = c(-data_max, data_max), name = plotting_codes[[i]][j], pointbars = pointy)
      
      dev.off()
      
    }
  }
}

#------------------------------------------------------------------------------------------------------------
### PLOTTING - SECTOR 
#------------------------------------------------------------------------------------------------------------

if (Sector) {
  
  # defining plot limits according to data ranges
  maxdata <- max(sqrt(data_nonas))
  
  if(combine){
    
    lims <- maxdata * 1.5
    
    # config tiff
    if(format == 'tif'){
      tiff(file_out("Sector", driv = 'tif'), height = lims*yscale_Sector*tif_pix, width = lims*no_obs*xscale_Sector[i]*tif_pix, pointsize = 50*tif_pix/50)
      par(mar=c(1,1,1,1))
    } else if (format == 'pdf'){
      pdf(file_out("Sector"), height = lims*yscale_Sector, width = lims*no_obs*xscale_Sector[i], pointsize = 50)
    }
    
    plot(1:1, xlim=c(0, 2*lims*no_obs), ylim=c(-lims, lims), type="n", 
         xaxt="n", yaxt="n", xlab="", ylab="")
    
    for (j in 1: nrow (data)){
      # isolate data
      series <- data[j,]
      d.sector <- series
      
      # plotting
      Sector_Schot(x.centre = (2*j-1)*lims, y.centre = 0, scale = 1, scale.x = 1, r.limit = r.limit, d.sector, text.sector = names(d.sector),
                   n.sectors = 10, grid.circle = TRUE, n.rings = 1, labeltext = TRUE, labelsize = 0.5*text_scaling[i])
      text(x = (2*j-1)*lims, y = -0.6*lims, plotting_codes[[i]][j], cex = 0.9, col = "black")
    }
    
    dev.off()
    
  } else {
    
    lims <- maxdata * 1.2
    xscale_Sector <- 10
    
    for (j in 1: nrow (data)){
      # config tiff
      if(format == 'tif'){
        tiff(file_out_1("Sector", code = plotting_codes[[i]][j], driv = 'tif'), height = lims*yscale_Sector*tif_pix, width = lims*xscale_Sector[i]*tif_pix, pointsize = 50*tif_pix/50)
        par(mar=c(1,1,1,1))
      } else if (format == 'pdf'){
        pdf(file_out_1("Sector", code = plotting_codes[[i]][j]), height = lims*yscale_Sector, width = lims*xscale_Sector[i], pointsize = 50)
      }
      
      # isolate data
      series <- data[j,]
      d.sector <- series
      
      plot(1:1, xlim=c(-lims, lims), ylim=c(-lims, lims), type="n", 
           xaxt="n", yaxt="n", xlab="", ylab="")
      
      # plotting
      Sector_Schot(x.centre = 0, y.centre = 0, scale = 1, scale.x = 1, r.limit = r.limit, d.sector, text.sector = names(d.sector),
                   n.sectors = 10, grid.circle = TRUE, n.rings = 1, labeltext = TRUE, labelsize = 0.5*text_scaling[i])
      text(x = 0, y = -0.6*lims, plotting_codes[[i]][j], cex = 0.9, col = "black")
      
      dev.off()
    }

  }
}

#------------------------------------------------------------------------------------------------------------
### PLOTTING - MAUCHA/JESTER 
#------------------------------------------------------------------------------------------------------------

if (Maucha) {
  
  # defining plot limits according to data ranges
  maxdata <- max(sqrt(data_nonas))/2.5
  
  if(combine){
    
    lims <- maxdata * 3
    
    # config tiff
    if(format == 'tif'){
      tiff(file_out("Maucha", driv = 'tif'), height = lims * yscale_Maucha*tif_pix, width = lims * no_obs * xscale_Maucha[i]*tif_pix, pointsize = 50*tif_pix/50)
      par(mar=c(1,1,1,1))
    } else if (format == 'pdf'){
      pdf(file_out("Maucha"), height = lims * yscale_Maucha, width = lims * no_obs * xscale_Maucha[i], pointsize = 50)
    }
    
    plot(1:1, xlim=c(0, 2*lims*no_obs), ylim=c(-lims, lims), type="n", 
         xaxt="n", yaxt="n", xlab="", ylab="")
    
    for (j in 1: nrow (data)){
      # isolate data
      series <- data[j,]
      vals <- series
      # plotting
      Maucha_Schot(xm = (2*j-1)*lims, ym = 0, vals = as.numeric(vals), vars = names(data), r.limit = r.limit, cex.label = 0.35*text_scaling[i])
      text(x = (2*j-1)*lims, y = -0.6*lims, plotting_codes[[i]][j], cex = 0.7, col = "black")
    }
    
    dev.off()
    
  } else {
    
    lims <- maxdata * 3.75
    xscale_Maucha <- 10
    
    for (j in 1: nrow (data)){
      
      # config tiff
      if(format == 'tif'){
        tiff(file_out_1("Maucha", code = plotting_codes[[i]][j], driv = 'tif'), height = lims * yscale_Maucha*tif_pix, width = lims * xscale_Maucha[i]*tif_pix, pointsize = 100*tif_pix/50)
        par(mar=c(1,1,1,1))
      } else if (format == 'pdf'){
        pdf(file_out_1("Maucha", code = plotting_codes[[i]][j]), height = lims * yscale_Maucha, width = lims * xscale_Maucha[i], pointsize = 50)
      }
      
      plot(1:1, xlim=c(-lims, lims), ylim=c(-lims, lims), type="n", 
           xaxt="n", yaxt="n", xlab="", ylab="")
      
      # isolate data
      series <- data[j,]
      vals <- series
      # plotting
      Maucha_Schot(xm = 0, ym = 0, vals = as.numeric(vals), vars = names(data), r.limit = r.limit, cex.label = 0.35*text_scaling[i])
      text(x = 0, y = -0.6*lims, plotting_codes[[i]][j], cex = 0.7, col = "black")
      
      dev.off()
    }
    
  }
  
}

}
