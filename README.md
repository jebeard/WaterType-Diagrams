# WaterType-Diagrams
R project and input excel sheet to implement water quality diagrams described in the forthcoming paper "Four new ecohydrologically relevant water quality diagrams" (Schot et al.)

Plotting scripts created by Mike Silberbauer, adapted by Jack Beard. Workflow by Jack Beard.

# Workflow:
1. Download this folder and unzip into a directory of your choosing
2. Open Excel sheet in the "data" folder (input_data.xlsx)
3. Insert your own data as follows:

	a. Copy dummy data worksheet "multi_figure" if data is for multiple observations / "single_figure" if for single
	
	b. Replace dummy data with your own data, keeping column titles the same (can be more/less observations than given, also in mg or meq at this point, R script will convert automatically)
	
	c. Rename the worksheet from "multi_figure" to whatever you want output figure to be called
	
	d. Save and close
	
4. Open "WQ_diagrams.Rproj" (requires R and RStudio installations, see https://cran.r-project.org/bin/windows/base/ and https://www.rstudio.com/products/rstudio/download/)
5. If not already open, open "MASTER.R" script by navigating to it (location = "scripts") in the Files tab of RStudio (usually a tab in bottom right window)
6. In the R script, change the inputs in the "USER INPUTS" section as such:

	a. Change "plot_worksheets" to same names as the data sheets created in step 3
	
	b. Change "units" to data input units
	
	c. Choose which figures to plot (Stiff/Maucha/Sector)
	
	d. Choose if you want figures to plot as individual images (combine = T) or in single row format (combine = F)
	
7. Run R script by pressing "Source" in top right of RStudio
8. Check figures in "figures_out" directory
9. If figures are stretched/non-circular, alter "xscale_sector"/"xscale_maucha" parameters and run again!


# Relevant references
Pascal version of the Maucha diagram: https://github.com/MikeSilberbauer/maucha-ionic-diagram
Silberbauer, M., 2020. Re ReScience challenge: Geographical Trends in the Water Chemistry of Wetlands in the South-Western Cape Province, South Africa. Rescience C, 6.  https://zenodo.org/record/3996198/files/article.pdf (10.5281/ZENODO.3996197). 
