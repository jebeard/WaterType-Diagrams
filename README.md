# WaterType-Diagrams
Created by Jack Beard, 12/08/2021

R project and input excel sheet to implement water quality diagrams from the paper "Four new ecohydrologically relevant water quality diagrams" (Schot et al.)

# Workflow:
1. Download this folder and unzip into a directory of your choosing
2. Open Excel sheet in the "data" folder (input_data.xlsx)
3. Insert your own data as follows:
	a. Copy dummy data worksheet "multi_figure" if data is for multiple observations / "single_figure" if for single
	b. Replace dummy data with your own data, keeping column titles the same 
	   (can be more/less observations than given, also in mg or meq at this point, R script will convert automatically)
	c. Rename the worksheet from "multi_figure" to whatever you want output figure to be called
	d. Save and close
4. Open "WQ_diagrams.Rproj" (requires R and RStudio installations, see https://cran.r-project.org/bin/windows/base/ and https://www.rstudio.com/products/rstudio/download/)
5. If not already open, open "MASTER.R" script by navigating to it (location = "scripts") in the Files tab of RStudio (usually a tab in bottom right window)
6. In the R script, change the inputs in the "USER INPUTS" section as such:
	a. Change "plot_worksheets" to same names as the data sheets created in step 3
	b. Change "units" to data input units
7. Run R script by pressing "Source" in top right of RStudio
8. Check figures in "figures_out" directory
9. If figures are stretched/non-circular, alter "xscale_sector"/"xscale_maucha" parameters and run again!
