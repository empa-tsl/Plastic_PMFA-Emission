setwd("/Users/leo/Desktop/E-2018.1")

excel.file <- "2021.01.19_FeedData.xlsx"

# source needed functions
source("Code/functions.needed.R")
source("Code/functions.needed.analysis.R")

library("xlsx")
library("trapezoid")

Materials <- c("PUR", "ABS", "PA", "PC", "PMMA")
Systems <- "CH"
SIM <- 10^4

message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Module 1 definition"))
source("/Users/leo/Desktop/E-2018.1/Code/1-InputFormatting.R")
source("/Users/leo/Desktop/E-2018.1/Code/2-SpecialFlowsMod1.R")
message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Module 1 done !"))

message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Module 2 definition"))
source("/Users/leo/Desktop/E-2018.1/Code/3-Merging.R")
source("/Users/leo/Desktop/E-2018.1/Code/4-SpecialFlowsMod2.R")
message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Module 2 done !"))

source("/Users/leo/Desktop/E-2018.1/Code/5-CalculationScript.R")

# Do some plots if required by the user
fun <- function() {
  ANSWER <- readline("Do you want to plot some results now? ")
  
  while(!ANSWER %in% c("Yes", "No", "y", "n", "Y", "N")){
    cat("Oops. Possible answers are Yes, No, y, n, Y, N \n")
    cat("Let's try again \n")
    ANSWER <- readline("Do you want to plot some results now? ")
  }
  
  if(ANSWER %in% c("No", "n", "N")){
    cat("Okay, sure :'( \n")
  } else {
    cat("Let's do this! \n")
    
    # graphs for checking the input into the model
    source("/Users/leo/Desktop/E-2018.1/Code/Graphs_Feed.R")
    
    # print some general numbers
    source("/Users/leo/Desktop/E-2018.1/Code/Print_Total_Emissions.R")
    source("/Users/leo/Desktop/E-2018.1/Code/Print_Total_Emissions_percapita.R")
    
    # create aggregated boxplots
    source("/Users/leo/Desktop/E-2018.1/Code/Graphs_Boxplots.R")
    source("/Users/leo/Desktop/E-2018.1/Code/Graphs_Boxplots_Aggregated.R")
    # source("Code/Graphs_Boxplots_Emissions.R")
    
    # create a stacked barchart of all environmental compartments
    source("/Users/leo/Desktop/E-2018.1/Code/Graphs_Barcharts_Comparison_EnvComp.R")
    
    # code for ranking the flows in terms of relative uncertainty
    source("/Users/leo/Desktop/E-2018.1/Code/Graphs_Flows_Unc.R")

    # create a barplot of the total burdens by environmental compartment
    source("/Users/leo/Desktop/E-2018.1/Code/Graphs_Mass.R")
    # make a violin plot of the final mass in each compartment?
    # code unfinished
    # source("Code/Graphs_VioPlot_Polymer.R")
    
    # graph the TC distributions in a vioplot for textile emissions
    source("/Users/leo/Desktop/E-2018.1/Code/Graphs_TC_Distr_Textiles.R")
    
    # compare my total emissions with values from literature
    source("/Users/leo/Desktop/E-2018.1/Code/Graphs_Litterature_Comparison.R")
    
    # plot the 10 largest sources for each polymer
    # end compartments with macro and MP separately but stacked
    source("/Users/leo/Desktop/E-2018.1/Code/Graphs_FC_Cons.R")
    # create figures of 10 largest sources to air, wastewater, water and soil for macro and MP separately
    # materials are stacked in a barplot
    source("/Users/leo/Desktop/E-2018.1/Code/Graphs_Emissions_to_Air_by_source.R")
    source("/Users/leo/Desktop/E-2018.1/Code/Graphs_Emissions_to_Water_by_source.R")
    source("/Users/leo/Desktop/E-2018.1/Code/Graphs_Emissions_to_WW_by_source.R")
    source("/Users/leo/Desktop/E-2018.1/Code/Graphs_Emissions_to_Soil_by_source.R")
    # create a matrix of barplots showing the distribution of burdens by polymer,
    # type of emission and final environmental compartment
    source("/Users/leo/Desktop/E-2018.1/Code/Graphs_Emissions_to_DetComp_by_Pol.R")
    
    source("/Users/leo/Desktop/E-2018.1/Code/Graphs_Source_To_EnvComp.R")
    
    # graph of final compartment by polymer and type of product similar to the first paper
    source("/Users/leo/Desktop/E-2018.1//Graphs_FinalCompartment.R")
    
    # code for exporting flow data for TikZ
    # aggregated flows by polymer
    source("/Users/leo/Desktop/E-2018.1/Code/Format_Flows_Agg_TikZ_Pol_.R")
    # all flows by polymer and also aggregated for plastic
    source("/Users/leo/Desktop/E-2018.1/Code/Format_Flows_TikZ_All.R")
    
    # create a figure showing the percentage emitted by product category
    source("/Users/leo/Desktop/E-2018.1/Code/Graphs_EmissionFactors.R")
    # create tables of emission factors to export for SI
    source("/Users/leo/Desktop/E-2018.1/Code/Table_EmissionFactors_LCI.R")
    
    # create tables of flows for SI
    source("/Users/leo/Desktop/E-2018.1/Code/Format_Flows_Table_SI.R")
    
  }
    
}

if(interactive()) fun()
