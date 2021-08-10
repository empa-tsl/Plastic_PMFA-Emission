setwd("C:\Users\think\Desktop\Code.0119")

message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Starting input Formatting..."))
source("C:\Users\think\Desktop\Code.0119/1-InputFormatting.R")
rm(list = ls())

message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Starting export Calculation..."))
source("C:\Users\think\Desktop\Code.0119/2-ExportCalculation.R")
rm(list = ls())

message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Starting simulation..."))
source("C:\Users\think\Desktop\Code.0119/3-CalculationScript.R")
rm(list = ls())

message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Graphing feed data..."))
source("D:\WORKSPACE\zipeng\4-FeedGraphing.R")
rm(list = ls())

message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Formatting results for TikZ..."))
source("D:\WORKSPACE\zipeng\5-FormatDataForTikZ.R")
rm(list = ls())

message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"),  " Calculating numbers for discussion..."))
source("D:\WORKSPACE\zipeng\6-CalculationNumbersForDiscussion.R")
rm(list = ls())

message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Plotting chosen distributions..."))
source("C:\Users\think\Desktop\Code.0119/Fig_Probabilistic_Plots.R")
rm(list = ls())

message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Starting uncertainty plotting..."))
source("C:\Users\think\Desktop\Code.0119/Fig_UncertaintyAssessment.R")
rm(list = ls())

message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Graphing detailed consumption..."))
source("C:\Users\think\Desktop\Code.0119/Fig_DetailedConsumption_Barplot.R")
rm(list = ls())

message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Graphing consumption and waste management..."))
source("C:\Users\think\Desktop\Code.0119/Fig_Consumption_WasteManagement.R")
rm(list = ls())

message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Graphing final compartment..."))
source("C:\Users\think\Desktop\Code.0119/Fig_FinalCompartment.R")
rm(list = ls())
