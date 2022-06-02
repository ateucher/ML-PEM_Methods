
# Script to generate DEM derived covariates

# read in libraries 

library(RSAGA)
library(raster)
library(sf)
library(rgdal)
library(purrr)
library(dplyr)
library(spatialEco)

source("./_functions/landfClass.R")



create_covariates <- function(saga_dir,  saga_files, cov_type) {
                              
        saga_dir = "E:/transfer/software/saga-7.4.0_x64"
        saga_files = "E:/temp/PEM_DATA/BEC_DevExchange_Work/Buck_AOI/1_map_inputs/covariates/5m"
        cov_type = "stand" # alternative is landscape for sample planning
        
        
# 1) Set up Saga - r link on your machine -----------------------------

 # if(Sys.info()['sysname'] == "Windows"){
 # 
 #        saga_cmd ="C:\\Program Files\\SAGA-GIS\\saga_cmd.exe"
 #        error_check <- system(paste(saga_cmd, "-v"))
 #        if(error_check == 127) {
 #                print("Cannot find saga_cmd.exe please define filepath")
 #                print("for example : saga_cmd = `E:/transfer/software/saga-7.4.0_x64`")
 #            }
 #        }
           
        saga_cmd <- list.files(saga_dir, recursive = TRUE, 
                               pattern = "saga_cmd.exe", full.names = TRUE)
        
        # test if saga is working. If the response is 127 this is an error - check filepath again.
        system(paste(saga_cmd, "-v"))


# 2) Set up output folder for saga temp folder and output folder 
        
tmpOut <- file.path(saga_files, "sagaTmp")

ifelse(!dir.exists(file.path(tmpOut)),               
       dir.create(file.path(tmpOut)), FALSE)

ifelse(!dir.exists(file.path(saga_files, "outputs")),
       dir.create(file.path(saga_files, "outputs")), FALSE)


# 3) set up base raster DTM

        dem_files <- list.files(saga_files, 
                                 pattern = "dem.tif$", 
                                 full.names = TRUE, 
                                 recursive = TRUE, 
                                 ignore.case = TRUE)
        
        DTM <- raster(file.path (dem_files)[1])
        
        # ensure raster is in BC albers projection
        PROJ <- crs(paste("+init=epsg:", 3005, sep = "")) 
        crs(DTM) <- PROJ 
        
        # convert dem to saga format by first converting to gtiff
        ## Bit of a hack here -- SAGA does not like the output from raster package
        ## save it as gTiff, re-open using rgdal and export as SAGA ...
        
        rDTM <- file.path(tmpOut, "dem.tif") 
        
        raster::writeRaster(DTM,  rDTM, drivername = "GTiff", overwrite = TRUE)  # save SAGA Version using rgdal
        
        dtm <- rgdal::readGDAL(rDTM)
        
        sDTM <- file.path( tmpOut,"dem.sdat")
        
        
        ## If the file exists delete and save over.
        if(file.exists(sDTM)){
                unlink(sDTM)
                rgdal::writeGDAL(dtm, sDTM, drivername = "SAGA")  ## TRUE
        } else {
                rgdal::writeGDAL(dtm, sDTM, drivername = "SAGA" )               ## FALSE
        }
        ## END HACK ------------------
        

# set function to convert .sdat to tif and assign projection 

sdat_to_tif <- function(infile, out_crs = 3005){
        
        #infile <- dahfile
        # sagafile <- rasterfiles[iii] # 5
        sagafile <- basename(infile)
        print (sagafile)
        outfile <- gsub("sdat", "tif", sagafile)
        r <- readGDAL(file.path(saga_files,"sagaTmp",sagafile))
        w <- file.path(saga_files, "outputs", outfile) #, sep = "")
        writeGDAL(r, w)
        
        rfile <- raster(w)
        
        if(is.na(crs(rfile))) {
                print("setting projection to 3005")
                # ensure raster is in BC albers projection
                PROJ <- crs(paste("+init=epsg:",out_crs, sep = "")) 
                crs(rfile) <- PROJ 
                
        } else {
                print("projection already set to 3005")
        }
        
        w_out <- writeRaster(rfile, file.path(saga_files, basename(w)), overwrite = TRUE, driver = "GTiff")
        
} 


##############################################################################

# Step 1 preprocess DEM 

# Fill sinks in dem to prepare base DEM for other layers: 

#### Tool 1: preprocess DEM 

## http://www.saga-gis.org/saga_tool_doc/2.2.2/ta_preprocessor_5.html
## Module Fill Sinks XXL (Wang & Liu)

# This module uses an algorithm proposed by Wang & Liu to identify and fill surface depressions in digital elevation models.
# The method was enhanced to allow the creation of hydrologic sound elevation models, i.e. not only to fill the depression(s) but also to preserve a downward slope along the flow path. If desired, this is accomplished by preserving a minimum slope gradient (and thus elevation difference) between cells.
# This version of the module is designed to work on large data sets (e.g. LIDAR data), with smaller datasets you might like to check out the fully featured standard version of the module.

# WARNING : 
# note this process may also fill outside the boundary of the AOI. Need to check carefully as this
# will impact the other derivatives. 

# if the extent outside the boundary is given a value, we need to write this out , assign no data
# values and then write back to the .

sinksFilled <- file.path(tmpOut, "filledsinks.sgrd")
sysCMD <- paste(saga_cmd, "ta_preprocessor 5", "-ELEV" ,
                sDTM,
                "-FILLED", sinksFilled,
                "-MINSLOPE ", 0.1)
system(sysCMD)

sDTM <- sinksFilled


# Preprocessing : Option 2: another method to generate preproces of dem. 

# Generate sink drainage route detection layer to use in preprocess DEM - this is needed in other covariates
# Perfomring checks on comparison of filled sinks and preprocess shows very little difference. 
# While raw dem compared to preprocessed of filled shows much more variation 


## WARNING THIS WILL GENERATE VALUES OUTSIDE OF BOUNDARY

# original script uses ta_preprocessor tool 1 - sink drainage and rout 
#http://www.saga-gis.org/saga_tool_doc/2.2.2/ta_preprocessor_1.html

sinksRoute <- file.path(tmpOut, "sinkroute.sgrd")

sysCMD <- paste(saga_cmd, "ta_preprocessor 1", 
                "-ELEVATION" , sDTM,        
                "-SINKROUTE", sinksRoute                              
                                                   
)
system(sysCMD)

# preproces DEM version 2:  fills sinks (input requires DEM + sink detection layer 
# generated above)/
#http://www.saga-gis.org/saga_tool_doc/2.2.2/ta_preprocessor_2.html


dem_preproc <- "dem_preproc.sgrd"
dem_preproc <- file.path(tmpOut, dem_preproc)

sysCMD <- paste(saga_cmd, "ta_preprocessor 2", 
                "-DEM" , sDTM,        
                "-SINKROUTE", sinksRoute,
                "-DEM_PREPROC", dem_preproc,
                "-METHOD", 1,
                "-THRESHOLD", 0
                
)
system(sysCMD)





if(cov_type == "landscape") {

######################################################################################

# Step 2: Generating layers for stage 1 sampling (25m ONLY)

#####################################################################################

# For the stage 1 sampling we need to create specifc landscape scale metrics including 
# - Dem
# - MRVBF with groups
# - DAH with three groups 
# - Landform (TPI metrics)

dem_template <- raster(file.path(saga_files,"template.tif"))

# 1) MRVBF  - this generates 5-6 classes 

MRVBF6 <- "mrvbf_LS.sgrd"
MRRTF6 <- "mrrtf_LS.sgrd"

MRVBF6 = file.path(tmpOut, MRVBF6)
MRRTF6  = file.path(tmpOut, MRRTF6)

sysCMD <- paste(saga_cmd, "ta_morphometry 8", "-DEM",
                file.path(gsub("sdat","sgrd", dem_preproc)),
                "-MRVBF", MRVBF6,
                "-MRRTF", MRRTF6,                       
                "-T_SLOPE", 64,
                "-T_PCTL_V", 6,
                "-T_PCTL_R", 2,   
                "-P_SLOPE", 4.0,
                "-P_PCTL", 3.0,
                "-UPDATE", 1,
                "-CLASSIFY", 1,
                "-MAX_RES", 100
)
system(sysCMD)


# write to tif 
mrvbfile <- list.files(file.path(saga_files,"sagaTmp"), pattern = "mrvbf_LS.sdat", full.names = T)
sdat_to_tif(mrvbfile)

# sieve 
mrv_file <- list.files(saga_files, pattern = "mrvbf_LS.tif", full.names = TRUE )
mrv  <- raster(mrv_file)
mrv_s <- file.path(saga_files,"mrvbf_LSs_test.tif")

## # set up a sieve python command - NOTE Still not working  
# saga_cmd = "saga_cmd"}  

infile = mrv_file 
outfile = mrv_s

###
#py.sieve <- "C:\\Program Files\\QGIS 3.18\\apps\\Python37\\Scripts\\gdal_sieve.py"
#gdal_sieve <- paste(py.sieve,
#                    "-st", 
#                    10, -4, 
#                    infile, 
#                    "-of","GTiff",
#                    outfile
#                    )
#
#system(gdal_sieve)

# # set up a sieve python command - NOTE Still not working  
# saga_cmd = "saga_cmd"}  
# 
# gdal_sieve [-q] [-st threshold] [-4] [-8] [-o name=value]
# srcfile [-nomask] [-mask filename] [-of format] [dstfile]
# 
# python3 -m 
# gdal_sieve -st 10 -4 -of GTiff "crs=EPSG:3857&format&type=xyz&url=http://ecn.t3.tiles.virtualearth.net/tiles/a%7Bq%7D.jpeg?g%3D1&zmax=18&zmin=0" C:/Users/genperk/AppData/Local/Temp/processing_SOaSPr/77b725a00d4f44ffa9d85dbc4840e7a5/OUTPUT.tif
# 
# python3 -m gdal_sieve -st 250 -8 -of GTiff D:/PEM_DATA/BEC_DevExchange_Work/WilliamsLake_AOI/1_map_inputs/covariates/25m/bgc.tif D:/PEM_DATA/BEC_DevExchange_Work/WilliamsLake_AOI/0_raw_inputs/base_layers/raw/sieve.tif
# 

# infile <- "WilliamsLake_AOI/1_map_inputs/covariates/25m/bgc.tif"
# outfile <- "WilliamsLake_AOI/1_map_inputs/covariates/25m/bgc_s.tif"
# 
# gdal_cmd 
# 
# 
# sysCMD = paste(gdal_cmd , 
#                "-st", 250,           # sieving
#                -8,                  # connectedness
#                "-of","GTiff", 
#                mrv_file, mrv_s )
# 
# system(sysCMD)
# sysCMD = paste("gdal_sieve", 
#                "-st", 250,           # sieving
#                -8,                  # connectedness
#                "-of","GTiff", 
#                infile, outfile )
# 
# system(sysCMD)
# 
# 



# IN QGIS  sieve via QGIS gdal or can use below but suboptimal option

# sieved threshold Date Creek - 250
# tick connectness = 8 
# filename = mvvbf_LS_s.tif


# Tested this but not very good. 
# perhaps could add to gdal utiles? or code this into this script (IMPROVEMENT TO BE COMPLETED)
# library(bfastSpatial)                 
# mrv1 <- areaSieve(mrv, thresh = 10, directions = 8, verbose = TRUE,
#                   keepzeros = FALSE, cores = 5)
#writeRaster(mrv1 , file.path(saga_files, "mrvbf_ls_s3.tif"))







##### >> 12 -- Diuranal Anisotropic Heating -----------------------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_12.html

dAH <- "dah.sgrd"
dAH = file.path(tmpOut, dAH)
sysCMD <- paste(saga_cmd, "ta_morphometry 12", "-DEM",
                file.path(gsub("sdat","sgrd", dem_preproc)), # Input DTM
                "-DAH", dAH,                                            # Output
                "-ALPHA_MAX", 202.5                                     # Default Parameters
)
system(sysCMD)

# convert to tif
dahfile <- list.files(file.path(saga_files,"sagaTmp"), pattern = "dah.sdat", full.names = T)
sdat_to_tif(dahfile)

# reclass 

dah <- list.files(file.path(saga_files), pattern = "dah.tif", full.names = TRUE)
dah <- raster(dah)

# set threshold value
threshold <- 0.2

# Build a reclass matrix for three group using (+/- threshold)
# all values > 0 and <= 0.25 become 1, etc.
# Wetzinkwa on the 0.3 threshold
# PeterHope on the 0.2 threshold

m <- c( -10, (threshold*-1), 1,  
        (threshold*-1 ), threshold, 2,  
        threshold, 10,  3)

rclmat <- matrix(m, ncol=3, byrow =TRUE)
rc <- reclassify(dah , rclmat)
#crs(rc) <- PROJ
#rc <- crop(rc, dem_template)

writeRaster(rc, file.path(saga_files, "dah_LS.tif"), driver = "GTiff", overwrite = TRUE)


#lsrast<- list.files(saga_files, full.names = TRUE, pattern = ".tif")
#st <- stack(lsrast)

# filter based on the 0.3 for Date Cree
# - review the slope (style into three classes
#                     - <25% slope or 0.43 radians,
#                     - 45% slope or 0.43 - 0.78 radians 
#                     - >45% slope
#                     - once this is stlyed then you can adjust the grouping on the DAH to match
#                     - Deception = -0.2 to 0.2. 
#                     - Date Creek = -0.3 to 0.3. 
#                     - Peter Hope = -0.2 to 0.2



# 4) Landform classification 

# I was having lots of errors with the edge effect in SAGA calculating tpi metrics so calculated 
# in R instead. 

# we can calculate 6 class landscape format, using a script based on the tpi calculations within
# spatialEco package and then wrapped into function found in the old version of the package 
# GmAMisc (https://github.com/gianmarcoalberti/GmAMisc/blob/master/R/landfClass.R). 
# As this is an old version I copied the function into our function folder for convenience of use. 

# I tested versious options. Increasing the scale means a smoother general format, although the increase
# in size reduces the aoi due to edge effect of surrounding pixal calculations. 
# NOte scale needs to be an odd number
# sn and ln are only valid when using the "ten" classes. 
# tpi parameters are only if you want a tpi exported as part of the process. 
# The best option was scale = 75, which was a compromise of generalisation and edge loss

library(spatialEco)
# note make sure your DEM does not contain data outside the AOI as this impacts the calculations

DTM <- raster(file.path(saga_files,"dem.tif"))
#DTM <- raster(file.path(trim_files,"dem.tif"))

lfclass <- landfClass(DTM, scale = 75, sn = 3, ln = 7, n.classes = "six")

# convert to classes based on the grouping 

writeRaster(lfclass, file.path(saga_files, "landform_LS.tif"), overwrite = TRUE)

# these last paramerts look good. 

# IN QGIS  sieve via QGIS gdal or can use below but suboptimal option

# sieved threshold Date Creek - 250
# tick connectness = 8 
# filename = landform_ls.tif

# we can reclass to remove the extreme values outside the AOI  

lfclass <- list.files(file.path(saga_files), pattern = "landform_LSs.tif", full.names = TRUE )
lfclass <- raster(lfclass)

lfclass <- crop(lfclass, dah)
# set threshold value
threshold <- 0

# Build a reclass matrix for three group using (+/- threshold)
# all values > 0 and <= 0.25 become 1, etc.

values(lfclass)[values(lfclass) < threshold] = NA

writeRaster(lfclass, file.path(saga_files, "lfclass_st.tif"), driver = "GTiff", overwrite = TRUE)






## For the stage 1 sampling if needed crop to the Template size


files_to_crop <- c("lfclass_st.tif", "dah_LS.tif", "mrvbf_LSs.tif")

dem_template

for (i in files_to_crop){
        #i <- files_to_crop[1]
        
        trim_size <- raster(file.path(saga_files, i))
        dem_size <- crop(trim_size, dem_template)
        writeRaster(dem_size, file.path(saga_dir, i), format = "GTiff", overwrite = TRUE)
        
                }

        } else { # loop for landscape vs stand covariate generation 


####################################################################################

## Step 3: Generate base covariates for models

#################################################################################

##### >> 1 -- Slope Aspect and Curvature -------------------------------
# note getting some errors with the gencurve and totcurve. Suspect this might be edge 
# effect problem that was occuring in tpi 

# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_0.html
slope <- "slope.sgrd"
slope = file.path(tmpOut, slope)
aspect <- "aspect.sgrd"
aspect = file.path(tmpOut, aspect)
gencurve <- "gencurve.sgrd"
gencurve = file.path(tmpOut, gencurve)
totcurve <- "totcurve.sgrd"
totcurve = file.path(tmpOut, totcurve)

sysCMD <- paste(saga_cmd, "ta_morphometry 0", "-ELEVATION", 
                sDTM,     # Input DTM
                "-SLOPE", slope, 
                "-ASPECT", aspect,                     # Outputs
                "-C_GENE", gencurve, 
                "-C_TOTA", totcurve,                # Outputs
                "-METHOD", 6, 
                "-UNIT_SLOPE", 0,     # degrees
                "-UNIT_ASPECT", 0       # Default Parameters
)
system(sysCMD)




##### >> 2 -- Total Catchment Area --------------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_hydrology_0.html
## Note this is the same as flow Accumulation top down (#19 although less outputs included here that are included in #19

 tCatchment <- "tCatchment.sgrd"
 tCatchment = file.path(tmpOut, tCatchment)
 sysCMD <- paste(saga_cmd, "ta_hydrology 0", "-ELEVATION", 
                 sDTM, 
                 "-FLOW", tCatchment,                                    # Output
                 "-METHOD", 4                                            # Default Parameters
 )
 system(sysCMD)

 
#####################
# Still to run - using tCatchement instead for the base tca raster for other inputs 
# This is not working properly but is the equivalent to tca - needs more work 

#  #   Following this method for calculating topographic wetness index:
# #    https://gracilis.carleton.ca/CUOSGwiki/index.php/Enhanced_Wetness_Modelling_in_SAGA_GIS
# #    See this paper as well for discussion on different ways to calculate TWI:
# #   https://link.springer.com/article/10.1186/s40965-019-0066-y

##### >> 3a -- Total Catchment Area --------------------------------------

tca <- "tca1.sgrd"
tca <- file.path(tmpOut, tca)
flowlength4 <- "flowlength1.sgrd"
flowlength4 <- file.path(tmpOut, flowlength4)

sysCMD <- paste(saga_cmd, "ta_hydrology 1", 
                "-ELEVATION",sDTM, 
                "-FLOW", tca,    
                "-FLOW_LENGTH", flowlength4,
                "-FLOW_UNIT", 1,
                "-METHOD", 3                                            
)
system(sysCMD)

####################

# try other methods 
# 
# # # no difference when using method (#3) and 2 and 1 : 
# 
#      # flow_accum = paste0(
#      #   "<tool library='ta_hydrology' tool='1' name='Flow Accumulation (Recursive)'>
#      #      <input id='ELEVATION'>dem_preproc</input>
#      #      <output id='FLOW'>tca2</output>
#      #      <output id='FLOW_LENGTH'>flowlength2</output>
#      #      <option id='FLOW_UNIT'>1</option>
#      #      <option id='METHOD'>2</option>
#      #  </tool>"
#      # ),
# 
#      # flow_accum = paste0(
#      #   "<tool library='ta_hydrology' tool='1' name='Flow Accumulation (Recursive)'>
#      #      <input id='ELEVATION'>dem_preproc</input>
#      #      <output id='FLOW'>tca1</output>
#      #      <output id='FLOW_LENGTH'>flowlength1</output>
#      #      <option id='FLOW_UNIT'>1</option>
#      #      <option id='METHOD'>1</option>
#      #  </tool>"# 
# 


##### >> 4 -- Flow Width and Specific Catchment Area --------------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_hydrology_19.html

sCatchment <- "sCatchment.sgrd"
sCatchment = file.path(tmpOut, sCatchment)


sysCMD <- paste(saga_cmd, "ta_hydrology 19", "-DEM", sDTM,       # Input from 1
                "-SCA", sCatchment,                                     # Output
                "-TCA", tCatchment,                                     # Input from 2
                "-METHOD", 1                                            # Parameters
)
system(sysCMD)


##### >> 5 -- Topographic Wetness Index --------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_hydrology_20.html

twi <- "twi.sgrd"
twi = file.path(tmpOut, twi)

sysCMD <- paste(saga_cmd, "ta_hydrology 20", 
                "-SLOPE", slope,           # Input from 11
                "-AREA", sCatchment,                                    # Input from 3
                "-TWI", twi,                                            # Output
                "-CONV",1,  
                "-METHOD", 1                           
)
system(sysCMD)


##### >> 6 -- Channel Network -------------------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_channels_0.html
# https://sourceforge.net/projects/saga-gis/files/SAGA%20-%20Documentation/SAGA%20Documents/SagaManual.pdf/download

channelsNetwork <- "cnetwork.sgrd"
channelsNetwork = file.path(tmpOut, channelsNetwork)

sysCMD <- paste(saga_cmd, "ta_channels 0",
                "-ELEVATION", sDTM ,     # Input from 1
                "-CHNLNTWRK", channelsNetwork,                            # Output
                "-INIT_GRID", tCatchment,                                 # Input from 2
                "-INIT_VALUE", 1000000,
                "-INIT_METHOD", 2,                # Based on SAGA Manual Documentation, p. 119
                "-DIV_CELLS", 5.0,
                "-MINLEN", 10.0                        # Default Parameters
)
system(sysCMD)


##### >> 7 -- Overland Flow Distance to Channel Network -----------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_channels_4.html

hDistance <- "hdist.sgrd"
vDistance  <- "vdist.sgrd"
hDistance = file.path(tmpOut, hDistance)
vDistance = file.path(tmpOut, vDistance)

sysCMD <- paste(saga_cmd, "ta_channels 4", 
                "-ELEVATION", sDTM,   # Input from 1
                "-CHANNELS", channelsNetwork,                             # Input from 4
                "-DISTANCE", hDistance, 
                "-DISTVERT", vDistance,           # Outputs
                "-METHOD", 1, 
                "-BOUNDARY", 1                              # Parameters
)
system(sysCMD)

# note distnob created using XML script with no boundary. This shows NA for areas on the edge where
# metrics cannot be calculated) 


hDistance <- "hdistnob.sgrd"
vDistance  <- "vdistnob.sgrd"
hDistance = file.path(tmpOut, hDistance)
vDistance = file.path(tmpOut, vDistance)

sysCMD <- paste(saga_cmd, "ta_channels 4", 
                "-ELEVATION", sDTM,   # Input from 1
                "-CHANNELS", channelsNetwork,                             # Input from 4
                "-DISTANCE", hDistance, 
                "-DISTVERT", vDistance,           # Outputs
                "-METHOD", 1, 
                "-BOUNDARY", 0                              # Parameters
)
system(sysCMD)


##### >> 8 -- MRVBF -----------------------------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_8.html

MRVBF <- "mrvbf.sgrd"
MRRTF <- "mrrtf.sgrd"

MRVBF = file.path(tmpOut, MRVBF)
MRRTF  = file.path(tmpOut, MRRTF)

# use defaul parameters
sysCMD <- paste(saga_cmd, "ta_morphometry 8", "-DEM",
                sDTM,
                "-MRVBF", MRVBF,
                "-MRRTF", MRRTF,                       # Outputs
                "-T_SLOPE", 16,
                "-T_PCTL_V", 0.4,
                "-T_PCTL_R", 0.35,    # Default Parameters
                "-P_SLOPE", 4.0,
                "-P_PCTL", 3.0,
                "-UPDATE", 0,
                "-CLASSIFY", 0,
                "-MAX_RES", 100
)
system(sysCMD)
 
# Test a Variety of paramter and method versions. 
# tested a number of MRVBF options for the t-slope parameter #ie 
# use dem_preproces for input and lowered the slope parameter from 15 to 10 
#
 MRVBF2 <- "mrvbf2.sgrd"
 MRRTF2 <- "mrrtf2.sgrd"
 
 MRVBF2 = file.path(tmpOut, MRVBF2)
 MRRTF2  = file.path(tmpOut, MRRTF2)
 
 #  Adjust parameters -  Option 2. 
 sysCMD <- paste(saga_cmd, "ta_morphometry 8", "-DEM",
                 sDTM,
                 "-MRVBF", MRVBF2,
                 "-MRRTF", MRRTF2,                       
                 "-T_SLOPE", 10,
                 "-T_PCTL_V", 0.4,
                 "-T_PCTL_R", 0.35,    
                 "-P_SLOPE", 4.0,
                 "-P_PCTL", 3.0,
                 "-UPDATE", 0,
                 "-CLASSIFY", 0,
                 "-MAX_RES", 100
 )
system(sysCMD)


##### >> 9 -- Terrain Ruggedness Index ----------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_16.html

TRI <- "tri.sgrd"
TRI  = file.path(tmpOut, TRI)
sysCMD <- paste(saga_cmd, "ta_morphometry 16", 
                "-DEM",sDTM, 
                "-TRI", TRI,  # Output
                "-MODE", 0, 
                "-RADIUS", 3.0, 
                "-DW_WEIGHTING", 0          # Parameters
)
system(sysCMD)


##### >> 10 -- Convergence Index -----------------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_1.html

convergence <- "convergence.sgrd"
convergence  = file.path(tmpOut, convergence)
sysCMD <- paste(saga_cmd, "ta_morphometry 1",
                "-ELEVATION ", sDTM,      # Input DTM
                "-RESULT", convergence,                                 # Output
                "-METHOD", 1,
                "-NEIGHBOURS", 1                          # Parameters
)
system(sysCMD)


##### >> 11 -- Openness --------------------------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_lighting_5.html

opos <- "open_pos.sgrd"
opos = file.path(tmpOut, opos)
oneg <- "open_neg.sgrd"
oneg = file.path(tmpOut, oneg)
sysCMD <- paste(saga_cmd, "ta_lighting 5", "-DEM", 
               sDTM,
                "-POS", opos, 
                "-NEG", oneg,                               # Outputs
                "-RADIUS", 1000, 
                "-METHOD", 0,                          
                "-DLEVEL",  3, 
                "-NDIRS", 8
)
system(sysCMD)



##### >> 13 -- Topographic Position Index --------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_18.html

tpi <- "tpi.sgrd"
tpi= file.path(tmpOut, tpi)

sysCMD <- paste(saga_cmd, "ta_morphometry 18", "-DEM", 
               sDTM ,# Input DTM
                "-TPI", tpi,                                            # Output
                "-STANDARD", 0, 
                "-RADIUS_MIN", 0, 
                "-RADIUS_MAX", 100,   # Default Parameters
                "-DW_WEIGHTING", 0, 
                "-DW_IDW_POWER", 1, 
                "-DW_IDW_OFFSET", 1, 
                "-DW_BANDWIDTH", 75
)
system(sysCMD)


#### >> 14 -- Valley Depth ------------------------------------- 
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_channels_7.html

val_depth = "val_depth.sgrd"
val_depth = file.path(tmpOut, val_depth)

RidgeLevel = "rid_level.sgrd"
RidgeLevel = file.path(tmpOut, RidgeLevel)

sysCMD = paste(saga_cmd, "ta_channels 7", 
               "-ELEVATION", sDTM,            # input DEM
               "-VALLEY_DEPTH", val_depth,         # output Valley Depth
               "-RIDGE_LEVEL", RidgeLevel,           # output Ridge Level
               "-THRESHOLD", 1,
               "-NOUNDERGROUND", 1,
               "-ORDER", 4
)
system(sysCMD)




#### >> 15 -- Melton Ruggedness Number -------------------------- ## works
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_23.html

MRNCatchment = "mnr_area.sgrd"
MRNCatchment = file.path(tmpOut, MRNCatchment)
MRNMaxHeight = "mnr_mheight.sgrd"
MRNMaxHeight = file.path(tmpOut, MRNMaxHeight)
MRN = "mnr.sgrd"
MRN = file.path(tmpOut, MRN)

sysCMD = paste(saga_cmd, "ta_hydrology 23", 
               "-DEM", sDTM,                 # input DEM
               "-AREA", MRNCatchment,               # output MRN Catchment
               "-ZMAX", MRNMaxHeight,               # output MRN Max Height
               "-MRN", MRN                          # output MRN
)
system(sysCMD)


#### >> 16 -- Flow Accumulation (Flow Tracing)
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_2.html

FlowAccumFT = "flow_accum_ft.sgrd"
FlowAccumFT = file.path(tmpOut, FlowAccumFT)
MeanOvCatch = "MeanOvCatch.sgrd"
MeanOvCatch = file.path(tmpOut, MeanOvCatch)
AccumMaterial = "AccumMaterial.sgrd"
AccumMaterial = file.path(tmpOut, AccumMaterial)
sysCMD = paste(saga_cmd, "ta_hydrology 2",
               "-ELEVATION", sDTM,            # input DEM
               "-FLOW", FlowAccumFT,                 # output Flow Accumulation
               "-VAL_MEAN", MeanOvCatch,             # output Mean over Catchment
               "-ACCU_TOTAL", AccumMaterial,         # output Accumulated Material
               "-FLOW_UNIT", 1,
               "-METHOD", 1,
               "-MINDQV", 0
)
system(sysCMD)


#### >> 17 -- Slope Length --------------------------------------- ## works
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_7.html

SlopeLength = "slength.sgrd"
SlopeLength = file.path(tmpOut, SlopeLength)
sysCMD = paste(saga_cmd, "ta_hydrology 7",
               "-DEM", sDTM,             # input DEM
               "-LENGTH", SlopeLength            # output Slope Length
)
system(sysCMD)
 


#### >> 18 -- Flow Accumulation (Parallelizable) ------------------- ## this tool doesn't seem to exist - SAGA version issue?
# # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_29.html

FlowAccumP = "flow_accum_p.sgrd"
FlowAccumP = file.path(tmpOut, FlowAccumP)
sysCMD = paste(saga_cmd, "ta_hydrology 29",
               "-DEM", sDTM,                  # input DEM
               "-FLOW", FlowAccumP,                  # output Flow Accumulation
               "-METHOD", 2,
               "-CONVERGENCE", 1.1
)
system(sysCMD)


#### >> 19 -- Flow Accumulation (Top-Down) ---------------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_0.html
#

FlowAccumTD = "flow_accum_td.sgrd"
FlowAccumTD = file.path(tmpOut, FlowAccumTD)
MeanOvCatchTD = "MeanOvCatchTD.sgrd"
MeanOvCatchTD = file.path(tmpOut, MeanOvCatchTD)
AccumMaterialTD = "AccumMaterialTD.sgrd"
AccumMaterialTD = file.path(tmpOut, AccumMaterialTD)
FlowPathLenTD = "FlowPathLenTD.sgrd"
FlowPathLenTD = file.path(tmpOut, FlowPathLenTD)
sysCMD = paste(saga_cmd, "ta_hydrology 0",
               "-ELEVATION", sDTM,                 # input DEM
               "-FLOW", FlowAccumTD,                      # output Flow Accumulation
               "-VAL_MEAN", MeanOvCatchTD,                # output Mean over Catchment
               "-ACCU_TOTAL", AccumMaterialTD,            # output Accumulated Material
               "-FLOW_LENGTH", FlowPathLenTD,             # output Flow Path Length
               "-FLOW_UNIT", 1,
               "-METHOD", 4,
               "-LINEAR_DO", 1,
               "-LINEAR_MIN", 500,
               "-CONVERGENCE", 1.1
)
system(sysCMD)


# DAH 

dAH <- "dah.sgrd"
dAH = file.path(tmpOut, dAH)
sysCMD <- paste(saga_cmd, "ta_morphometry 12", "-DEM",
                sDTM, # Input DTM
                "-DAH", dAH,                                            # Output
                "-ALPHA_MAX", 202.5                                     # Default Parameters
)
system(sysCMD)




#### >> 20 -- Stream Power Index ----------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_21.html

# Not included as binary output 

# StreamPower = "spower.sgrd"
# StreamPower = file.path(tmpOut, StreamPower)
# sysCMD = paste(saga_cmd, "ta_hydrology 21",
#                "-SLOPE", slope,                    # input Slope
#                "-AREA", tCatchment,                # input Catchment Area
#                "-SPI", StreamPower,               # output Stream Power Index
#                "-CONV", 0
# )
# system(sysCMD)


#### >> 21 -- Maximum Flow Path Length --------------------------- ## works
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_27.html

FlowPathLength = "max_fp_l.sgrd"
FlowPathLength = file.path(tmpOut, FlowPathLength)
sysCMD = paste(saga_cmd, "ta_hydrology 27",
               "-ELEVATION", sDTM,            # input DEM
               "-DISTANCE", FlowPathLength,          # output Max Flow Path Length
               "-DIRECTION", 0
)
system(sysCMD)



#### >> 21a -- Maximum Flow Path Length --------------------------- ## works
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_27.html
#
FlowPathLength = "max_fp_l1.sgrd"
FlowPathLength = file.path(tmpOut, FlowPathLength)
sysCMD = paste(saga_cmd, "ta_hydrology 27",
               "-ELEVATION", sDTM,            # input DEM
               "-DISTANCE", FlowPathLength,          # output Max Flow Path Length
               "-DIRECTION", 1
)
system(sysCMD)
# 


#### >> 22 -- Slope Limited Flow Accumulation ------------------- ## works 
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_26.html
#
# NEW VARIABLE - not generated for Deception 
#
FlowAccum = "slope_lts_fa.sgrd"
FlowAccum = file.path(tmpOut, FlowAccum)
sysCMD = paste(saga_cmd, "ta_hydrology 26",
               "-DEM", sDTM,               # input DEM
               "-FLOW", FlowAccum,                # output Flow Accumulation
               "-SLOPE_MIN", 0,
               "-SLOPE_MAX", 5,
               "-B_FLOW", 0
)
system(sysCMD)

#### >> 23 -- LS Factor -----------------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_22.html

LSFactor = "ls_factor.sgrd"
LSFactor = file.path(tmpOut, LSFactor)
sysCMD = paste(saga_cmd, "ta_hydrology 22",
               "-SLOPE", slope,                # input Slope
               "-AREA", tCatchment,            # input Catchment Area
               "-LS", LSFactor,                # output LS Factor
               "-CONV", 0,
               "-METHOD", 0,
               "-EROSIVITY", 1,
               "-STABILITY", 0
)
system(sysCMD)

#### >> 24 -- Solar covariates  -----------------------------------------
# solar direct and diffuse solar radiation 
# adjust min and max limits to 4am and 22 pm to reduce processing time
#http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_lighting_2.html
#Calculation of potential incoming solar radiation (insolation). Times of sunrise/sunset will only be calculated if time span is set to single day.


DirInsol <- "direinso.sgrd"
DirInsol = file.path(tmpOut, DirInsol)
DifInsol <- "diffinso.sgrd"
DifInsol = file.path(tmpOut, DifInsol)

sysCMD <- paste(saga_cmd, "ta_lighting 2", 
                "-GRD_DEM", sDTM ,# Input DTM
                "-GRD_DIRECT", DirInsol, 
                "-GRD_DIFFUS", DifInsol,       # Outputs
                 "-SOLARCONST", 1367, 
                 "-LOCALSVF", 1, 
                 "-SHADOW", 0,     # Parameters
                 "-LOCATION", 1, 
                 "-PERIOD", 2, 
                 "-DAY", "2018-02-15", 
                 "-DAY_STOP", "2019-02-15", 
                 "-DAYS_STEP", 30, 
                 "-HOUR_RANGE_MIN", 4, 
                 "-HOUR_RANGE_MAX", 22, 
                 "-HOUR_STEP", 0.5, 
                 "-METHOD", 2, 
                  "-LUMPED", 70
)
system(sysCMD)


#### >> 25 -- Terrain Surface Convexity ---------------------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_21.html


Convexity = "convexity.sgrd"
Convexity = file.path(tmpOut, Convexity)
sysCMD = paste(saga_cmd, "ta_morphometry 21", 
               "-DEM", sDTM,                   # input DEM
               "-CONVEXITY", Convexity,               # output Convexity
               "-KERNEL", 0,
               "-TYPE", 0,
               "-EPSILON", 0,
               "-SCALE", 10,
               "-METHOD", 1,
               "-DW_WEIGHTING", 0,
               "-DW_IDW_POWER", 2, 
               "-DW_BANDWIDTH", 1
)
system(sysCMD)


#### >> 26 -- Vertical Distance to Channel Network ------------- ## froze - maybe just very slow?
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_channels_3.html
VertDistance = "vert_dist.sgrd"
VertDistance = file.path(tmpOut, VertDistance)
#sinksFilled

sysCMD = paste(saga_cmd, "ta_channels 3", 
               "-ELEVATION", sDTM,            # input DEM
               "-CHANNELS", channelsNetwork,         # input Channel Network
               "-DISTANCE", VertDistance,            # output 
               "-THRESHOLD", 1,
               "-NOUNDERGROUND", 1
)
system(sysCMD)



#### >> 27 -- TCI Low -------------------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_24.html

TCILow = "tci_low.sgrd"
TCILow = file.path(tmpOut, TCILow)
sysCMD = paste(saga_cmd, "ta_hydrology 24", 
               "-DISTANCE", VertDistance,            # input Vertical Distance to Channel Network
               "-TWI", twi,                          # input TWI
               "-TCILOW", TCILow                     # output TCI Low
)
system(sysCMD)




#### >> 28 -- SAGA Wetness Index -------------------------------- ## works but VERY slow (~18 hours)
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_15.html

CatchmentArea = "swi_area.sgrd"
CatchmentArea = file.path(tmpOut, CatchmentArea)
CatchmentSlope = "swi_slope.sgrd"
CatchmentSlope = file.path(tmpOut, CatchmentSlope)
ModCatchmentArea = "swi_area_mod.sgrd"
ModCatchmentArea = file.path(tmpOut, ModCatchmentArea)
TopoWetIndex = "swi_twi.sgrd"
TopoWetIndex = file.path(tmpOut, TopoWetIndex)
sysCMD = paste(saga_cmd, "ta_hydrology 15", 
               "-DEM", sDTM,                  # input DEM
               "-AREA", CatchmentArea,              # output Catchment Area
               "-SLOPE", CatchmentSlope,            # output Catchment Slope
               "-AREA_MOD", ModCatchmentArea,       # output Modified Catchment Area
               "-TWI", TopoWetIndex,                # output TWI
               "-SUCTION", 10,
               "-AREA_TYPE", 1, 
               "-SLOPE_TYPE", 1,
               "-SLOPE_MIN", 0,
               "-SLOPE_OFF", 0.1,
               "-SLOPE_WEIGHT", 1
)
system(sysCMD)



#### >> 29 -- Wind Exposition Index ------------------------------ ## works but VERY slow
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_27.html

WindExp = "wind_exp_index.sgrd"
WindExp = file.path(tmpOut, WindExp)
sysCMD = paste(saga_cmd, "ta_morphometry 27", 
               "-DEM", sDTM,                     # input DEM
               "-EXPOSITION", WindExp,                  # output Wind Exposition Index
               "-MAXDIST", 300,
               "-STEP", 15,
               "-ACCEL", 1.5
)
system(sysCMD)


#### >> 30 -- Terrain Surface Texture -----------------------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_20.html

Texture = "texture.sgrd"
Texture = file.path(tmpOut, Texture)
sysCMD = paste(saga_cmd, "ta_morphometry 20", 
               "-DEM", sDTM,                     # input DEM
               "-TEXTURE", Texture,                      # output Terrain Surface Texture
               "-EPSILON", 1,
               "-SCALE", 10,
               "-METHOD", 1,
               "-DW_WEIGHTING", 0,
               "-DW_IDW_POWER", 2,
               "-DW_BANDWIDTH", 1
)
system(sysCMD)


#### >> 31 -- Morphometric Protection Index ----------------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_7.html
Protection = "protection.sgrd"
Protection = file.path(tmpOut, Protection)
sysCMD = paste(saga_cmd, "ta_morphometry 7",
               "-DEM",  sDTM,                        # input DEM
               "-PROTECTION", Protection,                  # output Morphometric Protection Index
               "-RADIUS", 2000
)
system(sysCMD)

#### >> 32 -- Vector Ruggedness Measure ---------------------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_17.html

VRM = "vrm.sgrd"
VRM = file.path(tmpOut, VRM)
sysCMD = paste(saga_cmd, "ta_morphometry 17", 
               "-DEM", sDTM,                   # input DEM
               "-VRM", VRM,                              # output Vector Ruggedness Measure
               "-MODE", 1,
               "-DW_WEIGHTING", 0, 
               "-DW_IDW_POWER", 2,
               "-DW_BANDWIDTH", 1
)
system(sysCMD)


#### >> 33 -- Mass Balance Index ----------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_10.html
MBI = "mbi.sgrd"
MBI = file.path(tmpOut, MBI)
sysCMD = paste(saga_cmd, "ta_morphometry 10",
               "-DEM", sDTM,               # input DEM
               "-HREL", VertDistance,               # input Vertical Distance to Channel Network
               "-MBI", MBI,                         # output Mass Balance Index
               "-TSLOPE", 15,
               "-TCURVE", 0.01,
               "-THREL", 15
)
system(sysCMD)


#### >> 34 -- Multi-Scale Topographic Position Index --------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_28.html
TPI = "mscale_tpi.sgrd"
TPI = file.path(tmpOut, TPI)
sysCMD = paste(saga_cmd, "ta_morphometry 28", 
               "-DEM", sDTM,                # input DEM
               "-TPI", TPI#,                        # output TPI
               #"SCALE_MIN", 1,
               #"SCALE_MAX", 8,
               #"SCALE_NUM", 3
)
system(sysCMD)


#### >> 35 -- Relative Heights and Slope Positions ----------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_14.html
SlopeHeight = "slope_height.sgrd"
SlopeHeight = file.path(tmpOut, SlopeHeight)
ValleyDepth = "val_depth_2.sgrd" #don't need this as created above?
ValleyDepth = file.path(tmpOut, ValleyDepth)
NormHeight = "norm_height.sgrd"
NormHeight = file.path(tmpOut, NormHeight)
StandHeight = "stand_height.sgrd"
StandHeight = file.path(tmpOut, StandHeight)
MSPosition = "ms_position.sgrd"
MSPosition = file.path(tmpOut, MSPosition)
sysCMD = paste(saga_cmd, "ta_morphometry 14",  
               "-DEM", sDTM,                 # input DEM
               "-HO", SlopeHeight,                  # output Slope Height
               "-HU", ValleyDepth,                  # output Valley Depth
               "-NH", NormHeight,                   # output Normalized Height
               "-SH", StandHeight,                  # output Standardized Height
               "-MS", MSPosition,                   # output Mid-Slope Position
               "-W", 0.5,
               "-T", 10,
               "-E", 2
)
system(sysCMD)


#### >> 36 -- Valley and Ridge Detection (Top Hat Approach) -------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_24.html

# not very informative (binary outputs)

# HillHeight = "hill_height.sgrd"
# HillHeight = file.path(tmpOut, HillHeight)
# ValleyIndex = "valley_index.sgrd"
# ValleyIndex = file.path(tmpOut, ValleyIndex)
# HillIndex = "hill_index.sgrd"
# HillIndex = file.path(tmpOut, HillIndex)
# HillslopeIndex = "hillslope_index.sgrd"
# HillslopeIndex = file.path(tmpOut, HillslopeIndex)
# sysCMD = paste(saga_cmd, "ta_morphometry 24",
#                "-DEM", sinksFilled,                 # input DEM
#                "-HILL", HillHeight,                 # output Hill Height
#                "-VALLEY_IDX", ValleyIndex,          # output Valley Index
#                "-HILL_IDX", HillIndex,              # output Hill Index
#                "-SLOPE_IDX", HillslopeIndex,        # output Hillslope Index
#                "-RADIUS_VALLEY", 1000,
#                "-RADIUS_HILL", 1000,
#                "-THRESHOLD", 100,
#                "-METHOD", 0
# )
# system(sysCMD)

#### >> 37 -- Upslope and Downslope Curvature ---------------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_26.html
LocalCurve = "local_curv.sgrd"
LocalCurve = file.path(tmpOut, LocalCurve)
UpslopeCurve = "upslope_curv.sgrd"
UpslopeCurve = file.path(tmpOut, UpslopeCurve)
LocalUpCurve = "local_upslope_curv.sgrd"
LocalUpCurve = file.path(tmpOut, LocalUpCurve)
DownCurve = "downslope_curv.sgrd"
DownCurve = file.path(tmpOut, DownCurve)
LocalDownCurve = "local_downslope_curv.sgrd"
LocalDownCurve = file.path(tmpOut, LocalDownCurve)
sysCMD = paste(saga_cmd, "ta_morphometry 26",
               "-DEM",  sDTM,                       # input DEM
               "-C_LOCAL", LocalCurve,                      # output Local Curvature
               "-C_UP", UpslopeCurve,                       # output Upslope Curvature
               "-C_UP_LOCAL", LocalUpCurve,                 # output Local Upslope Curvature
               "-C_DOWN", DownCurve,                        # output Downslope Curvature
               "-C_DOWN_LOCAL", LocalDownCurve,             # output Local Downslope Curvature
               "-WEIGHTING", 0.5
)
system(sysCMD)

#### >> 38 -- Steepest Slope (Slope Aspect and Curvature) --------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_0.html
SteepestSlope <- "steepest_slope.sgrd"
SteepestSlope = file.path(tmpOut, SteepestSlope)
sysCMD <- paste(saga_cmd, "ta_morphometry 0", 
                "-ELEVATION", sDTM,                              # input DEM
                "-SLOPE", SteepestSlope,                                # output Steepest Slope
                "-METHOD", 1,                                           # method 1 - steepest slope
                "-UNIT_SLOPE", 0, 
                "-UNIT_ASPECT", 0      
)
system(sysCMD)



# #### >> 39 -- Upslope Area -------------------------------------
# 
# # http://www.saga-gis.org/saga_tool_doc/7.6.2/ta_hydrology_4.html
# UpslopeArea = "upslopearea.sgrd"
# UpslopeArea = file.path(tmpOut, UpslopeArea)
# sysCMD = paste(saga_cmd, "ta_hydrology 4",
#                "TARGET", sDTM,
#                "-ELEVATION", sDTM,           # input DEM
#                "-AREA", UpslopeArea,                # output Upslope Area
#                "-METHOD", 3,
#                "-CONVERGE", 1.1
# )
# system(sysCMD)
# 

        }

} # end of covar function creation






#create_covariates(saga_dir = "E:/transfer/software/saga-7.4.0_x64",
#        saga_files = "E:/temp/PEM_DATA/BEC_DevExchange_Work/Baboon_AOI/1_map_inputs/covariates/25m",
#        cov_type = "stand" )# alternative is landscape for sample planning
        






######################################################################

## writeout option

saga_files = "E:/temp/PEM_DATA/BEC_DevExchange_Work/Buck_AOI/1_map_inputs/covariates/5m"

rasterfiles <- list.files(file.path(saga_files,"sagaTmp"),pattern = ".sdat$", full.names = T)

# Boundary issues2018
# rasterfiles <- rasterfiles[c(4,7,8)] # 15
# rasterfiles <- rasterfiles[26] #20 -24
# 
# #to run #15
# rasterfiles <- rasterfiles[c(25:31,35:36,38:41,50,52,53, 54,55, 57, 59, 60) ]
# 
# rasterfiles <- rasterfiles[35:36]
# rasterfiles <- rasterfiles[38:41]
# rasterfiles <- rasterfiles[c(50,52,53, 54,55, 57)]
# rasterfiles <- rasterfiles[59:60]
# 
# 
# # to rerun: 
# #15
# #20
# #21
# #22
# 



# write out to tif
for(r in rasterfiles){ sdat_to_tif(r)} 
        
# check the stack works: 

tiffiles <- list.files(file.path(saga_files), full.names = T,pattern = ".tif$")
tiffiles <- tiffiles[c(1,3:51,53:65)]
stack(tiffiles)  


#rasterfiles
test_stack <- stack(rasterfiles[c(1:5, 6:10)])#, 8:23, 25:36, 41:47, 49)]) #,50:59,61:70)])
test_stack <- stack(rasterfiles[c(1:5,8,10)])#, 8:23, 25:36, 41:47, 49)]) #,50:59,61:70)])



#######################################################################

# downsample to the smallest res for modelling 



downscale_folder <- "E:/temp/PEM_DATA/BEC_DevExchange_Work/Baboon_AOI/1_map_inputs/covariates/10m"
template_folder <- "E:/temp/PEM_DATA/BEC_DevExchange_Work/Baboon_AOI/1_map_inputs/covariates/5m"


cov_fineres_match(downscale_folder, template_folder)
        
        







######################################################################################

# Gnerating layers for stage 1 sampling 

#####################################################################################

# For the stage 1 sampling we need to create specifc landscape scale metrics including 
# - Dem
# - DAH with three groups 
# - MRVBF with groups
# - Landform (TPI metrics)

# 1) DEM - already generated 


# 2) DAH - already generated but requires reclassing 

dah <- list.files(file.path(saga_files), pattern = "dah.tif", full.names = TRUE )
dah <- raster(dah)

# set threshold value
threshold <- 0.3

# Build a reclass matrix for three group using (+/- threshold)
# all values > 0 and <= 0.25 become 1, etc.
# Wetzinkwa on the 0.3 threshold

m <- c( -10, (threshold*-1), 1,  
        (threshold*-1 ), threshold, 2,  
        threshold, 10,  3)

rclmat <- matrix(m, ncol=3, byrow =TRUE)
rc <- reclassify(dah , rclmat)
#crs(rc) <- PROJ

writeRaster(rc, file.path(saga_files, "dah_3class0.3.tif"), driver = "GTiff", overwrite = TRUE)


#lsrast<- list.files(saga_files, full.names = TRUE, pattern = ".tif")
#st <- stack(lsrast)

# filter based on the 0.3 for Date Cree
# - review the slope (style into three classes
#                     - <25% slope or 0.43 radians,
#                     - 45% slope or 0.43 - 0.78 radians 
#                     - >45% slope
#                     - once this is stlyed then you can adjust the grouping on the DAH to match
#                     - Deception = -0.2 to 0.2. 
#                     - Date Creek = -0.3 to 0.3. 



# 3) MRVBF 

# A base MRVBF (for landscape + standard) is created above but needs to be sieved.

mrv <- list.files(saga_files, pattern = "mrvbf_LS.tif", full.names = TRUE )
mrv  <- raster(mrv)



# # set up a sieve python command 
# saga_cmd = "saga_cmd"}  
# 
# gdal_sieve [-q] [-st threshold] [-4] [-8] [-o name=value]
# srcfile [-nomask] [-mask filename] [-of format] [dstfile]
# 
# python3 -m 
# gdal_sieve -st 10 -4 -of GTiff "crs=EPSG:3857&format&type=xyz&url=http://ecn.t3.tiles.virtualearth.net/tiles/a%7Bq%7D.jpeg?g%3D1&zmax=18&zmin=0" C:/Users/genperk/AppData/Local/Temp/processing_SOaSPr/77b725a00d4f44ffa9d85dbc4840e7a5/OUTPUT.tif
# 
# python3 -m gdal_sieve -st 250 -8 -of GTiff D:/PEM_DATA/BEC_DevExchange_Work/WilliamsLake_AOI/1_map_inputs/covariates/25m/bgc.tif D:/PEM_DATA/BEC_DevExchange_Work/WilliamsLake_AOI/0_raw_inputs/base_layers/raw/sieve.tif
# 

infile <- "WilliamsLake_AOI/1_map_inputs/covariates/25m/bgc.tif"
outfile <- "WilliamsLake_AOI/1_map_inputs/covariates/25m/bgc_s.tif"

sysCMD = paste("gdal_sieve.py", 
               "-st", 250,           # sieving
               -8,                  # connectedness
               "-of","GTiff", 
               infile, outfile )

system(sysCMD)
sysCMD = paste("gdal_sieve", 
               "-st", 250,           # sieving
               -8,                  # connectedness
               "-of","GTiff", 
               infile, outfile )

system(sysCMD)





# IN QGIS  sieve via QGIS gdal or can use below but suboptimal option

# sieved threshold Date Creek - 250
# tick connectness = 8 
# filename = mvvbf_LS_s.tif


# Tested this but not very good. 
# perhaps could add to gdal utiles? or code this into this script (IMPROVEMENT TO BE COMPLETED)
# library(bfastSpatial)                 
# mrv1 <- areaSieve(mrv, thresh = 10, directions = 8, verbose = TRUE,
#                   keepzeros = FALSE, cores = 5)
#writeRaster(mrv1 , file.path(saga_files, "mrvbf_ls_s3.tif"))


# 4) Landform classification 
# I was having lots of errors with the edge effect in SAGA calculating tpi metrics so calculated 
# in R instead. 

# we can calculate 6 class landscape format, using a script based on the tpi calculations within
# spatialEco package and then wrapped into function found in the old version of the package 
# GmAMisc (https://github.com/gianmarcoalberti/GmAMisc/blob/master/R/landfClass.R). 
# As this is an old version I copied the function into our function folder for convenience of use. 

library(spatialEco)

# # 1) Calculation of tpi testing 
# tpi1 <- tpi(DTM , scale = 3, win = "rectangle", normalize = FALSE, zero.correct = FALSE)
# writeRaster(tpi1 , file.path(saga_files, "tpir.tif"))
# 
# tpi2 <- tpi(DTM , scale = 15, win = "rectangle", normalize = FALSE, zero.correct = FALSE)
# writeRaster(tpi2 , file.path(saga_files, "tpir2.tif"))
# 
# tpi3 <- tpi(DTM , scale = 25, win = "rectangle", normalize = FALSE, zero.correct = FALSE)
# writeRaster(tpi3 , file.path(saga_files, "tpir3.tif"))


# Calculation of landscape classification (using script from GmAMisc)

# I tested versious options. Increasing the scale means a smoother general format, although the increase
# in size reduces the aoi due to edge effect of surrounding pixal calculations. 
# NOte scale needs to be an odd number
# sn and ln are only valid when using the "ten" classes. 
# tpi parameters are only if you want a tpi exported as part of the process. 
# The best option was scale = 75, which was a compromise of generalisation and edge loss

# read in dtm 
# note make sure your DEM does not contain data outside the AOI as this impacts the calculations
#DTM <- raster(file.path(saga_files,"dem.tif"))
DTM <- raster(file.path(trim_files,"dem.tif"))


# lfc1 <- landfClass(DTM, scale = 3, sn = 3, ln = 7, n.classes = "six")
# lfc2 <- landfClass(DTM, scale = 25, sn = 3, ln = 7, n.classes = "ten", stand.tpi = TRUE)
# lfc3 <- landfClass(DTM, scale = 25, sn = 3, ln = 11, n.classes = "ten", stand.tpi = TRUE)
# lfc4 <- landfClass(DTM, scale = 25, sn = 3, ln = 11, n.classes = "ten", stand.tpi = FALSE)
# 
# lfc5 <- landfClass(DTM, scale = 51, sn = 3, ln = 7, n.classes = "six")
lfc6 <- landfClass(DTM, scale = 99, sn = 3, ln = 7, n.classes = "six")
writeRaster( lfc6, file.path(saga_files, "landform6.tif"), overwrite = TRUE)

hist(lfc6)

unique()

lfclass <- landfClass(DTM, scale = 75, sn = 3, ln = 7, n.classes = "six")


# convert to classes based on the grouping 

writeRaster(lfclass, file.path(saga_files, "landform.tif"), overwrite = TRUE)

# these last paramerts look good. 

# IN QGIS  sieve via QGIS gdal or can use below but suboptimal option

# sieved threshold Date Creek - 250
# tick connectness = 8 
# filename = landform_s250.tif

# we can reclass to remove the extreme values outside the AOI  

lfclass <- list.files(file.path(saga_files), pattern = "landform_stt.tif", full.names = TRUE )
lfclass <- raster(lfclass)

lfclass <- crop(lfclass, dah)
# set threshold value
threshold <- -5

# Build a reclass matrix for three group using (+/- threshold)
# all values > 0 and <= 0.25 become 1, etc.

values(lfclass)[values(lfclass) < -5] = NA

writeRaster(lfclass, file.path(saga_files, "lfclass_st.tif"), driver = "GTiff", overwrite = TRUE)



##################################################################################

# Check the format of raster values

## writeout option
cov.dir = "E:/temp/PEM_DATA/BEC_DevExchange_Work/BoundaryTSA_AOI/1_map_inputs/covariates/5m"
cov.dir = "E:/temp/PEM_DATA/BEC_DevExchange_Work/Deception_AOI/1_map_inputs/covariates/5m"

rasterfiles <- list.files(cov.dir, full.names = TRUE)

for (iii in 1:length(rasterfiles)){
        
        iii = 1
        
        orig_raster <- rasterfiles[iii] # 5
        print (orig_raster)
        orig_raster <- raster(orig_raster)
        
        outfile <- gsub("sdat", "tif", sagafile)
        r <- readGDAL(file.path(saga_files,"sagaTmp",sagafile))
        w <- file.path(saga_files, "outputs", outfile) #, sep = "")
        writeGDAL(r, w)
        
        rfile <- raster(w)
        
        if(is.na(crs(rfile))) {
                print("setting projection to 3005")
                # ensure raster is in BC albers projection
                PROJ <- crs(paste("+init=epsg:",3005, sep = "")) 
                crs(rfile) <- PROJ 
                
        } else {
                print("projection already set to 3005")
        }
        
        w_out <- writeRaster(rfile, file.path(saga_files, basename(w)), overwrite = TRUE, driver = "GTiff")
        
}




