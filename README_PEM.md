# Scripts for Machine Learning PEM Research Project

##  PEM Methods Research 

A detailed [workflow diagram](https://drive.google.com/file/d/1-AqUyNgmF5T_esnjIRmSs5VIp8FC0rZY/view?usp=sharing) describes the linkages between components and tasks for this project. Components of work are described in more detail [here](https://docs.google.com/document/d/1Go9m6KJyksoyHEKs4BpfrwVoklgPuglUmiYzKK5vDWc/edit#heading=h.768snbfw1sld)
with a contracted subset of work described [here](https://docs.google.com/document/d/1jgKAXGjMT29Z2OjJwaPoiqkZ7pZb5SV4_yrvYYCBec8/edit#heading=h.768snbfw1sld). 

Following the workshop in february 2020, a [scripting plan](https://docs.google.com/document/d/10Fcs5TA-gH-cfTFRrWhT3A7xE7dVU_IcZqFhm4PtfHk/edit) was developed to track development of functions that will feed into the workflow.


## Folder Structure

For each project area:


--ProjectName_AOI
            _AOI
            |
            _MapUnitLegend
            |
            0_raw_inputs
            |      base_layers
            |      DEM
                       lidar
            |      satellite_layers
            |
            1_map_inputs
            |      covariates
            |            1m
            |            2.5m
            |            5m
            |            10m
            |            25m
            |
            |      trainingData
            |
            2_sample_design
            |      stage1_StudyDesign
                                    input_raster
                                    input_raw
                                    output
                                    training_pts
            |      stage1_AirCall_Design
                                   input_raw
                                   output 
            |      stage2_StudyDesign
            |
            3_maps_analysis
                  analyses
                  predicted_maps
                  
                  
                  
                  
                  
 Functions are currently being compiled to create a package (https://github.com/ColinChisholm/PEMGeneratr). 

