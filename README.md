# BEC_DevExchange_Work
## PEM Research Project

PEM Methods Research   https://github.com/bcgov-c/BEC_DevExchange_Work/blob/master/README_PEM.md

Rmarkdown reports, including scripts, provided by [Colin Chisholm](http://chisholm.forestecosystems.ca). Mostly related to generation of the [Aleza Lake Research Forest](http://alrf.unbc.ca) and Eagle Hills PEMs.   http://www.forestecosystems.ca/PEM-Reports/



## 1) Summary of scripts: 

**Data Preparation**

-[01_BaseLayer_Consolidation] (https://github.com/bcgov-c/BEC_DevExchange_Work/blob/master/01_BaseLayer_Consolidate.Rmd) 
-01_LiDAR Processing]
-02a_DEM_SpatialLayer_Prep...] large and XML files for small AOIs
-02b_Satellite - preparation of satellite imagagery
-03a1_Energy_stats - some side work by Kiri
-03a1_Number_pts - some side work by Kiri

**Sample Design**

-03a_Stage1_HeliSamples
-03a_stage1_sampleDesign - main sampling layout script 
-03b_stage1_nonforestDesign 

**Data collation and formatting**

-04a_AA_trainingPt_TransectImport&Clean
-04a_TrainingPt_TransectImport (per AOI)
-04a_TrainingPt_summary - summarise data inputs 
-04a_TrainingPt_repeatsample - script to analyse repeat sample data


**Modelling**

-05a_PEM_iAA_Model_Assessment - main model script
-05a_PEM_iAA_Ensemble_ModelAssessment - stop gap script for ensemble (needs clean ip)

**Map generation and map accuracy assessment**

-05a_PEM_predict_map


**Stage 2:StudyDesign**

-06_Stage2_sample_entropy
-06_stage2_sample_entropyBoundary - stop gap scripts
-06_stage2_sampling_entropy_V2.RmD


-05d_PEM_model_comparison - model outputs (outof date)
-05a_PEM_external_AA_assessment - for external AA data testing 


**Final Map product scripts**
-07a_RasterDespeckle
-07cPEMColourThemes
-07d_descpeckle and polygonize 


## 2) Summary of files

Common files for all project work include: 

-_documentation - all notes 
-_functions - code to source with discrete functions
-_references 
-Additional_functions - additonal one off scripts for random tasks (needs to be cleaned up)
_ **_AOI - a folder for each study area

Each area of interest is layed out in the same manner to enable scripts to run. ie: Deception 

Deception:

- _MapUnitLegend - contains area specific files (map key.csv and models.xls for tracking model runs)
- _0_raw_inputs : All raw spatial data - (base_layers, DEM, satellite_layers)
- _1_map_inputs : Prepared data ready for models
  - covariates; a folder for each spatial resolution (2.5m, 5m , 10, etc)
  - trainingData: raw, processed and attributed training data files 
    contains a folder with training point data attributed at multiple resolution (att_2.5m, att_5m). 
    
- _2_sample_design: All layouts, data and processed data for different training point inputs. 

- _3_maps_analysis: 




