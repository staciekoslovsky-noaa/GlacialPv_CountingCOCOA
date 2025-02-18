# Glacial Harbor Seals: Counting COCOA Sites

This repository contains the code for preparing glacial COCOA sites for counting. The files and code in this repository are as follows:
* GlacialPv_COCOA_00_DefaultAnnotationFile_YYYY_[CameraView].csv - default review and suppression zones for each camera view and year; the review and suppression zones are always the same for the C camera view and always variable by year for the L/R camera views
* GlacialPv_COCOA_01_PrepareData4Viame.R - code for generating image lists and annotation files for COCOA survey; also creates a shapefile for evaluating the frequency at which images should be kept/used for counting
* GlacialPv_COCOA_02_ImportAnnotations.R - code for importing/replacing annotations into the DB after counts are complete

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.