# RESOURCE SELECTION FUNCTION FOR DAVIS STRAIT POLAR BEARS

Resource selection functions (RSFs) are used extensively in ecology to study species' spatial ecology. RSFs involve identifying suitable habitat by defining space as either "used", based on known locations of a species (e.g. using telemetry data), or "available", based on what was available to the species at that time, or what surrounds their known locations. Studies have used RSFs for understanding the exact sea ice characteristics required by polar bears for habitat in various areas across the Arctic, but have focused on either the global population of polar bears as a whole, or on just a few subpopulations. None, however, have studied the Davis Strait subpopulation (DS). Understanding the suitable habitat of each subpopulation of polar bears is critical as each uses slightly different habitat covariates, and climate change has the potential to impact each of their habitats in different ways.

The spatial ecology of DS polar bears is largely unknown, as previous studies have focused almost entirely on population dynamics. This subpopulation faces decreasing body conditions likely due to decreasing sea ice conditions that has led to less hunting opportunities. Understanding its current suitable habitat is critical in determining how it may be affected by climate change in the future. 

## Study objective

The overall goal of this larger study is to understand how climate change may potentially impact the suitable habitat of DS polar bears. For now, however, this repository will focus on developing an RSF to understand the current selected habitat covariates and associated coefficients for this subpopulation across various temporal scales. We are interested in how the habitat selection differs amongst three key ice seasons in Davis Strait (freeze-up, winter, and break-up) and across a 10 year span (1991-2001). This study also involves analyzing the changing sea ice conditions in Davis Strait across the same temporal scales (see repository ipm_sea-ice). Later, we will use this information to predict how the ideal DS polar bear habitat will change or shift in the future in various climate change scenarios. 

The steps for developing the resource selection function include:
  1. Address autocorrelation (we may find that the north and south substructures are correlated to each other) 
  2. Calculate average step length (distance between 2 consecutive points) for all bears
  3. Create circular buffer around each point with average step length as the radius
  4. Generate random points within each circular buffer (these will constitute the "available" habitat)
  5. Extract habitat covariates for each "used" and "available" point (covariates include: ocean depth, sea ice concentration, distance to specific sea ice concentrations, distance to open water, distance to land)
  6. Compare "used" versus "available" covariate coefficients and differentiate selection between each sea ice season (dates of which will be determined through study outlined in respository ipm_sea-ice)
  7. Compare "used" versus "available" covariate coefficients over time (i.e. between early and later parts of study period)

## Google drive
Any additional information or figures developed in the script can be found in this Google Drive: https://drive.google.com/open?id=1arj-a6-Z6-kAv8iH69P49pYzZx-6-vTj

