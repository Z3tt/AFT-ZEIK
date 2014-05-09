
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBALS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

extensions 
[
  ;profiler
]

globals
[ 
  Dimension                ;; [#]             total number of cells
  Homerange                ;; [ha]            home range area = 9 x 9 cells
  SLHomerange              ;; [m]             side length of home range = (Homerange*10000)^0.5
  SLCell                   ;; [m]             side length of cell = SLHomerange/9 [m]
  SLGrid                   ;; [km]            side length of landscape grid = (SLHomerange*50)/1000
  #trees                   ;; [#]             maximal number of of trees or large shrubs per cell = cell area/121 = (SLCell*SLCell)/121
  Scale                    ;; [cells]         number of basic vegetation cells per side which were aggregated to body mass depending cells (related to side length of home ranges)
  InitBreeders             ;; [#]             counts birds which did find an unoccupied home range and became breeders
  InitFloaters             ;; [#]             counts birds which could not find an unoccupied home range but became floaters
  InitOverflow             ;; [#]             counts birds which could not become a breeder or floater in the setup procedure
  ;RainMean                ;; [mm]            ;; moved to interface; mean amount of rainfa ll ; moved to interface
  ;RainStd                 ;; [mm]            ;; moved to interface; standard deviation of precipitation ; moved to interface
  cRain                    ;; [mm]            current amount of rainfal, drawn from a normal distribution (mean 300 mm, std 150 mm)
  outputRain               ;; [mm]            amount of current rainfall for output file
  Emigrants                ;; [#]             number of emigrants per year (floaters who did not find a home range)
  ;Population-Start        ;; [#]             ;; moved to interface; population size at the beginning (t = 0)  
  ;Search-Radius           ;; [#]             ;; moved to interface; number of cells the floater searches (incremented in home ranges = 9 cells); moved to interface
  ;NoFlights               ;; [#]             ;; moved to interface; number of explorer flights during dispersal
  ;Maximum-Floater         ;; [#]             ;; moved to interface; maximum threshold for number of floaters within a occupied home range ; moved to interface
  ;Floater-Radius          ;; [cells]         ;; moved to interface; home range diameter of floaters
  ;Threshold-Trees         ;; [#]             ;; moved to interface; minimum threshold for suitable nest sites for tree-breeding birds ; moved to interface
  ;Threshold-LargeShrubs   ;; [#]             ;; moved to interface; minimum threshold for suitable nest sites for large-shrub-breeding birds ; moved to interface
  ;Threshold-Shrubs        ;; [%]             ;; moved to interface; maximum threshold for suitable nest sites for ground-breeding birds ; moved to interface

]

turtles-own
[ 
  Age                      ;; [a]
  Status                   ;; [string]        breeder, floater, juvenile, emigrant
  ;Mass                    ;; [g]             ;; moved to interface
  ;Habitatpreference       ;; [string]        ;; moved to interface
  ;Nestsite                ;; [string]        ;; moved to interface
  MyNest                   ;; [patch]         own nest site (center of its home range)
  Longevity                ;; [a]             maximum age
  Mortality                ;; [%]             basic mortality rate
  cMortality               ;; [%]             current mortality rate
  Maturity                 ;; [a]             minimum age for reproduction
  Clutch                   ;; [#]             basic reproductive output
  cClutch                  ;; [#]             current reproductive output
  MedianDD                 ;; [cells]         median dispersal distance (depending on body mass) 
  StepSize                 ;; [number, 1-5]   factor determing random disperals distance (StepSize * MedianDD = dispersal distance)
  ;InfluenceHQ             ;; [0-1]           ;; moved to interface; influence-factor of HQ on reproduction and mortality in relevance to Mortality and Clutch (resulting in cMortality and cClutch)
  helpMyNest
  helpMyNestHQ  
  helpFloaterHR
  helpFloaterHRHQ
]

patches-own
[
  VegType
  pGrass                   ;; [%]
  pShrubs                  ;; [%]             including small and large shrubs
  pTree                    ;; [#]
  pLargeShrub              ;; [#]
  pAllShrubs               ;; [%]             proportion of small and large shrubs (pShrubs + pLargeShrubs 
  occupied?                ;; [boolean]       TRUE for occupied vegetation cells
  suitable?                ;; [boolean]       TRUE for suitable cells of a possible home range
  HQ                       ;; [number, 0-1]   habitat quality of the vegetation cell
  cHQ                      ;; [number, 0-1]   current habitat quality of the vegetation cell, including biomass changes due to cRainfall
  overallHQ                ;; [number, 0-1]   mean HQ of home ranges with a suitable nest site
  Nest?                    ;; [boolean]       TRUE for cells suited as nest sites in general (for birds)
  cNest?                   ;; [boolean]       TRUE for cells currently suited as nest sites after rainfall (for birds)
  NestPatch                ;; [boolean]       TRUE for cells suited as nest sites (for landscape view)
  NoFloater
  ;Threshold-overallHQ     ;; [number, 0-1]   threshold for determing a suitable home range by using overallHQ
]




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCALES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; SPATIAL
;;
;; Cell size differs with body mass
;; Home ranges consist of 9 x9 cells
;; World covers 50 x 50 potential home ranges = 450 x 450 cells
;; 
;; TEMPORAL
;;
;; 1 tick equals 1 year




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETUP + GO PROCEDURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  
     INIT_import-landscape   
     INIT_calculate-spatial-scales  ;; calculates all parameters related to spatial scales that are needed to setup the model
     INIT_calculate-HQ              ;; calculates habitat quality for each cell, which depends on the functional type
     INIT_calculate-HQ-HR           ;; calculates habitat quality of home ranges; suitability depends on funtional type

     reset-ticks

  ;; turtles
     crt Population-Start
     ask turtles  
     [
         set size 3
         set color red
     
         INIT_calculate-parameters  ;; calculates all population parameters that are needed to setup the model, i.e. longevity, mortality rate, age of maturity, and clutch size
      
         set Age random (longevity - 1 - maturity) + maturity  ;; minimum age "maturity" as integer, maximum age "longevity -1" as integer"
      
  ;; testing age distribution
         ;show Age 
     ]
  
     INIT_search-HR                                                            ;; birds fly around and search for suitable home ranges (depending on funftional traits)
     if View = "Vegetation" or View = "OFF" [ update-view ]                    ;; update of world interface (see "to update-view")...
     if View = "Home ranges" or View = "Habitat quality" [ INIT_update-view ]  ;; ... using HQ instead of cHQ and Nest? instead of cNest? because in INIT cHQ and cNest? are not available

  ;; warning if there are not enough home ranges for the initial population / only information

     output-print (word "Start population:")
     output-print (word " - " InitBreeders " breeder(s), " InitFloaters " floater(s)")  
     output-print (word " - " InitOverflow " bird(s) could not find")  
     output-print (word "   a suitable, unocupied home range.") 
    
end


;; GO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  
  ;; Profiler:
     ;profiler:start
     
     set Emigrants 0   ;; emigrants per year  
  
     rainfall          ;; apllies a random normal distribution of rainfall for each year
     calculate-HQ      ;; calculates current habitat quality because rainfall may change habitat quality of cells
     calculate-HQ-HR   ;; calculates current suitable home ranges because rainfall may change habitat quality of cells and thus home ranges
     ageing            ;; all birds age one year     
     search-HR         ;; floaters (including juveniles which reached age of maturity this year) have the chance to search for a suitable, unoccupied home range
     reproduce         ;; breeders with an overall habitat quality higher than "Threshold-overallHQ" reproduce     
     survive           ;; birds which have reached their maximum age die and specific mortality rates are apllied to adults and juveniles.
     ;update-view       ;; update of world interface     
     tick              ;; t = t + 1
  
;     if ticks mod 10 = 0 
;     [
;         set View "Home ranges"
;         update-view
;         export-all-plots (word "Plots_" behaviorspace-run-number "-" ticks "_" timer ".csv")
;         export-view (word "View_" behaviorspace-run-number "-" ticks "_" timer ".png")
;         set View "OFF"
;     ]
 
     ;if not any? turtles 
     ;[ 
         ;set View "Home ranges"
         ;update-view
         ;export-interface (word "Testrun_" FileName "_" Nestsite "_" ticks ".png")
         ;user-message (word "The population became extinct after " ticks " years.")
         ;stop 
     ;]
     
     ;if ticks = 100 
     ;[ 
         ;set View "Home ranges"       
         ;update-view
         ;export-interface (word "Testrun_" FileName "_" Nestsite "_" ticks ".png")
         ;stop 
     ;]
  
  ;; Profiler:
     ;profiler:stop
     ;print profiler:report
     ;profiler:reset
  
end




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INIT PROCEDURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; INIT: IMPORT LANDSCAPE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to INIT_import-landscape
  
  ;; NetLogo clears all (including vegetation) if there is an import file 
     if (Import = "Path") or (Import = "FileName")
     [
         ca                   
                                           
  ;; file import
         if Import = "Path" [ file-open user-file ]
         if Import = "FileName" [ file-open (word Filename ".csv") ]
                  
         while [not file-at-end?]
         [
             let next-X file-read
             let next-Y file-read
             let next-pTree file-read
             let next-pLargeShrub file-read
             let next-pShrubs file-read
             let next-pGrass file-read
             ask patch next-X next-Y
             [
                 set pTree next-pTree
                 set pLargeShrub next-pLargeShrub
                 set pShrubs (next-pShrubs * 100)
                 set pGrass (next-pGrass * 100)
            ]
         ]
         file-close
         ask patches
         [
             set occupied? FALSE    ;; all cells are unoccupied at the beginning                                                
             set suitable? FALSE    ;; no cell is suitable at the beginning
         ]
     ]
     
  ;; vegetation remains if there is no input file   
     if Import = "OFF"
     [ 
         ct  ;; clear turtles
         clear-drawing 
         clear-output
         clear-all-plots
         ask patches 
         [ 
             set pcolor 0
             set occupied? FALSE    ;; all cells are unoccupied at the beginning                                                
             set suitable? FALSE    ;; no cell is suitable at the beginning
             set cHQ 0
             set Nest? FALSE
             set cNest? FALSE
             set NestPatch 0
         ] 
     ]
 

  
end


;; INIT: CALCULATE FT DEPENDENT PARAMETERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; turtle procedure
  ;; Calculates all population parameters that are needed to setup the model, i.e. longevity, mortality rate, age of maturity, and clutch size.

to INIT_calculate-parameters
  
     set Longevity 4.75 * (Mass ^ 0.17)
     set Mortality 1 - (0.01 ^ (1 / Longevity))
     set Maturity 0.11 * (Mass ^ 0.4796)
     set Clutch 4.62 * (Mass ^ -0.138)
     set MedianDD (13.1 * ((Mass / 1000) ^ 0.63) * 1000) / SLCell
  
end


;; INIT: CALCULATE SPATIAL SCALES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; observer procedure
  ;; Calculates all parameters related to spatial scales that are needed to setup the model.

to INIT_calculate-spatial-scales
  
  ;; calculation total number of cells
     set Dimension (world-width  * world-height) 
  
  ;; scale calculations for insectivorous and omnivorous birds, related to bush thickets
     ifelse Habitatpreference = "bush thicket"   
     [ set Homerange 0.129 * (Mass ^ 0.881) ]   ; home range area [ha]
  ;; scale calculations for carnivorous and herbicorous birds, related to open grasslands
     ;;if "open grassland"     
     [ set Homerange 0.0676 * (Mass ^ 1.344) ]  ; home range area [ha]
     
     set SLHomerange ((Homerange * 10000) ^ 0.5)     ; side length home range [m] = sqrt (home range area [ha] * 10000)
     set SLCell (SLHomerange / 7)                    ; side length cell [m]       = side length home range [m] / side length home range (as a square) [cells]
     set SLGrid ((SLCell * world-width) / 1000)      ; side length grid [km]      = side length cell [m] * side length grid [cells]) / 1000
  
  ;; testing size calculations / only information 
     output-print (word "Home range area: " precision Homerange 2 " ha")                                 
     output-print (word "Max. diameter:   " precision (9 * SLCell) 0 " m")  ; home range diameter (as a "circle") = 9
     output-print (word " ")
     output-print (word "Cell size: " precision SLCell 0 " m x " precision SLCell 0 " m")
     output-print (word " ")
     output-print (word "Grid size (landscape area):")
     output-print (word precision SLGrid 2 " km x " precision SLGrid 2 " km (" precision (SLGrid ^ 2) 0 " km^2)")
     output-print (word " ")  

end


;; INIT: CALCULATE HQ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; observer procedure
  ;; Calculates habitat quality for each cell, which depends on the functional type.

to INIT_calculate-HQ  
 
  ;; sets number of basic vegetation cells per side which were aggregated to body mass depending cells 
  ;;    SLCell is rounded to an even number so it is a multiple of basic vegetation cells (2m x 2m); divided by 2 it represents the number of cells per side which were aggregated
  ;;    Example (10 g, "bush thicket"): Homerange = 0.98 ha -> SLHomerange = 99.04 m -> SLCell = 14.15 m -> SLCell_rounded = 14 m -> Scale = 7 -> 7*7 = 49 basic cells were aggregated to 1 new cell
     if Habitatpreference = "open grassland"
         [    
              if Mass = 10 [ set Scale 9 ] 
              if Mass = 30 [ set Scale 18 ]
              if Mass = 100 [ set Scale 41 ] 
         ]
     if Habitatpreference = "bush thicket"
         [ 
              if Mass = 10 [ set Scale 7 ] 
              if Mass = 30 [ set Scale 11 ] 
              if Mass = 100 [ set Scale 20 ] 
         ]      
 
  ;; habitat quality for carnivorous and herbicorous birds, related to open grasslands
     ifelse Habitatpreference = "open grassland"
     [ 
         ask patches 
         [ 
             set HQ (-0.1 + 0.02 * pGrass)
             if HQ < 0 [ set HQ 0 ] 
             if HQ > 1 [ set HQ 1 ]
         ]
     ]
  ;; habitat quality for insectivorous and omnivorous birds, related to bush thickets
     [ ;;if "bush thicket"
         ask patches 
         [ 
             set pAllShrubs (pShrubs + (pLargeShrub * 100 / Scale))
             set HQ (-0.54 + 0.134 * pAllShrubs - 0.0029 * pAllShrubs * pAllShrubs)
             if HQ < 0 [ set HQ 0 ] 
             if HQ > 1 [ set HQ 1 ]
         ]
     ]  

end


;; INIT: CALCULATE HQ OF HOME RANGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; observer procedure
  ;; Calculates habitat quality of home ranges (overallHQ is the meanHQ of all 49 home range cells); suitability depends on funtional type.
  ;; Saves potential nest site cells, based on functional trait dependent thresholds

to INIT_calculate-HQ-HR  
  
  ;; criteria for suitable home ranges for tree-breeding birds (depending on number of trees)
     ifelse Nestsite = "on trees"
     [
         ask patches with [pTree >= Threshold-Trees]
         [ 
             set overallHQ ((sum [HQ] of patches in-radius 4) / 49) 
             set Nest? TRUE 
         ]
     ]
     [   
  ;; criteria for suitable home ranges for shrub-breeding birds (depending on number of large shrubs) 
         ifelse Nestsite = "on large shrubs"
         [
             ask patches with [pLargeShrub >= Threshold-LargeShrubs]
             [ 
                 set overallHQ ((sum [HQ] of patches in-radius 4) / 49) 
                 set Nest? TRUE 
             ]
         ]
  ;; criteria for suitable home ranges for ground-breeding birds (depending on proportion of shrubs) 
         [ ;;if Nestsite = "ground-breeding"
             ask patches with [pShrubs <= Threshold-Shrubs]
             [ 
                 set overallHQ ((sum [HQ] of patches in-radius 4) / 49)
                 set Nest? TRUE 
             ]
         ]
     ]
  
end


;; INIT: SEARCH HOME RANGE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; observer procedure
  ;; Birds fly around and search for suitable home ranges (depending on funftional traits):
  ;;    - If there are suitable home ranges which are not occupied by other birds the bird occupies the home range with the highest overall habitat quality.
  ;;    - If there are suitable home ranges but all of them are occupied the bird can stay as a floater as long as the threshold of floaters is not reached.
  ;;    - If there are no unoccupied home ranges and the threshold of floaters for each home range is reached, the bird emmigrates/dies.

to INIT_search-HR

     set InitBreeders 0    
     set InitFloaters 0
     set InitOverflow 0
 
     ask turtles
     [  
  ;; birds search for home ranges
         let patches-HR patches with [(Nest? = TRUE) and (all? patches in-radius 4 [occupied? = FALSE]) and (overallHQ >= Threshold-overallHQ)]
         ifelse any? patches-HR  
         [
             move-to max-one-of patches-HR [overallHQ]                                ;; bird moves to an unoccupied, suitable nest site within an unoccupied home range with the highest overall HQ of all unoccupied home range,
             set MyNest patch-here                                                    ;; builds its nest in this patch,
             ask patches in-radius 4 [ set occupied? TRUE ]                           ;; occupies the whole home range, 
             set Status "Breeder"                                                     ;; becomes a breeder and 
             set InitBreeders InitBreeders + 1
             set cMortality (Mortality * ((2 * InfluenceHQ * (1 - [overallHQ] of patch-here)) +  (1 - InfluenceHQ)) + 0.1)  ;; mortality rate of juveniles from "last" winter is set to offspring via "hatch"
                 if cMortality > 1 [ set cMortality 1 ]
                 if cMortality < 0 [ set cMortality 0 ]
             set cClutch (2.44 * Clutch * 0.5 * (2 * InfluenceHQ * [overallHQ] of patch-here + (1 - InfluenceHQ)) )   ;; reproduces based on overallHQ and InfluenceHQ
             let Clutch-reminder cClutch - floor cClutch                              ;; residual value is saved in Clutch-remainder 
             ifelse random-float 1 < Clutch-reminder                                  ;; -> possibility of another offspring
             [ 
                 hatch (floor cClutch) + 1 
                 [ 
                     set Status "Juvenile"
                     set color yellow
                     set size 2
                     set Age 0   ;; without this line Age is distributed randomly!
                     set StepSize 0
                 ]
             ]
             [ 
                 hatch floor cClutch 
                 [ 
                     set Status "Juvenile"
                     set color yellow
                     set size 2
                     set Age 0
                     set StepSize 0                 
                 ]
             ]
         ]
         [ ;;if not but there is still room for a floater bird stays in the grid
             let patches-Floater patches in-radius Search-Radius with [(sum [NoFloater] of patches in-radius  Floater-Radius) < Maximum-Floater]
             ifelse any? patches-Floater
             [
                move-to max-one-of patches-Floater [ overallHQ ]
                set Status "Floater"                                                  ;; bird becomes a floater
                set color blue
                set InitFloaters InitFloaters + 1
            ]
            [ ;;else "all home ranges occupied"
                set InitOverflow InitOverflow + 1
                die                                                                   ;; bird leaves the grid           
            ]
        ]
  ] ;;close ask
  ask turtles with [Status = "Juvenile"] [if random-float 1 <= cMortality [ die ]]   ;; survival rates from winter are applied
  
end




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GO PROCEDURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; RAINFALL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; observer procedure
  ;; Applies a random normal distribution of rainfall for each year (mean and standard deviation: user input)

to rainfall
  
     let helpRain random-normal RainMean RainStd
     if helpRain < 0 [ set helpRain 0 ]
     set cRain helpRain
     set outputRain round cRain 
  
end


;; CALCULATE CURRENT HQ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; observer procedure  
  ;; Rainfall may change habitat quality of cells. Therefor it is calculated each year after the rain.

to calculate-HQ
  
  ;; habitat quality for carnivorous and herbicorous birds, related to open grasslands
     ifelse Habitatpreference = "open grassland"
         [ 
             ask patches 
             [ 
                set cHQ ((HQ + 2 * HQ * (cRain / RainMean)) / 3) 
                if cHQ > 1 [ set cHQ 1 ]
             ]
         ]
  ;; habitat quality for insectivorous and omnivorous birds, related to bush thickets         
         [ ;;if "bush thicket"
             ask patches 
             [ 
                set cHQ ((HQ + HQ * (cRain / RainMean)) / 2) 
                if cHQ > 1 [ set cHQ 1 ]
             ]
         ]  
 
end


;; CALCULATE CURRENT HQ OF HOME RANGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; observer procedure
  ;; Rainfall may change habitat quality of cells and thus home ranges. Therefor it is calculated each year after the rain (based on the "to caculate-HQ" submodel) 
  ;; and suitable home ranges depending on functional traits are defined.

to calculate-HQ-HR  
  
     ask patches with [ Nest? = TRUE ]
     [ 
         set cNest? FALSE 
         set overallHQ ((sum [cHQ] of patches in-radius 4) / 49)  ;; overallHQ is the meanHQ of all 49 home range cells
         if overallHQ > Threshold-overallHQ [ set cNest? TRUE ]
     ]
  
end


;; AGEING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; observer procedure
  ;; All birds age one year. 
  ;; Birds that reached age of maturity within these step become a floater. 
  ;; Birds which have reached their maximum age die.
  ;; (Calculation of Maturity see "INIT_calculate-parameters".)

to ageing
  
     ask turtles [ set Age Age + 1 ]
  
     ask turtles with [(Status = "Juvenile") and (Age >= Maturity)] 
     [ 
         set Status "JuvFloater" 
         set size 4
     ]
  
  ;; birds which reached maximum age
     ask turtles 
     [ 
         if Age >= Longevity
         [ 
             if Status = "Breeder"
             [ 
                 ask patches in-radius 4 [ set occupied? FALSE ]
                 ask turtles-here with [Status = "Juvenile"] 
                 [ 
                     set Status "JuvFloater"     ;; juveniles which survived winter become floater if the parents are dead                 
                     set size 4
                 ]
             ]
             die 
         ] 
     ]
     
  ;; testing maturity
     ;ask turtles with [Status = "Juvenile"]
     ;[ show Age ]
  
end


;; SEARCH-HR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; observer procedure
  ;; Floaters (including juveniles which reached age of maturity this year) have the chance to search for a suitable, unoccupied home range. 
  ;; First, all occupied nest sites are checked if the current habitat quality is sufficient; if not these birds loose their territories and become floater.
  ;; Second, floaters check potentially suitable home ranges around their current residential area; if there are unoccupied, suitable home ranges they stay there.
  ;; If there are no unoccpied, suitable home ranges, the floaters begin their interpatch-movement (see submodel "to disperse").

to search-HR

     ask patches [ set NoFloater 0 ]  ;; all Floaters are going to search for new territories

     ask turtles with [Status = "Breeder"]
     [
         ask patch-here 
         [
             if cNest? = FALSE       ;; if the current home range is not suitable after rainfall the bird becomes a floater and have to search for a new one
             [ 
                 ask myself 
                 [ 
                     set Status "Floater" 
                     set color blue
                 ]
                 
                 ask turtles-here with [Status = "Juvenile"]        ;; juveniles without a suitable home range have to search for a new one
                 [ 
                     set Status "JuvFloater"                        ;; dispersing juveniles have a higher mortality risk (see submodel "to survive")
                     set size 4
                 ]
                 ask patches in-radius 4 [ set occupied? FALSE ]
             ]
         ]
     ]
         
     ask turtles with [Status = "Floater" or Status = "JuvFloater"] 
     [   
         set color blue
         let patches-in-radius patches in-radius Search-Radius with [(cNest? = TRUE) and all? patches in-radius 4 [occupied? = FALSE]]         
         ifelse any? patches-in-radius
         [
             move-to max-one-of patches-in-radius [overallHQ]
             ;; place the bird on the center of the target patch (without this line the occupied home range could be larger than calculated!)
             setxy pxcor pycor
             set MyNest patch-here
             if Status = "JuvFloater" [ set Status "NewBreeder"]    ;; floaters and juveniles have a higher mortality risk (see submodel "to survive")
             if Status = "Floater" [ set Status "Breeder"]
             set color red
             ask patches in-radius 4 [ set occupied? TRUE ]         ;; ... bird occupies the whole home range    
         ]
         [ disperse-modus1 ]                                        ;; if not disperse (see next submodel "to disperse")
     ] ;;close ask turtles

     ask turtles with [Status = "Emigrant"] [ die ]
  
end


;; DISPERSE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; turtle procedure
  ;; sub-model of "to search-HR"
  ;; Inter-patch movement of floaters, if there is no unoccupied, suitable home range aroudn them.
  ;; disperse-modus1: Floaters choose randomly a direction (0°-360°) and fly 5 exploratory flights with a distance based on MedianDD (dependends on Mass) with a factor StepSize between 1 and 5 following a distribution presented in Sutherland et al. 2000.
  ;;                  At the end they search for suitable home rangesregular 
  ;; disperse-modus2: Floaters choose randomly a direction (0°-360°) and fly a distance of 70 cells (based on Bowman 2003, dispersal distance depends on home range area) with 5 steps for searching a new home range.
  ;; Both modi let the bird remember which was the unoccupied home range with highest overallHQ at each exploratory flight (modus 1) or at each step (modus 2) (or occupied home range resprectivley for becoming a floater).
  ;; The home range with the highest overallHQ out of these five possible home ranges is chosen.


;; MODUS 1

to disperse-modus1
   
     let HomeCell patch-here     
     
     if StepSize = 0
     [
         let RandomStep random-float 109.33
         if  RandomStep <  47.2                            [ set StepSize 1 ]
         if (RandomStep >= 47.2   and RandomStep < 77.2)   [ set StepSize 2 ]
         if (RandomStep >= 77.2   and RandomStep < 94.2)   [ set StepSize 3 ]
         if (RandomStep >= 94.2   and RandomStep < 101.7)  [ set StepSize 4 ]
         if (RandomStep >= 101.7  and RandomStep < 104.4)  [ set StepSize 5 ]
         if (RandomStep >= 104.4  and RandomStep < 105.2)  [ set StepSize 6 ]
         if (RandomStep >= 105.2  and RandomStep < 105.9)  [ set StepSize 7 ]               
         if (RandomStep >= 105.9  and RandomStep < 106.4)  [ set StepSize 8 ]
         if (RandomStep >= 106.4  and RandomStep < 106.8)  [ set StepSize 9 ]
         if (RandomStep >= 106.8  and RandomStep < 107.17) [ set StepSize 10 ]
         if (RandomStep >= 107.17 and RandomStep < 107.51) [ set StepSize 11 ]
         if (RandomStep >= 107.51 and RandomStep < 107.83) [ set StepSize 12 ]
         if (RandomStep >= 107.83 and RandomStep < 108.11) [ set StepSize 13 ]
         if (RandomStep >= 108.11 and RandomStep < 108.36) [ set StepSize 14 ]
         if (RandomStep >= 108.36 and RandomStep < 108.59) [ set StepSize 15 ]
         if (RandomStep >= 108.59 and RandomStep < 108.78) [ set StepSize 16 ]
         if (RandomStep >= 108.78 and RandomStep < 108.95) [ set StepSize 17 ]
         if (RandomStep >= 108.95 and RandomStep < 109.09) [ set StepSize 18 ]
         if (RandomStep >= 109.09 and RandomStep < 109.2)  [ set StepSize 19 ]
         if (RandomStep >= 109.2  and RandomStep < 109.28) [ set StepSize 20 ]
         if (RandomStep >= 109.28 and RandomStep < 109.28) [ set StepSize 21 ]                                                       
         if  RandomStep >= 109.28                          [ set StepSize 22 ]
     ]
                 
     repeat NoFlights
     [
         rt random-float 360            
         fd (StepSize * MedianDD)
         setxy pxcor pycor
         let patches-in-radius patches in-radius Search-Radius with [(cNest? = TRUE) and (all? patches in-radius 4 [occupied? = FALSE])]
         
      ;; testing esplorer flights   
         ;ask patches-in-radius [ ask patches in-radius Search-Radius [ set pcolor yellow ] ]
         
         ifelse any? patches-in-radius
         [ 
             let this-patch max-one-of patches-in-radius [overallHQ]
             
          ;; testing explorer flights           
             ;ask this-patch [ ask patches in-radius Search-Radius [ set pcolor cyan ] ]
             
             if [overallHQ] of this-patch > helpMyNestHQ
             [
                 set helpMyNest this-patch
                 set helpMyNestHQ [overallHQ] of this-patch
             ]
         ]
         [
             set patches-in-radius patches in-radius Search-Radius with [(sum [NoFloater] of patches in-radius Floater-Radius) < Maximum-Floater]
             if any? patches-in-radius
             [
                 let this-patch max-one-of patches-in-radius [overallHQ]
               
               ;; testing explorer flights                 
                 ;ask this-patch [ ask patches in-radius Search-Radius [ set pcolor red ] ]
                 
                 if [overallHQ] of this-patch > helpFloaterHRHQ
                 [
                     set helpFloaterHR this-patch
                     set helpFloaterHRHQ [overallHQ] of this-patch
                 ]
             ] ;; close if any? patches
         ] ;; close ifelse any? patches
         move-to HomeCell
         setxy pxcor pycor
     ] ;; close repeat
     
     ifelse helpMyNestHQ > 0
     [
         move-to helpMyNest
         ask patch-here [ ask patches in-radius Search-Radius [ set pcolor orange ] ]
         setxy pxcor pycor
         set MyNest patch-here 
         if Status = "JuvFloater" [ set Status "NewBreeder" ]
         if Status = "Floater" [ set Status "Breeder" ]
         set color red
         ask patches in-radius 4 [ set occupied? TRUE ]   ;; ... turtle occupies the best home range  
         set helpMyNest 0
         set helpMyNestHQ 0
         set helpFloaterHR 0
         set helpFloaterHRHQ 0
     ]
     [
         ifelse helpFloaterHRHQ > 0
         [
             move-to helpFloaterHR
             
          ;; testing explorer flights    
             ;ask patch-here [ ask patches in-radius Search-Radius [ set pcolor blue ] ]
             
             setxy pxcor pycor
             set Status "Floater"
             set color blue
             set size 4
             set NoFloater NoFloater + 1
             set helpMyNest 0
             set helpMyNestHQ 0
             set helpFloaterHR 0
             set helpFloaterHRHQ 0
         ]
         [
             let patches-in-radius patches in-radius Search-Radius with [(sum [NoFloater] of patches in-radius Floater-Radius) < Maximum-Floater]
             ifelse any? patches-in-radius
             [
                 move-to max-one-of patches-in-radius [overallHQ]
                 
              ;; testing explorer flights 
                 ;ask patch-here [ ask patches in-radius Search-Radius [ set pcolor blue ] ]
                 
                 setxy pxcor pycor
                 set Status "Floater"
                 set NoFloater NoFloater + 1
                 set helpMyNest 0
                 set helpMyNestHQ 0
                 set helpFloaterHR 0
                 set helpFloaterHRHQ 0
             ]
             [                 
                 set Status "Emigrant"
                 set color grey
                 set Emigrants Emigrants + 1 
             ]
         ]
     ]

end
   
   
;; MODUS 2

to disperse-modus2
  
     let patches-in-radius patches in-radius Search-Radius with [(sum [NoFloater] of patches in-radius Floater-Radius) < Maximum-Floater]
     if any? patches-in-radius
     [
        let this-patch max-one-of patches-in-radius [overallHQ]
        if [overallHQ] of this-patch > helpFloaterHRHQ
        [
             set helpFloaterHR this-patch
             set helpFloaterHRHQ [overallHQ] of this-patch
         ]
     ] ;; close if any? patches
     
     rt random-float 360
     repeat 5
     [             
         fd 14
         setxy pxcor pycor
         set patches-in-radius patches in-radius Search-Radius with [(cNest? = TRUE) and (all? patches in-radius 4 [occupied? = FALSE])]
         ifelse any? patches-in-radius
         [ 
             let this-patch max-one-of patches-in-radius [overallHQ]
             if [overallHQ] of this-patch > helpMyNestHQ
             [
                 set helpMyNest this-patch
                 set helpMyNestHQ [overallHQ] of this-patch
             ]
         ]
         [
             set patches-in-radius patches in-radius Search-Radius with [(sum [NoFloater] of patches in-radius Floater-Radius) < Maximum-Floater]
             if any? patches-in-radius
             [
                 let this-patch max-one-of patches-in-radius [overallHQ]
                 if [overallHQ] of this-patch > helpFloaterHRHQ
                 [
                     set helpFloaterHR this-patch
                     set helpFloaterHRHQ [overallHQ] of this-patch
                 ]
             ] ;; close if any? patches
         ] ;; close ifelse any? patches
     ] ;; close repeat
     
     ifelse helpMyNestHQ > 0
     [
         move-to helpMyNest
         set MyNest patch-here 
         if Status = "JuvFloater" 
         [ set Status "NewBreeder" ]
         if Status = "Floater" [ set Status "Breeder" ]
         set color red
         ask patches in-radius 4 [ set occupied? TRUE ]   ;; ... turtle occupies the best home range  
         set helpMyNest 0
         set helpMyNestHQ 0
         set helpFloaterHR 0
         set helpFloaterHRHQ 0
     ]
     [
         ifelse helpFloaterHRHQ > 0
         [
             move-to helpFloaterHR
             set Status "Floater"
             set helpMyNest 0
             set helpMyNestHQ 0
             set helpFloaterHR 0
             set helpFloaterHRHQ 0
         ]
         [
             set Status "Emigrant"
             set color grey
             set Emigrants Emigrants + 1 
         ]
     ]
     
end


;; REPRODUCE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; observer procedure
  ;; Adult birds which occupy a suitable home range (= "Breeders") with an overall habitat quality higher than "Threshold-overallHQ" produce clutches.
  ;; Clutch size is determined by chance to brood sucessfully (= "BreedingSuccess") and overall habitat quality of the occupied home range.

to reproduce
    
     ask turtles with [Status = "Breeder" or Status = "NewBreeder" ]
     [
         set cClutch (2.44 * Clutch * 0.5 * (2 * InfluenceHQ * [overallHQ] of patch-here + (1 - InfluenceHQ)))

      ;; testing current clutch size
         ;show cClutch
         
         let Clutch-reminder cClutch - (floor cClutch)   ;; residual value is saved in Clutch-remainder        

      ;; testing current clutch size
         ;show Clutch-reminder

         ifelse random-float 1 < Clutch-reminder       ;; -> possibility of another offspring
         [ 
             hatch (floor cClutch) + 1 
             [ 
                 set Status "Juvenile"
                 set color yellow
                 set size 2
                 set Age 0   ;; without this line Age is distributed randomly!
                 set StepSize 0
             ] 
         ]
         [ 
             hatch floor cClutch 
             [ 
                 set Status "Juvenile"
                 set color yellow
                 set size 2
                 set Age 0
                 set StepSize 0                 
             ]
         ]
     ] ;;close ask
  
end


;; SURVIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; observer procedure
  ;; Applies mortality to selected birds:
  ;;    - Mortality rate is apllied to breeders
  ;;    - A higher mortality rate is aplied to juveniles and floaters.
  ;; (Calculation of Longevity and Mortality see "INIT_calculate-parameters".)

to survive   
  
  ;; calculation of current mortality (cMortality) influenced by current HQ of the territories (overallHQ)
  
  ;; breeders
     ask turtles with [Status = "Breeder"]
     [        
         set cMortality (Mortality * ((2 * InfluenceHQ * (1 - [overallHQ] of patch-here)) +  (1 - InfluenceHQ)))
         if cMortality > 1 [ set cMortality 1 ]
         if cMortality < 0 [ set cMortality 0 ]

      ;; testing current mortality
         ;show cMortality

         if random-float 1 <= cMortality
         [ 
             ask patches in-radius 4 [ set occupied? FALSE ]
             ask turtles-here with [Status = "Juvenile"] [ die ]   ;; juveniles die if the parents are dead
             die 
         ] 
     ]
  
  ;; juveniles, juveniles which became breeders and floaters have a higher mortality rate
     ask turtles with [Status = "NewBreeder" or Status = "Juvenile"]
     [ 
         set cMortality (Mortality * ((2 * InfluenceHQ * (1 - [overallHQ] of patch-here)) +  (1 - InfluenceHQ)) + 0.1)
         if cMortality > 1 [ set cMortality 1 ]
         if cMortality < 0 [ set cMortality 0 ]

      ;; testing current mortality
         ;show cMortality

         if random-float 1 <= (cMortality) 
         [ 
             if Status = "NewBreeder" 
             [ 
                 ask patches in-radius 4 [ set occupied? FALSE ] 
                 ask turtles-here with [Status = "Juvenile"] [ die ]   ;; juveniles die if the parents are dead
             ]
             die 
         ]        
     ]
     ask turtles with [Status = "NewBreeder"] [ set Status "Breeder"]     
     
     ask turtles with [Status = "Floater"]
     [
         set cMortality (Mortality * ((2 * InfluenceHQ * (1 - (sum [cHQ] of patches in-radius 4) / 49)) +  (1 - InfluenceHQ)) + 0.2)
         if cMortality > 1 [ set cMortality 1 ]
         if cMortality < 0 [ set cMortality 0 ]                  
         if random-float 1 <= (cMortality) [ die ]
     ]
  
end




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VISIUAL PROCEDURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; UPDATE VIEW ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; observer procedure
  ;; Update of world interface. Shows either: 
  ;;    - dominating vegetation type (shrubs = brown, grass = green, bare soil = black) or
  ;;    - habitat quality (the larger quality the lighter the green shade) or 
  ;;    - occupied home ranges (orange), suitable cells (green) and unsuitable cells (grey).

to update-view
  ;; show dominating vegetation type
     if View = "Vegetation"
     [
         ask turtles [ show-turtle ]       
         ask patches 
         [
           if pTree != 0 [ set pcolor 0 ]                                      ;; black
           if pGrass > pShrubs [ set pcolor scale-color lime pGrass 120 -20 ]  ;; green
           if pGrass < pShrubs [ set pcolor scale-color 25 pShrubs 160 -20 ]   ;; orange 
           if (pGrass + pShrubs + pLargeShrub + pTree) = 0 [ set pcolor 9.9 ]  ;; white 
         ]
     ]
     
  ;; show habitat quality
     if View = "Habitat quality"
     [
         ask turtles [ show-turtle ]       
         ask patches 
         [
           if HQ > 0 [ set pcolor scale-color lime cHQ 1.2 -0.1 ]  ;; the larger cHQ, the darker the green shade; wider range so there is no black (min) and white (max) 
           if HQ = 0 [ set pcolor black ]
         ]
     ]
     
  ;; show occupied home ranges as well as suitable and unsuitable cells
     if View = "Home ranges"
     [   
         ask turtles [ show-turtle ]                
         ask patches with [occupied? = TRUE] [ set pcolor orange ]   ;; occupied home ranges are orange
         ask patches [ set suitable? FALSE ]
         ask patches with [(cNest? = TRUE) and (all? patches in-radius 4 [occupied? = FALSE]) and (overallHQ > Threshold-overallHQ)]
         [ ask patches in-radius 4 [ set suitable? TRUE ] ]                  
         ask patches with [(suitable? = TRUE) and (occupied? = FALSE)] [ set pcolor 66 ]   ;; suitable unoccupied home ranges are green
         ask patches with [(suitable? = FALSE) and (occupied? = FALSE)] [ set pcolor 9 ]   ;; unsuitable cells are grey 
     ] 
  
  ;; no visual update
     if View = "OFF" 
     [ 
         ask patches [ set pcolor black ] 
         ask turtles [ hide-turtle ]
     ]
  
end


;; INIT: UPDATE VIEW ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; observer procedure
  ;; Update of world interface (see submodel "to update-view"), using HQ instead of cHQ and Nest? instead of cNest? because in INIT cHQ and cNest? are not available.

to INIT_update-view
  
     if View = "Habitat quality"
     [
         ask turtles [ show-turtle ]       
         ask patches with [HQ > 0] [ set pcolor scale-color lime HQ 1.2 -0.1 ]       ;; the larger HQ, the darkerer the green shade
         ask patches with [HQ = 0] [ set pcolor black ]                              ;; if HQ is 0 the patch is black
     ]
     
     if View = "Home ranges"
     [
         ask turtles [ show-turtle ]       
         ask patches with [occupied? = TRUE] [ set pcolor orange ]   ;; occupied home ranges are orange
         ask patches [ set suitable? FALSE ]
         ask patches with [(Nest? = TRUE) and (all? patches in-radius 4 [occupied? = FALSE]) and (overallHQ > Threshold-overallHQ)]
         [ ask patches in-radius 4 [ set suitable? TRUE ] ]                  
         ask patches with [(suitable? = TRUE) and (occupied? = FALSE)] [ set pcolor 66 ]   ;; suitable unoccupied home ranges are green
         ask patches with [(suitable? = FALSE) and (occupied? = FALSE)] [ set pcolor 9 ]   ;; unsuitable cells are grey  
     ]
  
end


;; SHOW VEGETATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to showVeg
     
     ask patches [ set plabel " "] 
     ask patches with [pTree != 0] [ set pcolor 0 ]                                      ;; black
     ask patches with [pGrass > pShrubs] [ set pcolor scale-color lime pGrass 120 -20 ]  ;; green
     ask patches with [pGrass < pShrubs] [ set pcolor scale-color 25 pShrubs 160 -20 ]   ;; orange 
     ask patches with [(pGrass + pShrubs + pLargeShrub + pTree) = 0] [ set pcolor 9.9 ]  ;; white 
     
end


;; SHOW HQ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to showHQ      

  ;; sets number of basic vegetation cells per side which were aggregated to body mass depending cells 
  ;;    SLCell is rounded to an even number so it is a multiple of basic vegetation cells (2m x 2m); divided by 2 it represents the number of cells per side which were aggregated
  ;;    Example (10 g, "bush thicket"): Homerange = 0.98 ha -> SLHomerange = 99.04 m -> SLCell = 14.15 m -> SLCell_rounded = 14 m -> Scale = 7 -> 7*7 = 49 basic cells were aggregated to 1 new cell
     if Habitatpreference = "open grassland"
         [    
              if Mass = 10 [ set Scale 9 ] 
              if Mass = 30 [ set Scale 18 ]
              if Mass = 100 [ set Scale 41 ] 
         ]
     if Habitatpreference = "bush thicket"
         [ 
              if Mass = 10 [ set Scale 7 ] 
              if Mass = 30 [ set Scale 11 ] 
              if Mass = 100 [ set Scale 20 ] 
         ] 

     ifelse Habitatpreference = "open grassland"
     [ 
         ask patches 
         [ 
             set HQ (-0.1 + 0.02 * pGrass)
             if HQ < 0 [ set HQ 0 ] 
             if HQ > 1 [ set HQ 1 ]
             set pcolor scale-color lime HQ 1.2 -0.1                ;; the larger cHQ, the darker the green shade
             if Labels = TRUE [ set plabel precision  (HQ * 10) 0 ]
         ]
     ]
     [ ;;if "bush thicket"
         ask patches 
         [ 
             set pAllShrubs (pShrubs + (pLargeShrub * 100 / Scale))
             set HQ (-0.458 + 0.1082 * pAllShrubs - 0.0024 * pAllShrubs * pAllShrubs)
             ;set HQ (-0.458 + 0.1082 * pShrubs - 0.0024 * pShrubs * pShrubs)
             if HQ < 0 [ set HQ 0 ] 
             if HQ > 1 [ set HQ 1 ]
             set pcolor scale-color lime HQ 1.2 -0.1                ;; the larger cHQ, the darker the green shade      
             if Labels = TRUE [ set plabel precision  (HQ * 10) 0 ]
         ]
     ] 
     if Labels = FALSE [ ask patches [ set plabel " " ] ]
  
end

;; SHOW HRs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to showHR
  
     ask patches 
     [ 
         set NestPatch 0
         set overallHQ 0
         set pcolor black
         set plabel " "
         set plabel-color white
     ]
     
  ;; criteria for suitable home ranges for tree-breeding birds (depending on number of trees)
     ifelse Nestsite = "on trees"
     [
         ask patches with [(pTree >= Threshold-Trees)] 
         [ 
             set overallHQ ((sum [HQ] of patches in-radius 4) / 49)
             if overallHQ > Threshold-overallHQ 
             [ 
                 set NestPatch 1
                 if Labels = TRUE [ set plabel precision (overallHQ * 10) 0 ]
                 ask patches in-radius 4 [ set pcolor blue ]
             ]
         ]
     ]
     [   
  ;; criteria for suitable home ranges for shrub-breeding birds (depending on number of large shrubs) 
         ifelse Nestsite = "on large shrubs"
         [
             ask patches with [(pLargeShrub >= Threshold-LargeShrubs)]
             [ 
                 set overallHQ ((sum [HQ] of patches in-radius 4) / 49)
                 if overallHQ > Threshold-overallHQ 
                 [ 
                     set NestPatch 1
                     if Labels = TRUE [ set plabel precision (overallHQ * 10) 0 ]
                     ask patches in-radius 4 [ set pcolor blue ]
                 ]
             ]
         ]
  ;; criteria for suitable home ranges for ground-breeding birds (depending on proportion of shrubs) 
         [ ;;if Nestsite = "ground-breeding"
             ask patches with [(pShrubs <= Threshold-Shrubs)]
             [ 
                 set overallHQ ((sum [HQ] of patches in-radius 4) / 49)
                 if overallHQ > Threshold-overallHQ 
                 [ 
                     set NestPatch 1
                     if Labels = TRUE [ set plabel precision (overallHQ * 10) 0 ]
                     ask patches in-radius 4 [ set pcolor blue ]
                 ]
             ]
         ]
     ]
     ask patches with [NestPatch = 1] [ set pcolor orange ]
     if Labels = FALSE [ ask patches [ set plabel " " ] ]
  
end


;; SHOW NESTSITES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to showNest
  
     ask patches 
     [ 
         set pcolor black 
         set plabel " "
         set plabel-color white
     ]
     ask patches with [NestPatch = 1] [ set pcolor orange ]
     ifelse Nestsite = "on trees" 
     [ 
         ask patches 
         [ 
             set plabel pTree 
         ] 
     ]
     [
         ifelse Nestsite = "on large shrubs" 
         [ 
             ask patches 
             [ 
                 set plabel pLargeShrub 
             ]
         ]
         [ 
             ask patches 
             [ 
                 set plabel precision pShrubs 0 
             ] 
         ] 
     ]
     if Labels = FALSE [ ask patches [ set plabel " " ] ]
  
end




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@#$#@#$#@
GRAPHICS-WINDOW
241
11
1188
979
-1
-1
2.082222222222222
1
10
1
1
1
0
1
1
1
0
449
0
449
1
1
1
ticks
30.0

OUTPUT
1193
584
1501
788
10

PLOT
1193
402
1500
576
Precipitation
year
amount of rain [mm]
0.0
10.0
0.0
1000.0
true
false
"" "if not any? turtles [ stop ]"
PENS
"default" 1.0 0 -14070903 true "plot cRain" "plotxy ticks cRain"

PLOT
1193
208
1497
393
                        Population structure
year
# birds per class
0.0
10.0
0.0
10.0
true
true
"" "if not any? turtles [ stop ]"
PENS
"Breeders" 1.0 0 -5298144 true "" "plot count turtles with [Status = \"Breeder\"]"
"Floaters" 1.0 0 -14070903 true "" "plot count turtles with [Status = \"Floater\"]"
"Juveniles" 1.0 0 -1184463 true "" "plot count turtles with [Status = \"Juvenile\" or Status = \"JuvFloater\"]"
"Emigrants" 1.0 0 -5987164 true "" "plot Emigrants"

PLOT
1193
12
1497
210
Population size
year
# birds in total
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles"

MONITOR
1433
294
1492
339
# birds
count turtles
0
1
11

MONITOR
1433
342
1492
387
Year
ticks
17
1
11

PLOT
8
178
237
316
Landscape
NIL
NIL
0.0
1.0
0.0
1.0
false
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles"

TEXTBOX
14
196
231
311
q
11
139.9
0

PLOT
8
321
236
442
Functional Traits
NIL
NIL
0.0
1.0
0.0
1.0
false
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles"

TEXTBOX
14
337
230
437
FT
11
139.9
0

SWITCH
17
273
116
306
Labels
Labels
1
1
-1000

BUTTON
120
237
227
270
(2) Show HRs
showHR
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
119
273
227
307
(3) Show Nests
showNest
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
17
237
116
270
Show Vegetation
showVeg
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
16
340
81
400
Mass
30
1
0
Number

CHOOSER
85
340
226
385
Habitatpreference
Habitatpreference
"bush thicket" "open grassland"
0

CHOOSER
86
388
227
433
Nestsite
Nestsite
"on trees" "on large shrubs" "ground-breeding"
2

PLOT
9
725
239
979
Thresholds & Precipitation
NIL
NIL
0.0
1.0
0.0
1.0
false
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles"

TEXTBOX
17
743
232
974
thr
11
139.9
0

INPUTBOX
19
746
116
806
Threshold-Trees
1
1
0
Number

INPUTBOX
119
746
227
806
Threshold-Shrubs
40
1
0
Number

INPUTBOX
19
808
227
868
Threshold-LargeShrubs
5
1
0
Number

SLIDER
19
873
227
906
Threshold-overallHQ
Threshold-overallHQ
0
0.5
0
0.01
1
NIL
HORIZONTAL

INPUTBOX
20
908
130
968
RainMean
300
1
0
Number

INPUTBOX
133
908
229
968
RainStd
150
1
0
Number

PLOT
9
447
238
721
Birds
NIL
NIL
0.0
1.0
0.0
1.0
false
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles"

TEXTBOX
15
464
232
717
birds\n
11
139.9
0

INPUTBOX
19
467
225
527
Population-Start
500
1
0
Number

SLIDER
19
530
225
563
Search-Radius
Search-Radius
0
27
4
0.5
1
cells
HORIZONTAL

SLIDER
19
604
226
637
Maximum-Floater
Maximum-Floater
0
10
1
1
1
floater(s) per HR
HORIZONTAL

PLOT
8
10
236
173
Control
NIL
NIL
0.0
1.0
0.0
1.0
false
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles"

TEXTBOX
15
27
230
168
plot\n
11
139.9
0

BUTTON
19
32
94
65
NIL
Setup
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

BUTTON
96
32
159
65
NIL
Go
NIL
1
T
OBSERVER
NIL
G
NIL
NIL
1

BUTTON
162
32
225
65
Run
Go
T
1
T
OBSERVER
NIL
R
NIL
NIL
1

BUTTON
18
200
116
233
Create Landscape
INIT_import-landscape\nshowVeg
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
120
200
227
233
(1) Show HQ
showHQ
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
125
118
227
163
View
View
"Vegetation" "Home ranges" "Habitat quality" "OFF"
1

BUTTON
19
132
120
165
Update View
update-view
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
20
678
227
711
InfluenceHQ
InfluenceHQ
0
1
0.5
0.01
1
NIL
HORIZONTAL

INPUTBOX
19
69
120
129
FileName
GrassHigh_Sc11_1
1
0
String

CHOOSER
124
69
226
114
Import
Import
"Path" "FileName" "OFF"
1

SLIDER
19
567
225
600
NoFlights
NoFlights
0
9
4
1
1
flights per year
HORIZONTAL

SLIDER
20
640
227
673
Floater-Radius
Floater-Radius
1
10.5
4
0.5
1
cells
HORIZONTAL

@#$#@#$#@
# Animal Functional Type Model

The info tab is divided in two parts. First, there is a description of each button, chooser, slider and switch on the interface tab which explains shortly the most important details to use the model. After that the detailed description of the entire model is presented as Overview, Design concepts, Details (ODD) protocol (GRIMM et al. 2006, GRIMM et al. 2010).



# Interface & Controlling


## Section _Control_


**Button _Setup_**
Runs the setup. Creates a landscape and a population of birds, which search for suitable home ranges. Calculates all spatial and population parameters that are needed to setup the model.


**Button _Go_**
Runs the model once (equal to one year). Within each year the following processes are repeated in the respective order: rainfall, calculate habitat quality, reproduction, survival, search home ranges and aging. After this state variables and the list of residents and floaters are synchronously updated. 


**Button _Run_**
Runs the go procedure forever.


**Button _Setup & Run_**
Runs the setup procedure once and the go procedure forever.


**Input _FileName_**
Needed if _Import = FileName_: imports the specified .csv-file.


**Chooser _Import_**
If _Import = Path_ : Opens a dialog that allows the user to choose an existing file on the system.
If _Import = FileName_ : Imports the file "FileName".csv (must be located in the same folder)
If _Import = OFF_ : uses a previously imported landscape and deletes all turtles, drawings, plots and outputs as well as it resets all variables.


**Chooser _View_**
There are four optional views on the world:

  1. Vegetation: 
Shows dominating vegetation type of each cell. Patches with a higher amount of grass compared to the amount of shrubs are colored green (the darker the shade the higher the amount of grass). Patches dominated by shrubs are colored orange (the darker the shade the higher the amount of srubs). If the cell contains only bare soil it is colored white.

  2. Home ranges:
Show suitable home ranges and if they are occupied. Home ranges are colored green if they are suitable and available and colored orange if they are occupied. Suitable home ranges depend on functional trait _Nestsite_ and habitat quality. Cells not suited for breeding are colored grey (= matrix). 

 3. Habitat quality:
Shows habitat quality (HQ) of each cell. HQ depends on functional trait _Habitatpreference_. The darker the shade, the higher HQ.

  4. OFF:
All cells are colored black. Birds are invisible.


**Button _Update View_**
Updates view immediately. The procedure _Update View_ is also run by the go procedure.



## Section _Landscape_

This section is only for inspecting and testing imported landscapes.


**Button _Create Landscape_**
Creates only a landscape WITHOUT a population of birds. If Import in the control section is _ON_  it imports a given landscape file. If _Import = OFF_ it uses a previously imported landscape (compare setup procedure).


**Button _Show Vegetation_**
Shows vegetation. Colors are similar to _View = Vegetation_. 


**Button _(1) Show HQ_** 
Shows habitat quality of each cell. If _Labels = ON_ labels show HQ * 10 (ranging from 0 to 10 instead of 0.0 to 1.0).


**Button _(2) Show HRs_** 
Shows habitat quality of each cell. Cells suitable as nest sites are colored orange, cells which belong to these home ranges are colored blue. If _Labels = ON_ labels show overallHQ * 10 for nest site cells (ranging from 0 to 10 instead of 0.0 to 1.0).
NOTE: Only run after _(1) Show HQ_ because HQ is needed for calculation!


**Button _(3) Show Nests_** 
Shows cells suitable as nest sites. These cells are colored orange, all others are colored black. If _Labels = ON_ labels show number of trees or large shrubs or amount of shrubs respectively (depending on functional trait _Nestsite_).
NOTE: Only run after _(1) Show HQ_ and _(2) Show HRs_!


**Switch _Labels_**
If _Labels = ON_: labels are displayed.
If _Labels = OFF_: labels are invisible.



## Section _Functional Traits_

The functional type approach is characterized by a set of functional traits: body mass, habitat preference (open grassland, bush thicket), and nest site needs (on trees, on large shrubs, ground-breeding).


**Input _Mass_**
Functional trait body mass in gram. Mass values are assumed to be continuous. 


**Chooser _Habitatpreference_**
Funcitonal trait preferred habitat, which is related to foraging. There are two types of preferred habitat:

  1. open grassland

  2. bush thicket 

Herbivorous and carnivorous species are assumed to be associated with open grasslands while omnivorous and insectivorous species are assumed to be associated with bush thickets.


**Chooser _Nestsite_**
Functional trait nest site needs. For each type there are different thresholds for suitable nest sites. There are threee types which characterise a cell as potentially suited nest site:
  
  1. on trees
  
  2. on large shrubs

  3. ground-breeding

Ground-breeding birds need open landscape and is thus related to shrub cover.



## Section _Birds_


**Input _Population-Start_**
Population size of females at the beginning of each simulation.  We simplified the model by considering females only and assuming that all females will find a mate.


**Slider _Search-Radius_**
Defines the radius of cells which are inspected when loaters search for a home range. This search radius is applied several times within the birds interpatch movement while the bird is flying in a given direction.


**Slider _Maximum-Floater_**
Sets the maximum number of floaters per home range or breeder resprectively.



## Section _Thresholds & Precipitation_


### Thresholds nest sites: 
Suitable nest site cells arecharacterised by vegetation type (depending on functional trait _Nestsite_) and by HQ and overallHQ respectively (depending on functional trait _Habitatpreference_). overallHQ is the mean of HQ of all home range cells belonging to that nest site cell. 


**Input _Threshold-Trees_**
Threshold for (potentially) suitable nest site cells if _Nestsite = on trees_. Suitable if total number of trees of a cell is equal or higher than this threshold.


**Input _Threshold-Shrubs_**
Threshold for (potentially) suitable nest site cells if _Nestsite = ground-breeding_. Suitable if the amount of shrubs of a cell is equal or lower than this threshold.


**Input _Threshold-LargeShrubs_**
Threshold for (potentially) suitable nest site cells if _Nestsite = on large shrubs_. Suitable if total number of large shrubs of a cell is equal or higher than this threshold.


**Slider _Threshold-overallHQ_**
If overallHQ of a potentially suitable cells is equal or higher than this threshold, cells is suitable as nest site.


### Precipitation:
Current rainfall is drawn from a normal distribution with a mean annual rainfall and a given standard deviation. 


**Input _RainMean_**
Sets the mean annual rainfall in mm.


**Input _RainStd_**
Sets the standard deviation in annual rainfall in mm.






# The ODD protocol

The description of the individual-based, spatial-explicit model follows the Overview, Design concepts, Details (ODD) protocol (GRIMM et al. 2006, GRIMM et al. 2010).

## Overview

### Purpose

The purpose of the Animal Functional Type (AFT) model is to understand which functional traits of birds in savannas are reacting most sensitive to changes in the environment and which functional types are most threatened. The general interest is the application of functional types in modeling animal population dynamics.

### Entities, state variables, and scales

The model comprises four hierarchical levels: individuals, vegetation cells, home ranges, and landscape. 
 
**Individuals** are characterized by the state variables: identity number (who), its current location (x- and y-coordinates), age, social status and its functional type. The functional types are characterized by a set of functional traits: body mass (continuous value), habitat preference (open grassland, bush thicket), and nest site needs (on trees, on large shrubs, ground-breeding) (Table 1). The functional traits are the same for all individuals of the simulated population and they do only change for different runs. The body mass determines reproduction rate, age of maturity, longevity, mortality rate, home range size and dispersal range. Herbivorous and carnivorous species are assumed to prefer open grasslands while omnivorous and insectivorous species are assumed to be associated with bush thickets. This assumption is made because herbivorous and carnivorous species need open habitats to search for leaves, seeds, buds and fruits and to hunt and catch their prey respectively (DONALDSON & KELK 1970, SEYMOUR & DEAN 2010). Insectivorous birds need increased habitat hetero-geneity since arthropod abundances increase until shrubs become the only dominant vegetation form (BLAUM et al. 2009; SEYMOUR & DEAN 2010). Omnivorous birds are assumed to change their diet seasonally for whatever food sources are most readily available, such as eating insects in spring and summer. Nest site needs describe the preferred nest site structures; we differentiate between birds breeding on trees and birds breeding on large trees as well as ground-breeders. Social status depends on the current situation of the adult and differentiates between juveniles, breeders and floaters. Individuals who have not completed their first year are referred to as juveniles while older ones are referred to as adults. Breeders have an own breeding home range while floaters are sexually mature, but non-breeding individuals (SMITH 1978). We simplify the model by considering females only and assuming that all females will find a mate since most bird populations have a skewed sex ration with a higher proportion of males in the population (e.g. DONALD 2007 ). Furthermore, we exclude group living species. The model is built to work with body masses between 10g and 1000 g because this range covers the major part of world’s birds species (BLACKBURN & GASTON 1994).  

**Table 1:** List of all functional traits used in the animal functional type model, the values that can be chosen and related parameters and variables as well as the equation(s) for calculating the parameters (in parentheses, see section “3.3 Submodels”).
![Table Traits AFT] (file:_Table_traits.jpg)

**Vegetation cells** are characterized by the state variables: location, proportion of vegetation types, the number of trees and large shrubs and its habitat quality (HQ, depending on the preferred habitat). Each vegetation cell contains ratio of grass, shrubs and bare ground (continuous values) as well as the number of trees and large shrubs. The vegetation cells differ in size depending on the given body mass (Eq. 1 and 2).

**Home ranges** are characterized by the state variables: location (x- and y-coordinates), the number and identity numbers of the individuals it contains, the total number of trees and large shrubs, proportion of shrubs and grasses and its overall HQ. The home range grid is based on the vegetation grid. The size of the home range depends on the body mass since home ranges of birds depend strongly on their size (OTTAVIANI et al. 2006). Each home range consists of 49 vegetation cells within a semi-round radius of 4 cells; thus the vertical and horizontal diameter of a home range is 9 vegetation cells. The home range grid consists of minimum 50 × 50 potential home ranges, i.e. the landscape grid consists of minimum 450 × 450 up to 477 × 477 vegetation cells (see Table 4). Because of the allometric relationship between body mass and home range area the cell size changes with body mass. In particular, assuming a 14 m × 14 m vegetation cell for the smallest home range results in a home range diameter of 126 m (omnivorous or insectivorous bird with a body mass of 10 g, Eq. 1 and 4). The maximum vegetation cell size is almost 386 m ⨯ 386 m (herbivorous or carnivorous bird with a body mass of 1000 g, Eq. 2 and 5). We assume home ranges to be defended territories, thus each home range can be inhabited by one breeding pair (one female) only. Additional floaters can stay in each home range beside the breeding pair but it cannot reproduce (SMITH 1978; NEWTON 1988). There is no consensus in the number of floaters per breeding pair in literature. NEWTON (1988) observed for peregrine falcons that a ratio of one or more floaters to one breeder could be expected for healthy populations; thus the number of floaters per breeding female is adjustable. Because density dependence is one of the key processes regulating bird population dynamics (TURCHIN 1995; NEWTON 1998) the strong relationship between breeding individuals and floaters should be considered because it has the potential to affect population dynamics beside the breeding population (PENTERIANI et al. 2011). The overall HQ of the home range depends on the needs of the functional type and is characterized by the number of trees and large shrubs and the proportion of shrubs and grass of the cells associated with this home range.

**Landscape**is the highest hierarchical level in the model and it is characterized by the state variables: rain intensity and vegetation. The overall landscape is characterized by the proportion of grass and shrubs as well as the density of trees and large shrubs of all cells and the clumping intensities of shrubs, large shrubs and trees. The grid is built as a torus; this means when a bird crosses the edge of the grid, it appears on the other side (i.e. emigration from the study site equals immigration). Home ranges do also cross grid boundaries if necessary.


### Process overview and scheduling

The model proceeds in annual time steps. Within each year the following processes are repeated in the respective order: rainfall, calculation of HQ, aging, survival, dispersal and reproduction. After this state variables and the set of all individuals as well as residents, floaters, emigrants and juveniles in particular are synchronously updated. Processes take place within each cell and individual respectively. Within each step, individuals and cells are processed in a random order. Figure 1 depicts a flow chart of the model. Processes are described in detail in section “3.3 Submodels” below.

![Flow chart AFT] (file:_Flow_Chart_AFT.jpg)
**Figure 1:** Flow chart of the animal functional type model including initialization and submodels. We distinguish between landscape related submodels (green) and population dynamics related submodels (blue). The model proceeds in annual time steps. For a summary of all functional traits and adjustable parameters see Table 1 and Table 2. For a detailed description of each process see section “3.3 Submodels”.


For all simulation experiments of each functional type, I generated minimum 5 replicate landscapes according to the scenario conditions (Table 5). I systematically investigated 9 landscape scenarios, which varied in the proportion of shrubs and grasses (grass-dominated, shrub-dominated and an intermediate state) as well as in the clumping intensity of shrubs (lowly, medially and highly clumped). For each functional type and landscape 100 simulation runs were conducted. 

The simulation of a predefined population ends after 200 years or if the population goes extinct within this time span. Output variables are the number of breeders (equivalent to number of occupied home ranges) and floaters respectively by averaging the numbers of the last 20 years. If a population goes extinct the extinction time is recorded. Then I calculated the ratio between breeders and floaters and the overall probability that a population will go extinct within 200 years.



## Design concepts

### Basic principles

We apply a functional type approach by using ecological groups. This approach uses a “non-phylogenetic classification leading to a grouping of organisms that respond in a similar way to a syndrome of environmental factors” (GITAY & NOBLE 1997). While classifications based on ecological and functional traits have a long tradition in plant ecology their application in animal ecology remains challenging (BLAUM et al. 2011). In nature conservation there is an advantage of using such classification systems instead of single species approaches because it reduces the working expenditures and thus costs and time (BLAUM et al. 2011). Therefore, we apply and test the functional type approach for multiple bird species in arid systems to understand which traits are most threatened.

### Emergence

The population dynamics emerge from the life history of each individual, which derive from empirical and stochastic rules. The main drivers of the population characteristics are HQ (“quality” depends on the vegetation, which is preferred by the current functional type) and precipitation, which results in more or less primary production related to food and resources and thus to offspring numbers, survival rates and number of possible home ranges. 
 
### Adaptation

Floaters and juveniles who reached maturity must search for a breeding territory if the home range they are actually staying in is occupied by a breeding pair (female) or if the home range does not match their needs anymore.  Within their search they compare home ranges and select the best one related to overall HQ. If there is no unoccupied territory they choose the home range with the highest overall HQ for staying as a floater. The rules for dispersal in general and the estimation of the HQ of a patch are explained in the section “3.3 Submodels – Dispersal” and “3.3 Submodels – Calculate habitat quality (HQ)” respectively.

### Objectives

The agent objective is to maximize their fitness in terms of offspring and longevity. The reproductive output is determined by finding an unoccupied suitable home range (finding a mate) and by HQ of the chosen home range. Thus HQ of the home range should be maximized. In general, HQ depends on the preferred habitat, on the vegetation composition and on current precipitation, which influences current biomass production of plants. The calculated HQ includes implicitly both, the availability of food resources and predator shelter and determines the reproductive output. The rules for calculating HQ are explained in the section “3.3 Submodels – Calculate habitat quality (HQ)”. HQ is directly related to the functional type.

### Learning

In the model there is no learning implemented.

### Prediction

The individuals cannot predict the future conditions.

### Sensing

All individuals know the overall HQ of the home range they are currently staying in, their own social status (breeder or floater) and age. If an animal is dispersing it gets information about the overall HQ of home ranges within its dispersal searching radius (see section “3.3 Submodels – Dispersal”).

### Interaction

If a home range is already inhabited by a female (or a breeding pair respectively), another individual can settle down as a floater but it cannot reproduce. If there is already a resident and an additional floater the individual has to find another home range (see section “3.3 Submodels – Dispersal”).

### Stochasticity

Creation of the landscape grid is a stochastic process. Generally there are two parameters for each vegetation type which determine if the algorithm is random or rather clumped: a probability for turning a cell into an occupied vegetation cell with one or more neighbouring cells containing the same vegetation type (Thr_neighbour) and a probability for neighbouring cells not containing the same type (Thr_empty). The greater the difference between Thr_neighbour and Thr_empty the greater the degree of clumping will be. Unlike trees and shrubs, where we use a binary system (yes/no), grass cover is assumed to differ also on a 2 m-scale. Therefor grass is distributed using ThrG_empty and ThrG_neighbour and a normal distribution with a mean of 1.5 • GrassCover and a standard deviation of 0.15. To admit a regular distribution of grasses, the mean is adjusted to the overall amount of grass. If one or more neighbour cells contain woody vegetation the mean is set to GrassCover because we assume a descending gradient of grass cover towards shrubs and trees.
At the beginning of each year the current rainfall is drawn from a normal distribution if there is no specific precipitation input data. Within the submodel reproduction there is a 50 percent probability for the offspring to be female or male respectively. If an individual is searching for a new territory, the direction of the dispersal is determined randomly. Also the step size of the dispersal distance is drawn from a distribution using data from SUTHERLAND et al. (2000).

### Collectives

Each simulated population is characterized by specific functional traits for all individuals.

### Observation

The main results are the extinction risk of the given functional type and/or mean population size for the last 20 years. After 200 years numbers of breeders and floaters are estimated for the last 20 years and the ratio between breeders and floaters is calculated. Furthermore, the annual precipitation is recorded.

## Details

### Initialization

Functional traits (Table 1), population size, search radius, maximum number of floaters per breeding pair, influence of HQ on reproduction and mortality, thresholds for nest sites and suitable home ranges as well as mean and standard deviation of annual rainfall are adjustable variables.  Table 2 shows these parameters and the chosen values for my simulations. Based on the defined functional traits the dependent parameters and variables are calculated and set respectively. Apart from the following calculations of grid-relevant parameters within the initialization, calculation of all parameters are shown within the submodels because they are calculated and recalculated within the runs (compare Table 1 and section 3.3).

**Table 2:** Adjustable parameters and chosen values for the AFT model.
![Table Parameters AFT] (file:_Table_parameters_AFT.jpg)

The basic landscape  grid consists of 477 × 477 vegetation cells which are 2 m x 2m and contain either only one of four vegetation types or bare soil. Generally there are two parameters for each vegetation type which determine if the algorithm is random or rather clumped: a probability for turning a cell into an occupied vegetation cell with one or more of eight neighbouring cells containing the same vegetation type (Thr_neighbour) and a probability for neighbouring cells not containing the same type (Thr_empty) (Table 3). The greater the difference between Thr_neighbour and Thr_empty is the greater is the degree of clumping. 
First, trees are distributed depending on ThrT_empty and ThrT_neighbour until the maximum number of trees is reached (TreeCover). Within this procedure not only neighbouring cells are considered as neighbours but cells in radius between DistT and DistTneighbour cells. MOUSTAKAS et al. (2008) found a mean inter-stem distance DistT of 20–25 m to four nearest neighbours and a maximum percentage of tree canopy cover of 3% in the Souhthern Kalahari (MAP 411 mm). Second, shrubs and large shrubs are distributed using ThrS_empy and ThrS_neighbour. Some of these cells are randomly turned into large shrubs while maximum covers are not reached (Shrub_Cover, LShrub_Cover and total shrub cover). At least, grasses are distributed. Unlike trees and shrubs which are binary (yes/no) on this scale, grass cover is assumed to differ also on a 2 m-scale. Therefor grass is distributed using ThrG_empty and ThrG_neighbour and a normal distribution with a mean of 1.5 • GrassCover and a standard deviation of 0.15.  To admit a regular distribution of grasses, the mean is adjusted to the overall amount of grass. If one or more neighbour cells contain woody vegetation the mean is set to GrassCover because we assume a descending gradient of grass cover towards shrubs and trees (Figure 2).
To build up the specific landscape grid, the home range size (HR, in ha) is calculated based on the body mass (BM, in g) once in the initial process. The home range area depends mainly on the diet (OTTAVIANI et al. 2006).

(Eq. 1) _HR [ha] = 0.129 × BM[g]^0.881_ for omnivorous/insectivorous birds
(Eq. 2) _HR [ha] = 0.0676 × BM[g]^1.344_ for herbivorous/carnivorous birds

The allometric scaling is estimated on traits of African birds (NEWMAN 2002, HOKEY et al. 2005).

**Table 3:** List of parameters determining the resulting landscape, sorted by vegetation type,  values and references.
![Table Parameters Landscape] (file:_Table_parameters_Landscape.jpg)

Because the specific landscape grid should cover a minimum of 50 × 50 potential home ranges and each home range is divided in 7 × 7 vegetation cells (semi-round with a maximum diameter of 9 vegetation cells), the landscape grid contains at least 450 × 450 vegetation cells (Table 4). The basic landscape grids are aggregated to match the functional types. Therefor the cell size, which differs with body mass, must be calculated using the specific home range area (equation 1 or 2):

(Eq. 3) _Cell size [m] = sqr (HR [ha] * 10000) / 7 = sqr (HR [m^2]) / 7_ 

**Table 4:** Body mass dependent scales for basic and super landscape grids.
![Table Scales Landscape] (file:_Table_scales.jpg)

The cell side length is rounded to an even number so that it is a multiple of the vegetation cell side lenth of the basic landscape grid. Assuming a 14 m × 14 m vegetation cell for the functional type omnivorous or insectivorous, body mass of 10 g 7 × 7 vegetation cells of the basic landscape grid were aggregated to a new one (Table 4). These “super-cells” store information about the vegetation types: total number of trees, total number of large shrubs, mean cover of shrubs and mean cover of grass. These rescaled landscape grids are assembled to fit the specific landscape grid, which equals 475 × 475 vegetation “super-cells” (in this case 3325 × 3325 basic vegetation cells, resulting in 7 × 7 rescaled basic landscape grids).

For all simulation experiments of each functional type, I generated minimum 5 replicate landscapes according to the scenario conditions (Table 5). I systematically investigated 9 landscape scenarios, which varied in the proportion of shrubs and grasses (grass-dominated, shrub-dominated and an intermediate state) as well as in the clumping intensity of shrubs (lowly, medially and highly clumped). For each functional type and landscape 100 simulation runs were conducted.
The population size depends on the initial value which determines the number of females who search for a suitable and available home range. Each individual occupies the suitable home range with the highest overall HQ. For this purpose it gets the information about the HQ of all vegetation cells and all suitable, unoccupied nest site cells. A nest site cell is defined by reaching the given thresholds in vegetation structure (see Table 2); a nest site cell is suitable if all vegetation cells in radius of 9 cells are unoccupied. It chooses the nest site cell with the highest overall HQ and occupies the whole home range. (Calculation of HQ and the thresholds for suitable nest sites are explained in section 3.3; compare also Table 1.) If all home ranges are occupied the bird can become a floater if the floater limit is not reached. If the floater limit is reached too, the remaining birds leave the grid. All individuals were assigned a random age between one year and their body mass depending maximal age (calculation see longevity in section “3.3 Submodels – Survival”). 

**Table 4:** Description of 9 shrub pattern scenarios, differing in degree of encroachment and intensity of clumping.
![Table Scenarios] (file:_Table_scenarios.jpg)

Herbivorous and carnivorous species are assumed to prefer open grasslands while omnivorous and insectivorous species are assumed to be associated with bush thickets. This assumption is made because herbivorous and carnivorous species need open habitats to search for leaves, seeds, buds and fruits (DONALDSON & KELK 1970, SEYMOUR & DEAN 2010) and to hunt and catch their prey respectively. Insectivorous birds need increased habitat heterogeneity since arthropod abundances increase until shrubs become the only dominant vegetation form (BLAUM et al. 2009; SEYMOUR & DEAN 2010). Omnivorous birds are assumed to change their diet seasonally for whatever food sources are most readily available, such as eating insects in spring and summer.

### Input data

If data on precipitation and/or population sizes are available, these data can be used.

### Submodels

    1. Rainfall

If there is no specific precipitation input, the current rainfall is drawn at the beginning of each year from a normal distribution with the mean precipitation of 300 mm and a standard deviation of 150 mm that was measured at the study area in Erichsfelde, Namibia. Rainfall is the first process which is applied, and shapes current habitat quality. 

    2. Calculate habitat quality (HQ)

HQ of a cell depends (i) on the vegetation composition of the three vegetation types, (ii) on the preferred habitat as functional trait and (iii) on the current rainfall, which influences the current biomass production. HQ includes implicitly both, the availability of food resources and predator shelter. Herbivorous and carnivorous species are assumed to be associated with open grasslands while omnivorous and insectivorous species are assumed to be adapted to bush thickets. HQ ranges from 0 to 1 meaning that values higher than 1 are put on that level. 

(Eq. 4) for species related to open grasslands:  _HQ = -0.1+0.02 × grass cover_ 	
(Eq. 5) for species related to bush thickets: _HQ = -0.458+0.1082 × shrub cover - 0.0024 × shrub cover^2_ 

We assume that HQ in a given year is to 50 % determined by habitat structure and 50 % determined by proportional changes due to precipitation. Biomass production is increasing linearly with precipation (ROSENZWEIG 1968, LE HOUÉROU et al. 1988, SALA et al. 1988, PARUELO et al. 1999, KNAPP & SMITH 2001). Deviations from mean annual precipitation (MAP) will lead to changed biomass abundance and thus influence HQ, while mean precipitation will not change current HQ. The change due to biomass is integrated as Δrain ∙ HQ (Δrain = rain/meanrain). We assume that the influence of the biomass change on functional types related to open grasslands is greater than on functional types related to bush thickets because rain has a direct effect on the proportion of grass in the current year as the proportion of shrubs remains more or less stable. Again, values higher than 1 are reset to 1.

(Eq. 6) for species related to open grasslands:  _HQ(current) = (HQ + 2 × ∆rain x HQ) / 3_ 
(Eq. 7) for species related to bush thickets: _HQ(current) = (HQ + 2 × ∆rain x HQ) / 2_

![ Habitat Quality AFT] (file:_Habitat_Quality.jpg)
**Fig. 2:** Relationship between vegetation cover of the preferred habitat and habitat quality (HQ). Green represents grass cover which is related to herbivorous and carnivorous species (see Eq. 4), brown represents shrubs which are related to insectivorous and omnivorous species (see Eq. 5).

    3. Aging

At the end of a year (early spring) the age of all individuals is incremented by one year.

    4. Survival

When an individual exceeds its expected maximum age (longevity), it dies. Longevity is increasing with increasing body mass and thus following allometric scaling:

(Eq. 8) _longevity = 4.75 × BM[g]^0.17_ (PRINZINGER 1993).

The basic mortality rate is calculated from longevity, assuming that after longevity is reached only 1% of the population survives: 

(Eq. 9) _mortality rate = 1 - 0.01^(1 ⁄ longevity)_.

Mortality is also influenced by the habitat. Therefore, a current mortality rate is calculated based on current HQ:

(Eq. 10) _mortality rate (current) = mortality rate × (1 - overallHQ) + 0.5_. 

For juveniles (age < 1 year) the current mortality is increased by 0.1. If a parent dies its juveniles become floater.

    5. Dispersal

Surviving offspring disperses in year of maturity as well as floaters do. First, the bird looks at its current location if a suitable home range is unoccupied. If not it does 5 explorer flights.. SUTHERLAND et al. (2000) showed that body mass can predict median dispersal distance (MDD):

(Eq. 11) _MDD [km] = 13.1 × BM[kg]^0.63_,
(Eq. 12) _MDD [cells] = (MDD × 100) / cell side length_.

They also showed that the distribution of distances moved is left skewed (SUTHERLAND et al. 2000, Table 1 and 2 and Figure 1). This indicates that many birds cluster near natal areas whereas some disperse relatively far in comparison. Using the left skewed distribution scaled by MDD, the step size for each floater is determined as factor weighting MDD individually:

(Eq. 13) _individual dispersal distance [cells] = step size × MDD [cells]_.

Explorer flights are assumed to be equally likely in all directions: subsequently, for each dispersing individual a direction (0°–359°) is chosen randomly After the random target cell is reached, the individual searches for suitable home ranges in its search radius (equal to home range area) and gets information about their current HQ. The individual chooses the unoccupied home range with the highest current HQ of all home ranges of all explorer flights, settles down and becomes a breeder. If there are home ranges with equal conditions the individual chooses randomly one of them. If there is no unoccupied home range the individual can stay as a floater in one of the occupied home ranges if the floater limit for these home ranges is not reached already.  If there is no home range to become a floater the individual emigrates and is put on the list of emigrants.

    6. Reproduction

Reproduction is possible if the individual is mature:

(Eq. 14) _age of maturity [a] = 0.11 × BM[g]^0.4796_.

Depending on the nest site preference as functional trait a home range can be used as a nest site if (i) it contains minimum two trees or five large shrubs respectively for tree- or shrub-breeding birds (to include indirectly the chance that nest sites are occupied by other functional types) or (ii) it does not exceed 40 % of shrub cover for ground-breeding since ground-breeders are adversely affected by bush encroachment and woody cover in general and need to breed in or against grass patches (SEYMOUR & DEAN 2010). If the current HQ of its home range is higher than 0, resident females can reproduce. The reproductive success is increasing with increasing body mass (allometric scaling):

(Eq. 12) _reproductive rate = 4.62 × BM[g]^(-0.138)_.

The allometric scaling is estimated from traits of African birds (NEWMAN 2002, ROBERTS 2005). Based on the reproductive rate and the current HQ of its home range, the reproductive output is calculated to include effects of resource availability:

(Eq. 13) _reproductive output = 0.5 x reproductive rate x (overallHQ + 0.5)_.

The sex ratio is assumed to be even, so with a probability of 50 % the offspring is female. Because we assume that all females will find a mate and females are the limiting sex regarding reproduction and dispersal, males are subsequently ignored. We assume that the reproductive output in a given year is to 50 % determined by habitat structure and thus by resource availability.

Floaters cannot reproduce.

    7. Updates

After performing these processes once all state variables and the sets of residents, juveniles, floaters, and juveniles are synchronous updated.

## References

BLACKBURN, M. & K.J. GASTON 1994: The Distribution of Body Sizes of the World's Bird Species. _Oikos_ **70**:127–130. DOI: 10.2307/3545707

BLAUM N., C. SEYMOUR, E. ROSSMANITH, M. SCHWAGER & F. JELTSCH 2009: Changes in arthropod diversity along a land use driven gradient of shrub cover in savanna rangelands: identification of suitable indicators. _Biodiversity and Conservation_ **18**:1187–1199. DOI: 10.1007/s10531-008-9498-x

BLAUM N., E. MOSNER, M. SCHWAGER & F. JELTSCH 2011: How functional is functional? Ecological groupings in terrestrial animal ecology towards an animal functional type. Biodiversity and Conservation 20:2333–2345. DOI: 10.1007/s10531-011-9995-1

BOWMAN J. 2003: Is dispersal distance of birds proportional to territory size? _Canadian Journal of Zoology_ **81**:195–102.DOI: 10.1139/Z02-237

DONALD P.F. 2007: Adult sex ratios in wild bird populations. _Ibis_ **149**:671–692.

DONALDSON C.H. & D.M. KELK 1970: An investigation of the veld problems of the Molopo area: I. Early findings. _Proceedings of the Annual Congresses of the Grassland Society of Southern Africa_ **5**:50–57. DOI: 10.1080/00725560.1970.9648610

GITAY H. & I.R. NOBLE 1997: What are functional types and how should we seek them? In: T.M. SMITH, H.H. SHUGART & F.I. WOODWARD (_Eds._) Plant functional types: their relevance to ecosystem properties and global change. _University Press, Cambridge, United Kingdoms._

GRIMM V., U. BERGER, F. BASTIANSEN, S. ELIASSEN, V. GINOT, J. GISKE, J. GOSS-CUSTARD, T. GRAND, S.K. HEINZ, G. HUSE, A. HUTH, J.U. JEPSEN, C. JØRGENSEN, W.M. MOOIJ, B. MÜLLER, G. PE’ER, C. PIOU, S.F. RAILSBACK, A.M. ROBBINS, M.M. ROBBINS, E. ROSSMANITH, N. RÜGER, E. STRAND, S. SOUISSI, R.A. STILLMAN, R. VABØ, U. VISSER, D.L. DEANGELIS 2006: A standard protocol for describing individual-based and agent-based models. _Ecological Modelling_ **198**:115–126. DOI: 10.1016/j.ecolmodel.2006.04.023

GRIMM V., U. BERGER, D.L. DEANGELIS, J.G. POLHILL, J. GISKE & S.F. RAILSBACK 2010: The ODD protocol: A review and first update. _Ecological Modelling_ **221**:2760–2768. DOI: 10.1016/j.ecolmodel.2010.08.019

HOCKEY P.A.R, W.R.J. DEAN & P.G. RYAN 2005: Roberts Birds of Southern Africa, 7. Edition. _New Holland Publishers, London, United Kingsdoms_.

KNAPP A.K. & M.D. SMITH 2001: Variation among biomes in temporal dynamics of aboveground primary production. _Science_ **291**:481–484. DOI: 10.1126/science.291.5503.481 

LE HOUÉROU H.N., R.L. BINGHAM & W. SKERBEK. 1988: Relationship between the variability of primary production and the variability of annual precipitation in world arid lands. _Journal of Arid Environments_ **15**:1–18.

MOUSTAKAS, A., K. WIEGAND, S. GETZIN, D. WARD, K.M. MEYER, M. GUENTHER & K.-H. MUELLER 2008: Spacing patterns of an Acacia tree in the Kalahari over a 61-year period: How clumped becomes regular and vice versa. _Acta Oecologica_ **33**:355–364. DOI: 10.1016/j.actao.2008.01.008

NEWMAN K. 2002: Newman's Birds of Southern Africa, 8. Edition. _Struik, Cape Town, South Africa_.

NEWTON I. 1988: Population regulation in peregrines: an overview. In: T.J. Cade, J.H. Enderson, C.G. Thelander & C.M. White (Eds): Peregrine Falcon populations: their management and recovery: 761–770. Peregrine Fund, Boise, Idaho.

NEWTON I. 1998: Population limitation in birds. _Academic Press, London, United Kingdoms_.

OTTAVIANI D., S.C. CAIRNS, M. OLIVERIO & L. BOITANI 2006: Body mass as a predictive variable of home-range size among Italian mammals and birds. _Journal of Zoology_ **206**:317–330. DOI: 10.1111/j.1469-7998.2006.00060.x

PARUELO J.M., W.K. LAUENROTH, I.C. BURKE & O. E. SALA 1999: Grassland precipitation-use efﬁciency varies across a resource gradient. _Ecosystems_ **2**:64–68. DOI: 10.1007/s100219900058

PENTERIANI V., M. FERRER & M.M. DELGADO 2011: Floater strategies and dynamics in birds, and their importance in conservation biology: towards an understanding of nonbreeders in avian populations. _Animal Conservation_ **14**:233–241. DOI: 10.1111/j.1469-1795.2010.00433.x

PRINZINGER R. 1993: Life span in birds and the ageing theory of absolute metabolic scope. _Comparative Biochemistry and Physiology Part A: Physiology_ **105**:609–615. DOI: 10.1016/0300-9629(93)90260-B

ROSENZWEIG M.L. 1968: Net primary productivity of terrestrial communities: prediction from climatological data. American Naturalist 102:67–74. DOI: 10.1086/282523

SALA O.E., W.J. PARTON, L.A. JOYCE, & W.K. LAUENROTH 1988: Primary production of the central grassland region of the United States. _Ecology_ **69**:40–45. DOI: 10.2307/1943158

SMITH S.M. 1978: The "Underworld" in a Territorial Sparrow: Adaptive Strategy for Floaters. _American Naturalist_ **112**:830–845. DOI: 10.1086/283298

TURCHIN P. 1995: Population regulation: old argument and a new synthesis. In: Population dynamics: new approaches and synthesis: 19–40. P.W. Price (Ed.). _Academic Press, San Diego, United States_.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.5
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Sens_Sc11_basic" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>outputRain</metric>
    <metric>count turtles</metric>
    <metric>count turtles with [Status = "Breeder"]</metric>
    <metric>count turtles with [Status = "Floater"]</metric>
    <metric>count turtles with [Status = "Juvenile" or Status = "JuvFloater"]</metric>
    <enumeratedValueSet variable="Nestsite">
      <value value="&quot;on trees&quot;"/>
      <value value="&quot;on large shrubs&quot;"/>
      <value value="&quot;ground-breeding&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Import">
      <value value="&quot;FileName&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;BothMedium_Sc11_1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;Home ranges&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Labels">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mass">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Habitatpreference">
      <value value="&quot;bush thicket&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Start">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Search-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NoFlights">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Maximum-Floater">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Floater-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InfluenceHQ">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Trees">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-LargeShrubs">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Shrubs">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-overallHQ">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainMean">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainStd">
      <value value="150"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sens_Sc11_SearchRadius+2" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>outputRain</metric>
    <metric>count turtles</metric>
    <metric>count turtles with [Status = "Breeder"]</metric>
    <metric>count turtles with [Status = "Floater"]</metric>
    <metric>count turtles with [Status = "Juvenile" or Status = "JuvFloater"]</metric>
    <enumeratedValueSet variable="Nestsite">
      <value value="&quot;on trees&quot;"/>
      <value value="&quot;on large shrubs&quot;"/>
      <value value="&quot;ground-breeding&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Search-Radius">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Import">
      <value value="&quot;FileName&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;BothMedium_Sc11_1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;Home ranges&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Labels">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mass">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Habitatpreference">
      <value value="&quot;bush thicket&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Start">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NoFlights">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Maximum-Floater">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Floater-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InfluenceHQ">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Trees">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-LargeShrubs">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Shrubs">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-overallHQ">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainMean">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainStd">
      <value value="150"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sens_Sc11_SearchRadius-2" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>outputRain</metric>
    <metric>count turtles</metric>
    <metric>count turtles with [Status = "Breeder"]</metric>
    <metric>count turtles with [Status = "Floater"]</metric>
    <metric>count turtles with [Status = "Juvenile" or Status = "JuvFloater"]</metric>
    <enumeratedValueSet variable="Nestsite">
      <value value="&quot;on trees&quot;"/>
      <value value="&quot;on large shrubs&quot;"/>
      <value value="&quot;ground-breeding&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Search-Radius">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Import">
      <value value="&quot;FileName&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;BothMedium_Sc11_1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;Home ranges&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Labels">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mass">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Habitatpreference">
      <value value="&quot;bush thicket&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Start">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NoFlights">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Maximum-Floater">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Floater-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InfluenceHQ">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Trees">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-LargeShrubs">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Shrubs">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-overallHQ">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainMean">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainStd">
      <value value="150"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sens_Sc11_NoFlights+2" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>outputRain</metric>
    <metric>count turtles</metric>
    <metric>count turtles with [Status = "Breeder"]</metric>
    <metric>count turtles with [Status = "Floater"]</metric>
    <metric>count turtles with [Status = "Juvenile" or Status = "JuvFloater"]</metric>
    <enumeratedValueSet variable="Nestsite">
      <value value="&quot;on trees&quot;"/>
      <value value="&quot;on large shrubs&quot;"/>
      <value value="&quot;ground-breeding&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NoFlights">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Import">
      <value value="&quot;FileName&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;BothMedium_Sc11_1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;Home ranges&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Labels">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mass">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Habitatpreference">
      <value value="&quot;bush thicket&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Start">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Search-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Maximum-Floater">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Floater-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InfluenceHQ">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Trees">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-LargeShrubs">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Shrubs">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-overallHQ">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainMean">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainStd">
      <value value="150"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sens_Sc11_NoFlights-2" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>outputRain</metric>
    <metric>count turtles</metric>
    <metric>count turtles with [Status = "Breeder"]</metric>
    <metric>count turtles with [Status = "Floater"]</metric>
    <metric>count turtles with [Status = "Juvenile" or Status = "JuvFloater"]</metric>
    <enumeratedValueSet variable="Nestsite">
      <value value="&quot;on trees&quot;"/>
      <value value="&quot;on large shrubs&quot;"/>
      <value value="&quot;ground-breeding&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NoFlights">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Import">
      <value value="&quot;FileName&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;BothMedium_Sc11_1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;Home ranges&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Labels">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mass">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Habitatpreference">
      <value value="&quot;bush thicket&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Start">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Search-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Maximum-Floater">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Floater-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InfluenceHQ">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Trees">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-LargeShrubs">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Shrubs">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-overallHQ">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainMean">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainStd">
      <value value="150"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sens_Sc11_MaxFloater+1" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>outputRain</metric>
    <metric>count turtles</metric>
    <metric>count turtles with [Status = "Breeder"]</metric>
    <metric>count turtles with [Status = "Floater"]</metric>
    <metric>count turtles with [Status = "Juvenile" or Status = "JuvFloater"]</metric>
    <enumeratedValueSet variable="Nestsite">
      <value value="&quot;on trees&quot;"/>
      <value value="&quot;on large shrubs&quot;"/>
      <value value="&quot;ground-breeding&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Maximum-Floater">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Import">
      <value value="&quot;FileName&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;BothMedium_Sc11_1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;Home ranges&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Labels">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mass">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Habitatpreference">
      <value value="&quot;bush thicket&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Start">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Search-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NoFlights">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Floater-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InfluenceHQ">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Trees">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-LargeShrubs">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Shrubs">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-overallHQ">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainMean">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainStd">
      <value value="150"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sens_Sc11_MaxFloater-1" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>outputRain</metric>
    <metric>count turtles</metric>
    <metric>count turtles with [Status = "Breeder"]</metric>
    <metric>count turtles with [Status = "Floater"]</metric>
    <metric>count turtles with [Status = "Juvenile" or Status = "JuvFloater"]</metric>
    <enumeratedValueSet variable="Nestsite">
      <value value="&quot;on trees&quot;"/>
      <value value="&quot;on large shrubs&quot;"/>
      <value value="&quot;ground-breeding&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Maximum-Floater">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Import">
      <value value="&quot;FileName&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;BothMedium_Sc11_1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;Home ranges&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Labels">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mass">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Habitatpreference">
      <value value="&quot;bush thicket&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Start">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Search-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NoFlights">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Floater-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InfluenceHQ">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Trees">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-LargeShrubs">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Shrubs">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-overallHQ">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainMean">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainStd">
      <value value="150"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sens_Sc11_ThrNestGround" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>outputRain</metric>
    <metric>count turtles</metric>
    <metric>count turtles with [Status = "Breeder"]</metric>
    <metric>count turtles with [Status = "Floater"]</metric>
    <metric>count turtles with [Status = "Juvenile" or Status = "JuvFloater"]</metric>
    <enumeratedValueSet variable="Nestsite">
      <value value="&quot;ground-breeding&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Shrubs">
      <value value="35"/>
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Import">
      <value value="&quot;FileName&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;BothMedium_Sc11_1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;Home ranges&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Labels">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mass">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Habitatpreference">
      <value value="&quot;bush thicket&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Start">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Search-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NoFlights">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Maximum-Floater">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Floater-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InfluenceHQ">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Trees">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-LargeShrubs">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-overallHQ">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainMean">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainStd">
      <value value="150"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sens_Sc11_ThrNestShrub" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>outputRain</metric>
    <metric>count turtles</metric>
    <metric>count turtles with [Status = "Breeder"]</metric>
    <metric>count turtles with [Status = "Floater"]</metric>
    <metric>count turtles with [Status = "Juvenile" or Status = "JuvFloater"]</metric>
    <enumeratedValueSet variable="Nestsite">
      <value value="&quot;on large shrubs&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-LargeShrubs">
      <value value="3"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Import">
      <value value="&quot;FileName&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;BothMedium_Sc11_1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;Home ranges&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Labels">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mass">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Habitatpreference">
      <value value="&quot;bush thicket&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Start">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Search-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NoFlights">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Maximum-Floater">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Floater-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InfluenceHQ">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Trees">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Shrubs">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-overallHQ">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainMean">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainStd">
      <value value="150"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sens_Sc11_ThrNestTree" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>outputRain</metric>
    <metric>count turtles</metric>
    <metric>count turtles with [Status = "Breeder"]</metric>
    <metric>count turtles with [Status = "Floater"]</metric>
    <metric>count turtles with [Status = "Juvenile" or Status = "JuvFloater"]</metric>
    <enumeratedValueSet variable="Nestsite">
      <value value="&quot;on trees&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Trees">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Import">
      <value value="&quot;FileName&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;BothMedium_Sc11_1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;Home ranges&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Labels">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mass">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Habitatpreference">
      <value value="&quot;bush thicket&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Start">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Search-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NoFlights">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Maximum-Floater">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Floater-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InfluenceHQ">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-LargeShrubs">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Shrubs">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-overallHQ">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainMean">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainStd">
      <value value="150"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sens_Sc11_InfluenceHQ+16" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>outputRain</metric>
    <metric>count turtles</metric>
    <metric>count turtles with [Status = "Breeder"]</metric>
    <metric>count turtles with [Status = "Floater"]</metric>
    <metric>count turtles with [Status = "Juvenile" or Status = "JuvFloater"]</metric>
    <enumeratedValueSet variable="Nestsite">
      <value value="&quot;on trees&quot;"/>
      <value value="&quot;on large shrubs&quot;"/>
      <value value="&quot;ground-breeding&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InfluenceHQ">
      <value value="0.66666666"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Import">
      <value value="&quot;FileName&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;BothMedium_Sc11_1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;Home ranges&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Labels">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mass">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Habitatpreference">
      <value value="&quot;bush thicket&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Start">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Search-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NoFlights">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Maximum-Floater">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Floater-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Trees">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-LargeShrubs">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Shrubs">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-overallHQ">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainMean">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainStd">
      <value value="150"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sens_Sc11_InfluenceHQ-16" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>outputRain</metric>
    <metric>count turtles</metric>
    <metric>count turtles with [Status = "Breeder"]</metric>
    <metric>count turtles with [Status = "Floater"]</metric>
    <metric>count turtles with [Status = "Juvenile" or Status = "JuvFloater"]</metric>
    <enumeratedValueSet variable="Nestsite">
      <value value="&quot;on trees&quot;"/>
      <value value="&quot;on large shrubs&quot;"/>
      <value value="&quot;ground-breeding&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InfluenceHQ">
      <value value="0.33333334"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Import">
      <value value="&quot;FileName&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;BothMedium_Sc11_1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;Home ranges&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Labels">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mass">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Habitatpreference">
      <value value="&quot;bush thicket&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Start">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Search-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NoFlights">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Maximum-Floater">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Floater-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Trees">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-LargeShrubs">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Shrubs">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-overallHQ">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainMean">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainStd">
      <value value="150"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sens_Sc11_FloaterHR+6" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>outputRain</metric>
    <metric>count turtles</metric>
    <metric>count turtles with [Status = "Breeder"]</metric>
    <metric>count turtles with [Status = "Floater"]</metric>
    <metric>count turtles with [Status = "Juvenile" or Status = "JuvFloater"]</metric>
    <enumeratedValueSet variable="Nestsite">
      <value value="&quot;on trees&quot;"/>
      <value value="&quot;on large shrubs&quot;"/>
      <value value="&quot;ground-breeding&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Floater-Radius">
      <value value="10.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Import">
      <value value="&quot;FileName&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;BothMedium_Sc11_1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;Home ranges&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Labels">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mass">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Habitatpreference">
      <value value="&quot;bush thicket&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Start">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Search-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NoFlights">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Maximum-Floater">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InfluenceHQ">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Trees">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-LargeShrubs">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Shrubs">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-overallHQ">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainMean">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainStd">
      <value value="150"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sens_Sc11_FloaterHR-2" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>outputRain</metric>
    <metric>count turtles</metric>
    <metric>count turtles with [Status = "Breeder"]</metric>
    <metric>count turtles with [Status = "Floater"]</metric>
    <metric>count turtles with [Status = "Juvenile" or Status = "JuvFloater"]</metric>
    <enumeratedValueSet variable="Nestsite">
      <value value="&quot;on trees&quot;"/>
      <value value="&quot;on large shrubs&quot;"/>
      <value value="&quot;ground-breeding&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Floater-Radius">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Import">
      <value value="&quot;FileName&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;BothMedium_Sc11_1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;Home ranges&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Labels">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mass">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Habitatpreference">
      <value value="&quot;bush thicket&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Start">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Search-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NoFlights">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Maximum-Floater">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InfluenceHQ">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Trees">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-LargeShrubs">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Shrubs">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-overallHQ">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainMean">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainStd">
      <value value="150"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sens_Sc11_ThrHQ+1" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>outputRain</metric>
    <metric>count turtles</metric>
    <metric>count turtles with [Status = "Breeder"]</metric>
    <metric>count turtles with [Status = "Floater"]</metric>
    <metric>count turtles with [Status = "Juvenile" or Status = "JuvFloater"]</metric>
    <enumeratedValueSet variable="Nestsite">
      <value value="&quot;on trees&quot;"/>
      <value value="&quot;on large shrubs&quot;"/>
      <value value="&quot;ground-breeding&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-overallHQ">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Import">
      <value value="&quot;FileName&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;BothMedium_Sc11_1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;Home ranges&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Labels">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mass">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Habitatpreference">
      <value value="&quot;bush thicket&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Start">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Search-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NoFlights">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Maximum-Floater">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Floater-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InfluenceHQ">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Trees">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-LargeShrubs">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Shrubs">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainMean">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainStd">
      <value value="150"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sens_Sc11_test" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10"/>
    <metric>outputRain</metric>
    <metric>count turtles</metric>
    <metric>count turtles with [Status = "Breeder"]</metric>
    <metric>count turtles with [Status = "Floater"]</metric>
    <metric>count turtles with [Status = "Juvenile" or Status = "JuvFloater"]</metric>
    <enumeratedValueSet variable="Nestsite">
      <value value="&quot;on trees&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Import">
      <value value="&quot;FileName&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;BothMedium_Sc11_1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;Home ranges&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Labels">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mass">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Habitatpreference">
      <value value="&quot;bush thicket&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Start">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Search-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NoFlights">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Maximum-Floater">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Floater-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InfluenceHQ">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Trees">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-LargeShrubs">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-Shrubs">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-overallHQ">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainMean">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RainStd">
      <value value="150"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
