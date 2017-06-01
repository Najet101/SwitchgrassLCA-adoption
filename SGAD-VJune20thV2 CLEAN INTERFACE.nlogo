;;;;; SWITCHGRASS ADOPTION MODEL;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATE       ;; Ver. ;; By  ;; Comments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 28-10- 2013;; 001  ;; NB  ;; 



globals        [  
                Price_of_base
                Price_of_SG
                SGPriceMem
                
                MaxSGPrice?
                MeanSGPrice?
                MinSGPrice?
                
                FieldToStorageCost
                StorageToRefineryCost
                FieldToRefineryCost
                
                new_sgprice
                new_basecost
                average_age
                average_education
                
                
                
                SG_root
                Area_planted_global
                PropWithSuccessor
                
                PropComputerUse
                ;  Distance_to_refinery ;; Average distance between farm and fuel refinery ;; 
                
                Co2_Global
                co2_pertonne_km      ; Amount of CO2 equivalents emitted for transporting one tonne of switchgrass one mile
                co2_growth            ; Used to keep a running total of the amount of CO2 equivalents emitted (negative = sequestered) in the entire study area from switchgrass growth
              
                co2_delivery          ; Used to keep a running total of the amount of CO2 equivalents emitted in the entire study area from switchgrass delivery from farmers to plants
                co2_electric_gen       ; Used to keep a running total of the amount of CO2 equivalents emitted in the entire study area from switchgrass-based electricity generation
                co2_fuel_gen           ; Used to keep a running total of the amount of CO2 equivalents emitted in the entire study area from switchgrass-based fuel production/manufacturing
                co2_fuel_burn          ; Used to keep a running total of the amount of CO2 equivalents emitted in the entire study area from switchgrass-based fuel combustion
                co2_per_liter_burn

                co2_gasoline_TFC   ;per liter
                co2_corn_etoh_TFC  ;per liter
                co2_coal_TFC       ;per MWh
                co2_natgas_TFC     ;per MWh
               
                Lgas_per_Letoh
                fplantegen       ;MWh electricity produced by fuel plants per liter of EtOH produced
               
                market_fuel_supply ;liters of switchgrass EtOH delivered to the market in this itteration
                market_MWh_supply  ;MWH of switchgrass electricity delivered to the market in this itteration
                       
                net_co2_per_liter   ;CO2 emissions from conversion of biomass to fuel in tonnes per liter
                Net_co2_per_tonne
                year_fuel_produced ;sum of total yearly production in liters
                year_fuel_transfo_co2    ;sum of total yearly emissions in tonnes
                
                liters_per_tonne   ;conversion rate in liters of EtOH per tonne of switchgrass
                
                heat_rate         ;thousand Btu per MWh
                MWhper_tonne_coal  ;MWh per tonne coal
                MWh_per_tonne_sg    ;MWh per tonne sg
                tonnes_per_MWh     ;tonnes of sg per MWh
                net_co2_per_MWh     ;CO2 emissions associated with combusting biomass in $ per MWh
                year_MWh_produced  ;Total annual generation in MWh
                year_eplant_co2    ;Total annual CO2 emissions in tonnes
                MWh_per_tonne_coal
                heat_content_coal
                heat_content_sg
                
                TotHarvest              ; Used as a placeholder to quantify the amount of switchgrass harvested
                trade
               
                ] 

                    ;; Main agents (turtle)
breed          [ 
                 Be_Farmers Be_Farmer
                 ]
breed          [ No_Be_Farmers 
                 No_Be_Farmer
                 ]


patches-own    [
                 SG_yield                    ;; yield of SG
                 Base_yield                  ;; yield of whatever they would be planting instead of SG
                 Owned-by                    ;; which farmer owns the field (one patch = one field)
                 Area_to_plant?              ;; for the followraing years;;boolean TRUE or FALSE;; will evolve from year to year  
                 farm_ID]

turtles-own    [
              ;; Economic attributes
              
               ;  farm_color
                 Fixed_cost_peracre          ;; Current FC per acre
                 Total_fixed_cost            ;; Current TFC    
                 Variable_cost_peracre       ;; Current VC per acre
                 Total_variable_cost         ;; Current TVC       
                 Total_cost_peracre          ;; FC per acre + VC per acre
                 Total_cost                  ;; TFC + TVC
                 Sales_price_SG_peracre      ;; slider determined by the user in the GUI
                 Sales_price_Base_peracre
                             
                 Current_revenues            ;; Current revenues based pre-SG based on base_yield
                 Current_profit              ;; Idem; based on current revenues
                 Potential_revenues          ;; of planting SG ;; based on SG price
                 Potential_profit            ;; of planting SG;; based on potential revenues
                 Breakeven?                  ;; TRUE or FALSE;; (Potential prof - Current prof) if TRUE go to next step: generate Interest Score (IS)  
                 
                 SG_harvested               
                 SG_revenues
                 SG_profit
                 
              ;; Personal attributes  
                 age_of_farmer 
                 ageofdeath
                 education
                 computer_use?
                 age_mem
                 successor?
                 
                 risk_aversion
                 farm_successor?
                 Familiarity_with_SG         ;; Index Randomly distributed (Mean 50; SD 30)    
                 
                 highra?
                 medra?
                 lowra?
                 
                 
                 
                 highfa?
                 medfa?     
                 lowfa?
                 
                 highis?
                 medis?
                 lowis?
                 
                 highEd?
                 medEd?
                 lowEd?
                 
                 highPotProf?
                 medPotProf?
                 lowPotProf?
                 
                 highPriceSG?
                 medPriceSG?
                 lowPriceSG?
                 
                 highCost?
                 medCost?
                 lowCost?
           
                 
                 On_the_fence?
                 mindchanged?
                 Interest_score              ;; Score = Familiarity * level_of_financial_risk ;; for the first test year
                 Happy?                     ;; happy if they made a profit selling SG (revenues > 0);; On setup, happy? is FALSE for everybody       
                 
                 Rep?
              ;; Agricultural attributes
                 
                 Number_of_fields_owned      ;; Number of paches owned (just the number)           
                 Farm_root                   ;; Center patch where my farm is located, used to color my land (contiguous patches) with the same shade
                 
           
                 My_farm                     ;; group of paches owned (agentset)
                 Parcels_col                 ;; color of my parcel, used to color the rest of my land with the same color of farm_root
                 Acres_owned                 ;; number_of_field_owned * 10 acres
                 number_of_field_planted
                 
                
                 
                 Plant_SG?                   ;; TRUE or FALSE
                 Initial_Percent_of_planting ;; Random percentage up to 40% of their land
                 My_percent_increase         ;; pseudo random percentage of increase; "proportional to profit made"
                 Percent_of_planting         ;; Increased Initial percent of planting
                 SG_planted_ton
                 SG_harvArea
                 
                 Portion_of_planting         ;; Number of field that will be devoted to SG (just the number)
                 
                 My_SGplant   
                  my_SGparcel
                              ;; Number of paches to be planted (agentset)
                 My_area_planted             ;; Surface SG plantes in acres
                 harv?                       ;; TRUE of FALSE
                 CollRev?                    ;; TRUE of FALSE
                            
                 Memory_planted              ;; Record the number of field planted (list of number)
                 Memory_lot_planted          ;; Record the number of patches planted (list agentset)
                ;;temp variables for troubleshotting; to be deleted when model done!;;
                 BE ;; list
                 Distance_to_refinery 
                 TransCost
                 usingStorage?
                 
                
                   
                Myco2_growth            ; Used to keep a running total of the amount of CO2 equivalents emitted (negative = sequestered) in the entire study area from switchgrass growth         
                Myco2_delivery          ; Used to keep a running total of the amount of CO2 equivalents emitted in the entire study area from switchgrass delivery from farmers to plants
                Myco2_electric_gen       ; Used to keep a running total of the amount of CO2 equivalents emitted in the entire study area from switchgrass-based electricity generation
                Myco2_fuel_gen           ; Used to keep a running total of the amount of CO2 equivalents emitted in the entire study area from switchgrass-based fuel production/manufacturing
                Myco2_fuel_burn          ; Used to keep a running total of the amount of CO2 equivalents emitted in the entire study area from switchgrass-based fuel combustion
               
                Myyear_fuel_transfo_co2
                Myyear_eplant_co2
                
                DistFieldToRefinery 
                DistFieldToStorage
                DistStorageToRefinery
                 ]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;SETUP;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SETUP;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SETUP;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  ;if ticks mod = 3 [go]
  __clear-all-and-reset-ticks
  
;set snapshot-directory user-directory
  setup-patches-and-create-turtles
  set-farmer-attributes
  
  set PropWithSuccessor random-normal 0.4 0.05
  set PropComputerUse random-normal 0.6 0.05
  
  set Price_of_base precision random-normal 350 50 0
  set Price_of_SG precision random-normal 350 100 0
  
  set SGPriceMem []
 
  set Area_planted_global sum [My_area_planted] of turtles
 
  set Co2_Global sum [Co2_total_from_SG] of turtles
 
  set co2_pertonne_km   0.00017623926144      ; (0.00010951 miles)= (0.0094 gal diesel/tonne mile) * (25,630 lbs TFC CO2 emissions/ 1000 gal) * (1 tonne / 2200 lbs)
  set co2_fuel_burn     0.001507        ;tonnes CO2 per liter ethanol, Ethanol = (789 g EtOH/L) / (46.07 g EtOH / mol EtOH) * (2 mol CO2 / mol EtOH) * (44 g CO2 / mol CO2) * (1 tonne / 1,000,000 g)
  set co2_gasoline_TFC  0.00297          ;tonnes CO2 per liter http://solar.gwu.edu/index_files/Resources_files/LCA_for_PHEVs.pdf
  set co2_corn_etoh_TFC 0.003            ;tonnes CO2 per liter Source
  set co2_coal_TFC      1.05             ;tonnes CO2 per MWh (Meta-analysis of TFC emissions) Benjamin K. Sovacool. Valuing the greenhouse gas emissions from nuclear power: A critical survey. Energy Policy, Vol. 36, 2008, p. 2950.
  set co2_natgas_TFC    0.443            ;tonnes CO2 per MWh (Meta-analysis of TFC emissions) Benjamin K. Sovacool. Valuing the greenhouse gas emissions from nuclear power: A critical survey. Energy Policy, Vol. 36, 2008, p. 2950.
;  set co2delivery (co2delivery + (trade * co2pertonnemile * disttoe)) ;using the distance to the electric plant, calculate the transportation CO2 emissions and add them to the running total for co2delivery
  set Lgas_per_Letoh    1.4678             ;Heat content conversion rate (One gallon of gasoline has 1.4678 x the energy content of one liter of EtOH)
  set fplantegen        0.00007407         ;GREET in MWh per L EtOH ; (0.26 kWH/gal EtOH) * (1 gal EtOH / 3.78 L EtOH) * (1 MWh / 1000 kWh)

  set heat_content_sg   20935             ;MJ/tonne
  set heat_content_coal 20000 ;;btu per lb
  set liters_per_tonne floor random-normal 320 20                   ;sets the yield of fuel (in liters) that can be produced per tonne of switchgrass input (normal distribution)
 
  set heat_rate random-normal 11.6 1  ;MJ/kWh (1.0551 * kbtu/kWh) -- Sets the heatrate of the plant   
 
  set MWh_per_tonne_coal (heat_content_coal * 2.326) / (1000 * heat_rate)   ;1 btu/lb = 2.326 MJ/tonne -- Calculates the MWh produced from one tonne of coal
  set MWh_per_tonne_sg (heat_content_sg / (1000 * heat_rate))                 ;Calculates the MWh produced from one tnne of switchgrass
  set tonnes_per_MWh (1 / MWH_per_tonne_sg) 
 
  set net_co2_per_MWh   0.946                   ;tonnes CO2e per MWH = (946 g/kWh) * (1000 kWh / 1 MWh) * (1 tonne / 1,000,000 g)
  set Net_co2_per_tonne -1.097                                  ;Source: GREET (tonnes of CO2 emitted [negative value = sequestered] per tonne of switchgrass during growth phase only). Note this value is slightly highraer than 1 due to extra carbon sequestration in the soil.
 
  set FieldToStorageCost 1.22 ; dollars per tonne ;;http://www.extension.iastate.edu/agdm/crops/pdf/a1-22.pdf
  set StorageToRefineryCost 0.2883 ;dollars per tonne;;;http://www.extension.iastate.edu/agdm/crops/pdf/a1-22.pdf
  
  set FieldToRefineryCost 0.14 ;dollars per tonne ;;;http://www.sciencedirect.com/science/article/pii/S0960852410018006
export-data
;export-dataShort
end


to setup-patches-and-create-turtles
  
   set-default-shape turtles "farm"
   create-turtles initial-number-of-farmers 
   [
   setxy random-pxcor random-pycor
    if any? other turtles-here [ move-to one-of patches with [ not any? turtles-here ] ]
    set color brown 
    set size 1
    set label who set label-color black
       
    set pcolor 43 + random 5
    set farm_root patch-here
    set parcels_col [pcolor] of farm_root  
   set average_age random-normal 55 5
   set ageofdeath random-normal 75 5
   set average_education 1  ;;post high-school
   set successor? FALSE
   
 ;   set Distance_to_refinery random-normal 60 40
    
   set DistFieldToRefinery random-normal 60 20
   set DistFieldToStorage random-normal 20 10
   set DistStorageToRefinery random-normal 50 20
   ; set mindchanged? FALSE
   ;set computer_use? FALSE
   
                                  
   ; set average_age 35 
  
    set age_mem []
    set memory_planted [] 
    set memory_lot_planted []
    
   ;set Plant_SG? FALSE
   ; set harv? FALSE                      
   ; set CollRev? FALSE
   ; set Rep? FALSE
   ]

  ask patches
    [
     set owned-by min-one-of turtles [distance myself]    
     set SG_yield 2.5 ;; ton per acre
     set base_yield random-normal 2 0.5
     set area_to_plant? FALSE
    ]                    
    ;; turtle owns patches it's nearest to

end   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;GO;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;GO;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;GO;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  
 farmer-decision-start-point
 ask turtles  [ifelse learn? = TRUE [learn_from_neighbors] []
   ]

;if Price_shock? [shock_price]
ask turtles [if commodity_shock? [commodity_shock]]

   update-attributes
   update_patches
   defineeffect
   update_effect
update_global
;CountPlantbySGprice
;ask turtles [if Transp_Scenarios [Transportation_Scenarios]]
;exportview
;ask patches [recolor]   

 ; export-files
  ;export-filesShort
;if ticks mod 10 = 0 [export-view (word "View-on-tick-" ticks ".png")]

;export-view (word "View-on-tick-" ticks ".eps")
; if record-movie?
;  [update-movie]
  tick
  if ticks = 50 [stop]

end           


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PROCEDURES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PROCEDURES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PROCEDURES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; setting farmers attributes

to set-farmer-attributes
  
  ask turtles
   [
     set age_of_farmer precision (random-normal average_age 10) 0
     ; set average_education 1  ;;post high-school
     set education random-normal average_education 3
     
  ; ask n-of (PropComputerUse * count turtles ) turtles [ set computer_use? TRUE ]  

  set risk_aversion (age_of_farmer + random-normal 0.9 0.3) / 100
        (if (age_of_farmer < average_age) [set risk_aversion risk_aversion - 0.1])
        
  ;  if (computer_use? = TRUE) [    
        set familiarity_with_SG (age_of_farmer + random-normal 4 2 + 2) / 10
        ;]
        (if (age_of_farmer < average_age) [set familiarity_with_SG familiarity_with_SG + 2])
        
   ask n-of (PropWithSuccessor * count turtles ) turtles [ set successor? TRUE ]   
    
 
    
    set number_of_fields_owned count patches with [ owned-by = myself ]
    set my_farm patches with [ owned-by = myself]
    ask my_farm [set pcolor [parcels_col] of myself]
    set acres_owned number_of_fields_owned * 10 ;; 6 acres per patch
    
    set variable_cost_peracre random-normal 250 100 ;;V
    set Total_variable_cost variable_cost_peracre * acres_owned 
    
    set fixed_cost_peracre  random-normal 100 50 ;;;F;; per acre
    set total_fixed_cost  fixed_cost_peracre * acres_owned
    
    set Total_cost_peracre precision (fixed_cost_peracre + variable_cost_peracre) 0
    
    set Total_cost precision (total_fixed_cost + Total_variable_cost) 0
    
      ;SG;;;;
    set Sales_price_SG_peracre  price_of_SG;;S;; formula BEunit= F/(S-V);; $ BEdollars=BEunit*S 
    set Potential_revenues SG_yield * acres_owned * price_of_SG
    set Potential_profit Potential_revenues - Total_cost
    
   ;;Base;;;
    set Sales_price_Base_peracre  price_of_base;;S;; formula BEunit= F/(S-V);; $ BEdollars=BEunit*S 
    set current_revenues base_yield * acres_owned * price_of_base  
    set Current_profit current_revenues - Total_cost
    
  ;;be;;  
    set BE Potential_profit - Current_profit
    
    set initial_Percent_of_planting random-float 0.4
    set my_percent_increase 1 + random-float .3  
    
  ; set Distance_to_refinery random-normal 60 40
    

]
  
  
end

to farmer-decision-start-point
ask turtles
 [ 
  ifelse Potential_profit >= Current_Profit
      [ 
        set Breed Be_farmers  set color 135; set size 1
        calculate_interest_score
        ]
      [
        set Breed No_be_farmers ;set size 2
      ]
   ]
  
end

to calculate_interest_score
  set interest_score (Familiarity_with_SG * risk_aversion) 
     ifelse interest_score > mean [Interest_score] of be_farmers 
           [
             set Plant_SG? TRUE 
            Plant_SG ;set size 3
             ]
            []
            
end

to Plant_SG

 ifelse Plant_SG? = TRUE 
    [ 
  set portion_of_planting precision (number_of_fields_owned * percent_of_planting) 0
  
     set My_SGplant n-of portion_of_planting my_farm ;; number of patches
     ask My_SGplant 
        [ 
       set area_to_plant? TRUE 
       recolor ]
     set number_of_field_planted count my_SGplant  with [ area_to_plant? = TRUE ];;
     set My_area_planted number_of_field_planted * 10 ;; surface planted
      set SG_planted_ton My_area_planted * SG_yield ;; ton planted
     ; recolor
     set harv? TRUE
     harvest_SG
    
     ]  
    []

end 



to harvest_SG
 
  set co2_growth 0                              ;clear previous year's growth from short-term memory variabl
  set co2_growth co2_growth + (SG_planted_ton  * Net_co2_per_tonne ) ;adds current farmers sequestration to the running total variable "co2growth" for all farmer
  
  set Myco2_growth SG_planted_ton * Net_co2_per_tonne  ;adds current farmers sequestration to the running total variable "co2growth" for all farmer

  set SG_harvArea My_area_planted * random-normal 0.8 0.1 ;; surface acres harvested with rate loss
  set SG_harvested SG_harvArea * SG_yield ;; ton harvested with rate of loss

  set collRev? TRUE
  set area_to_plant? FALSE
  
  set TotHarvest sum [SG_planted_ton] of be_farmers    ;calculates that year's total switchgrass harvest
    
  recolor
        Collect_revenues
       ; transport
        Transp_Scen

end


to transport 
  set co2_delivery 0
 ;  set co2delivery (co2delivery + (trade * co2pertonnemile * disttoe)) ;using the distance to the electric plant, calculate the transportation CO2 emissions and add them to the running total for co2delivery

  set co2_delivery ( co2_delivery + (trade * co2_pertonne_km * Distance_to_refinery)) ;using the distance to the fuel plant, calculate the transportation CO2 emissions and add them to the running total for co2delivery 
 set Myco2_delivery SG_harvested  * co2_pertonne_km * Distance_to_refinery
  calculate_SGEnerg_production_use  
end 


to Transp_Scen  
 set co2_delivery 0
 
 if Transp_Scenarios = "FieldtoRefinery"
 [
  set co2_delivery co2_delivery + (SG_harvested  * co2_pertonne_km * DistFieldToRefinery)   
  set TransCost FieldToRefineryCost * SG_harvested * DistFieldToRefinery
  
  set Myco2_delivery SG_harvested  * co2_pertonne_km * DistFieldToRefinery 
  
  set usingStorage? false
  ]
  
  if Transp_Scenarios = "FieldtoStorageandRefinery"
  [
   set co2_delivery co2_delivery + (SG_harvested  * co2_pertonne_km * (DistFieldToStorage + DistStorageToRefinery))
   set TransCost (FieldToStorageCost + StorageToRefineryCost) * SG_harvested * Distance_to_refinery
   
   set Myco2_delivery SG_harvested  * co2_pertonne_km * DistFieldToStorage + DistStorageToRefinery
   
   set usingStorage? true
   ]
 set Myco2_delivery SG_harvested  * co2_pertonne_km * (DistFieldToStorage + DistStorageToRefinery)
 calculate_SGEnerg_production_use 
end 


to Collect_revenues
  set SG_revenues SG_harvested * price_of_SG ;; in dollars
  set SG_profit precision (SG_revenues - (Total_cost_peracre * My_area_planted)) 0
  set Rep? TRUE
  ask be_farmers with [Rep? = TRUE] [replant?]
end

to replant?
  ifelse SG_profit > 0 
        [set happy? TRUE]
        [set happy? FALSE]
end

to learn_from_neighbors

  ask be_farmers with [Plant_SG? != true]
  
      [ 
        set On_the_fence? TRUE
        if count be_farmers in-radius influence_radius with [happy? = TRUE] > count be_farmers in-radius influence_radius with [happy? = FALSE]
        [
            Plant_SG; set size 2; print who print "I have changed to" print breed
            set mindchanged? TRUE
           
        ]
      ]
     
end

to recolor
  ifelse area_to_plant? = TRUE 
        [set pcolor 53 + random 5]
       [set pcolor 43 + random 6]
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;  script for producing fuel and electricity from the delivered switchgrass  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to calculate_SGEnerg_production_use                                                    ;
   
  set co2_fuel_gen 0                                              ;clear the previous year's running total of co2 from fuel generation from memeory
  set market_fuel_supply 0                                        ;clear the previous year's running total of fuel supplied to the market from memory
  set co2_electric_gen 0                                          ;clear the previous year's running totatl of co2 from electricity generation from memory
  set market_MWh_supply 0                                         ;clear the previous year's running total of electricity supplied to the market from memory
  
  set year_fuel_produced  (Total_Harvested_ton *  liters_per_tonne)       ;calculate the liters of fuel that will be produced this year based on the amount of switchgrass on hand
  set market_fuel_supply (market_fuel_supply + year_fuel_produced)  ;add that plant's annual output to the running total for the entire fuel market
  set year_fuel_transfo_co2 (net_co2_per_liter *  year_fuel_produced)       ;calcuate the co2 produced for that particular plant during fuel production
  set co2_fuel_gen (co2_fuel_gen + year_fuel_transfo_co2)                 ;add that plant's co2 emissions to the running total for all fuel plants
                            
  set year_MWh_produced (Total_Harvested_ton / tonnes_per_MWh )          ;calculate the MWh that will be produced this year based on the amout of switchgrass on hand
  set market_MWh_supply (market_MWh_supply + year_MWh_produced)     ;add that particular plant's electricty generation to the running total for the entire electric market
  set year_eplant_co2 (net_co2_per_MWh * year_MWh_produced)          ;calculate the co2 produced from that particular plant while generating electricity from switchgrass
  set co2_electric_gen (co2_electric_gen + year_eplant_co2)         ;add that plant's co2 emissions to the running total for all electric plants
  
  set co2_fuel_burn (market_fuel_supply * co2_per_liter_burn) ;calculate the emissions from fuel combustion based on the amount of fuel produced from fuel plants. Note this assumes that all fuel produced in a year is burned in that same year.

;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;  

 set Myyear_fuel_transfo_co2 (net_co2_per_liter *  year_fuel_produced)       ;calcuate the co2 produced fo 
 set Myco2_fuel_gen Myyear_fuel_transfo_co2                 ;add that plant's co2 emissions to the running total for all fuel plants
 
 set Myyear_eplant_co2 (net_co2_per_MWh * year_MWh_produced)          ;calculate the co2 produced from that particular plant while generating electricity from switchgrass
 set Myco2_electric_gen Myyear_eplant_co2        ;add that plant's co2 emissions to the running total for all electric plants
 
 set Myco2_fuel_burn (year_MWh_produced * co2_per_liter_burn) ;calculate the emissions from fuel combustion based on the amount of fuel produced from fuel plants. Note this assumes that all fuel produced in a year is burned in that same year.

 
;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;;;MY;;   
end

to calculate_ffuel_production_use
   set MWh_per_tonne_coal (heat_content_coal * 2.326) / (1000 * heat_rate) ;1 btu/lb = 2.326 MJ/tonne 
   set MWh_per_tonne_sg (heat_content_sg / (1000 * heat_rate))
   set tonnes_per_MWh (1 /  MWh_per_tonne_sg)  
end



;script to compare the emissions from fuel and electricity produced in that year from switchgrass to an equvalent amount of fuel and elesctricity produced by other means. 
  
  ;;;;;;;;;;;;;;;;;;;; Note: as written, this doesn't calculate the difference in impact associated with switching from conventional fuels to switchgrass. This just compares the co2 associated with equal amounts of energy produced
  ;;;;;;;;;;;;;;;;;;;; from different fuel sources. For example: if the model showed that 1000 liters of fuel were produced and 500 mWh of electricity were produced from switchgrass, this script will calculate the co2 impact from 
  ;;;;;;;;;;;;;;;;;;;; producing conventional fuel (gasoline) with equivalent energy content to that in 1000 liters of switchgrass fuel, and also the co2 associated with producing 500 MWh of electricity from conventional sources (coal, ng, etc).
  ;;;;;;;;;;;;;;;;;;;; To figure out the co2 impact from utilizing switchgrass, the user will need to define a total market demand for fuel and electricity. Then this calculation section can be modified to calculate the emissions when switchgrass
  ;;;;;;;;;;;;;;;;;;;; provides x% of the total fuel demand, and y% of the total electricity demand (with conventional sources providing (100-x)% of the fuel and (100-y)% of the electricity demand).
  
 
to-report Total_Harvested_ton
   report sum [SG_harvested] of turtles
end

to-report Total_HarvArea_acres
   report sum [SG_harvArea] of turtles
end

to-report Total_planted_acres ;;acres
  report sum [My_area_planted] of turtles
end

to-report Total_planted_ton ;;ton
  report sum [SG_planted_ton] of turtles
end

to-report Total_SG_revenues
  report sum [SG_revenues] of turtles
end

to-report Mean_SG_revenues
  report mean [SG_revenues] of turtles
end
  
to-report Mean_age
  report mean [age_of_farmer] of turtles
end

to-report Mean_Interest_score 
  report mean [Interest_score] of turtles
end  
to-report Mean_risk_aversion 
  report mean [risk_aversion] of turtles
end  

to-report Mean_familiarity_with_SG
  report mean [familiarity_with_SG] of turtles
end  

to-report Mean_My_area_planted
  report mean [My_area_planted] of turtles
end  

to-report Co2_total_from_SG
  report co2_growth + co2_delivery + co2_fuel_gen + co2_electric_gen + co2_fuel_burn
end

to-report Co2delivery
  report co2_delivery
end

to-report Co2_from_gas_and_coal
  report (market_MWH_supply * co2_coal_TFC) + (market_fuel_supply * Lgas_per_Letoh * co2_gasoline_TFC)
end

to-report Co2_from_gas_and-natgas
 report (market_MWH_supply * co2_natgas_TFC) + (market_fuel_supply * Lgas_per_Letoh * co2_gasoline_TFC)
end

to-report Price_of_baseBS
 report Price_of_base
end

to-report Price_of_SGBS
 report Price_of_SG
end

to-report Mean_my_percent_increase
  report mean [my_percent_increase] of turtles
end 

to-report Mean_SG_yield
  report mean [SG_yield] of turtles
end 

to-report Mean_percent_of_planting
  report mean [percent_of_planting] of turtles
end 

to-report Mean_base_yield
  report mean [base_yield] of turtles
end 




to-report Mean_DistFieldToRefinery
  report mean [DistFieldToRefinery] of turtles
end 
to-report Mean_DistFieldToStorage
  report mean [DistFieldToStorage] of turtles
end 
to-report Mean_DistFieldToStoragetoRefinery
  report mean [DistFieldToStorage + DistStorageToRefinery] of turtles
end 


to-report TOTDistFieldToRefinery
  report sum [DistFieldToRefinery] of turtles
end 
to-report TOTDistFieldToStorage
  report sum [DistFieldToStorage] of turtles
end 
to-report TOTDistFieldToStoragetoRefinery
  report sum [DistFieldToStorage + DistStorageToRefinery] of turtles
end 



to-report MeanAcOwned
  report mean [acres_owned] of turtles
end 

to-report MeanPotRev
  report mean [Potential_revenues] of turtles
end 

to-report MeanTotCostPerAcre
  report mean [Total_cost_peracre] of turtles
end 

to-report MeanTotCost
  report mean [Total_cost] of turtles
end 

to-report CountRepl
  report count turtles with [Rep? = TRUE]
end 


to-report MeanEducation
  report mean [education] of turtles
end 

to-report CountComputeruse
  report count turtles with [computer_use? = TRUE]
end 


to-report CountSuccessor
  report count turtles with [successor? = TRUE]
end 

to-report MeanPotProfit
  report mean [Potential_profit] of turtles
end 

to-report CountUserStorage
  report count turtles with [usingStorage? = true]
end 

to-report MeanTransCost
  report mean [TransCost] of turtles
end 

to-report totDistance_to_refinery
  report sum [Distance_to_refinery] of turtles
end 

;###################################################################################################
;###################################################################################################

to-report MeanMyco2_growth 
  report mean [Myco2_growth ] of turtles
end 

to-report MeanMyco2_delivery
  report mean [Myco2_delivery] of turtles
end 

to-report MeanMyco2_electric_gen
  report mean [Myco2_electric_gen] of turtles
end 

to-report MeanMyco2_fuel_gen  
  report mean [Myco2_fuel_gen ] of turtles
end 

to-report MeanMyco2_fuel_burn 
  report mean [Myco2_fuel_burn ] of turtles
end 

to-report MeanMyyear_fuel_transfo_co2
  report mean [Myyear_fuel_transfo_co2] of turtles
end 

to-report MeanMyyear_eplant_co2  
  report mean [Myyear_eplant_co2] of turtles
end 
                  


to-report TOTMyco2_delivery
  report sum [Myco2_delivery] of turtles
end 


;###################################################################################################
;###################################################################################################

;
;###################################################################################################
;###################################################################################################
; for beahavior space to add
;###################################################################################################
;###################################################################################################

to-report highra
  report random-normal max [risk_aversion] of turtles 0 
  end 

to-report medra
  report random-normal mean [risk_aversion] of turtles 0 
  end 
to-report lowra
  report random-normal min [risk_aversion] of turtles 0
  end 


to-report highfa
  report random-normal max [familiarity_with_SG] of turtles 0 
  end 

to-report medfa
  report random-normal mean [familiarity_with_SG] of turtles 0 
  end 
to-report lowfa
  report random-normal min [familiarity_with_SG] of turtles 0
  end 

to-report highis
  report random-normal max [interest_score] of turtles 0 
  end 

to-report medis
  report random-normal mean [interest_score] of turtles 0 
  end 
to-report lowis
  report random-normal min [interest_score] of turtles 0
  end 
;###################################################################################################
;###################################################################################################

;###################################################################################################
;###################################################################################################
; for beahavior space to add
;###################################################################################################
;###################################################################################################

;###################################################################################################
;###################################################################################################

;;education

to-report highEd
  report random-normal max [education] of turtles 0 
  end 

to-report medEd
  report random-normal mean [education] of turtles 0 
  end 
to-report lowEd
  report random-normal min [education] of turtles 0
  end 

;###################################################################################################
;###################################################################################################

;; ECONOMIC

to-report highPotProf
  report random-normal max [Potential_profit] of turtles 0 
  end 

to-report medPotProf
  report random-normal mean [Potential_profit] of turtles 0 
  end 
to-report lowPotProf
  report random-normal min [Potential_profit] of turtles 0
  end 
;###########################################################

to-report highPriceSG
  report random-normal max [Price_of_SG] of turtles 0 
  end 

to-report medPriceSG
  report random-normal mean [Price_of_SG] of turtles 0 
  end 
to-report lowPriceSG
  report random-normal min [Price_of_SG] of turtles 0
  end 

;###########################################################


to-report highCost
  report random-normal max [Total_cost] of turtles 0 
  end 

to-report medCost
    report random-normal mean [Total_cost] of turtles 0 
  end 

to-report lowCost
  report random-normal min [Total_cost] of turtles 0
  end 

;###################################################################################################
;###################################################################################################




to CountPlantbySGprice
  
 ask be_farmers
     [   
    ifelse plant_SG? = TRUE [
     if plant_SG? != FALSE [
       
 ifelse (Price_of_SG > mean SGPriceMem) 
    and (Price_of_SG <=  max SGPriceMem)
     [set MaxSGPrice? TRUE] 
     [set MaxSGPrice? FALSE]

 ifelse (Price_of_SG > min SGPriceMem) 
    and (Price_of_SG <=  mean SGPriceMem)
     [set MeanSGPrice? TRUE] 
     [set MeanSGPrice? FALSE]

ifelse (MaxSGPrice?  != TRUE) and (MeanSGPrice? != TRUE) 
   [set MinSGPrice? TRUE] 
   [set MinSGPrice? FALSE]
     
       ]
     ]
   []
  ]

end




 
;###################################################################################################
;###################################################################################################

to defineeffect 
  ask be_farmers
 ;  with [Plant_SG? = TRUE]
    [
    
    ifelse plant_SG? = TRUE [
     if plant_SG? != FALSE [
    
    ifelse (risk_aversion > medra) 
    and (risk_aversion <= highra)
     [set highra? TRUE] 
     [set highra? FALSE]
     
    ifelse (risk_aversion > lowra) 
   and (risk_aversion < medra)
     [set medra? TRUE]
     [set medra? FALSE]
    
     ifelse (highra? != TRUE) and (medra? != TRUE) 
     [set lowra? TRUE]
     [set lowra? FALSE]
;###################################################################################################     
       ifelse (familiarity_with_SG > medfa) 
    and (familiarity_with_SG <= highfa)
     [set highfa? TRUE] 
     [set highfa? FALSE]
     
    ifelse (familiarity_with_SG > lowfa) 
   and (familiarity_with_SG < medfa)
     [set medfa? TRUE]
     [set medfa? FALSE]
    

     ifelse (highfa? != TRUE) and (medfa? != TRUE) 
     [set lowfa? TRUE]
     [set lowfa? FALSE]
     
;###################################################################################################     
    ifelse (Interest_score > medis) 
    and (Interest_score <= highis)
     [set highis? TRUE] 
     [set highis? FALSE]
     
    ifelse (Interest_score > lowis) 
   and (Interest_score < medis)
     [set medis? TRUE]
     [set medis? FALSE]
      
   ifelse (highis? != TRUE) and (medis? != TRUE) 
     [set lowis? TRUE]
     [set lowis? FALSE]
     
;###################################################################################################    
     
   ifelse (education > medEd) 
    and (education <= highEd)
     [set highEd? TRUE] 
     [set highEd? FALSE]
     
    ifelse (education > lowEd) 
   and (education < medEd)
     [set medEd? TRUE]
     [set medEd? FALSE]
      
   ifelse (highEd? != TRUE) and (medEd? != TRUE) 
     [set lowEd? TRUE]
     [set lowEd? FALSE]
;###################################################################################################     
     
      ifelse (Potential_profit > medPotProf) 
    and (Potential_profit <= highPotProf)
     [set highPotProf? TRUE] 
     [set highPotProf? FALSE]
     
    ifelse (Potential_profit > lowPotProf) 
   and (Potential_profit < medPotProf)
     [set medPotProf? TRUE]
     [set medPotProf? FALSE]
      
   ifelse (highPotProf? != TRUE) and (medPotProf? != TRUE) 
     [set lowPotProf? TRUE]
     [set lowPotProf? FALSE]  
     
 ;###################################################################################################    
    ifelse (Price_of_SG > medPriceSG) 
    and (Price_of_SG <= highPriceSG)
     [set highPriceSG? TRUE] 
     [set highPriceSG? FALSE]
     
    ifelse (Price_of_SG > lowPriceSG) 
   and (Price_of_SG < medPriceSG)
     [set medPriceSG? TRUE]
     [set medPriceSG? FALSE]
      
   ifelse (highPriceSG != TRUE) and (medPriceSG? != TRUE) 
     [set lowPriceSG? TRUE]
     [set lowPriceSG? FALSE]  
     
    
;###################################################################################################  
ifelse (Total_cost > medCost) 
    and (Total_cost <= highCost)
     [set highCost? TRUE] 
     [set highCost? FALSE]
     
    ifelse (Total_cost > lowCost) 
   and (Total_cost < medCost)
     [set medCost? TRUE]
     [set medCost? FALSE]
      
   ifelse (highCost? != TRUE) and (medCost? != TRUE) 
     [set lowCost? TRUE]
     [set lowCost? FALSE]   
     
    ]
  ]
    []
; ask be_farmers [
; if (highra? = TRUE) and (Plant_SG? = TRUE)
 ;[set PHighra? TRUE]
 
 
 
  ]
  ;  ]
   
  end 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UPDATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-attributes
  ask turtles
   [
    ;;; the Following goes up with time ;;;;;;;;;;;
    set age_of_farmer precision (age_of_farmer + 1) 0
    set age_mem lput age_of_farmer age_mem

    set memory_planted lput My_SGplant memory_planted ;; agentset
    set memory_lot_planted lput portion_of_planting memory_lot_planted  
    
    ifelse happy? = TRUE
      [
        ifelse SG_profit < mean [SG_profit] of turtles   
          [set my_percent_increase 1 + random-float .5 ;set low? TRUE 
            set Percent_of_planting initial_Percent_of_planting * my_percent_increase]
          [set my_percent_increase 2 + random-float .5; set low? FALSE
            set Percent_of_planting initial_Percent_of_planting * my_percent_increase]
      ]
      [set Percent_of_planting initial_Percent_of_planting]
      
;;;; ;; death at 75 - if have a successor all attributes stay same except age, if no successor, all change

 if ( age_of_farmer > ( ageofdeath * random-normal 1 0.2 ) ) and ( successor? = TRUE ) [ set age_of_farmer 45 ] 
 if ( age_of_farmer > ( ageofdeath * random-normal 1 0.2 ) ) and ( successor? = FALSE ) 
     [ 

        set age_of_farmer precision (random-normal average_age 10) 0
        set risk_aversion (age_of_farmer + random-normal 0.9 0.3) / 100
        (if (age_of_farmer < average_age) [set risk_aversion risk_aversion - 0.1])
        
       ;  if (computer_use? = TRUE) [    
        set familiarity_with_SG (age_of_farmer + random-normal 4 2 + 2) / 10
        ;]
        (if (age_of_farmer < average_age) [set familiarity_with_SG familiarity_with_SG + 2])
        
        set education random-normal average_education 3
        
        ifelse ( random-float 1 + 0.00001 ) > PropWithSuccessor
                [set successor? FALSE ] 
                [set successor? TRUE ] 
        
        set number_of_fields_owned count patches with [ owned-by = myself ]
        set my_farm patches with [ owned-by = myself]
        ask my_farm [set pcolor [parcels_col] of myself]
        set acres_owned number_of_fields_owned * 10 ;; 6 acres per patch
        
        set variable_cost_peracre random-normal 250 100 ;;V
        set Total_variable_cost variable_cost_peracre * acres_owned 
        
        set fixed_cost_peracre  random-normal 100 50 ;;;F;; per acre
        set total_fixed_cost  fixed_cost_peracre * acres_owned
        
        set Total_cost_peracre precision (fixed_cost_peracre + variable_cost_peracre) 0
        
        set Total_cost precision (total_fixed_cost + Total_variable_cost) 0
        
        ;SG;;;;
        set Sales_price_SG_peracre  price_of_SG;;S;; formula BEunit= F/(S-V);; $ BEdollars=BEunit*S 
        set Potential_revenues SG_yield * acres_owned * price_of_SG
        set Potential_profit Potential_revenues - Total_cost
        
        ;;Base;;;
        set Sales_price_Base_peracre  price_of_base;;S;; formula BEunit= F/(S-V);; $ BEdollars=BEunit*S 
        set current_revenues base_yield * acres_owned * price_of_base  
        set Current_profit current_revenues - Total_cost
        
        ;;be;;  
        set BE Potential_profit - Current_profit
        
        set initial_Percent_of_planting random-float 0.4
        set my_percent_increase 1 + random-float .3  
        
       ; set Distance_to_refinery random-normal 60 40
    
  
         ]
   ]
   
end

to update_effect
  ask be_farmers
 ;  with [Plant_SG? = TRUE]
    [
    
    ifelse plant_SG? = TRUE [
     if plant_SG? != FALSE [
    
    ifelse (risk_aversion > medra) 
    and (risk_aversion <= highra)
     [set highra? TRUE] 
     [set highra? FALSE]
     
    ifelse (risk_aversion > lowra) 
   and (risk_aversion < medra)
     [set medra? TRUE]
     [set medra? FALSE]
    
     ifelse (highra? != TRUE) and (medra? != TRUE) 
     [set lowra? TRUE]
     [set lowra? FALSE]
;###################################################################################################     
       ifelse (familiarity_with_SG > medfa) 
    and (familiarity_with_SG <= highfa)
     [set highfa? TRUE] 
     [set highfa? FALSE]
     
    ifelse (familiarity_with_SG > lowfa) 
   and (familiarity_with_SG < medfa)
     [set medfa? TRUE]
     [set medfa? FALSE]
    

     ifelse (highfa? != TRUE) and (medfa? != TRUE) 
     [set lowfa? TRUE]
     [set lowfa? FALSE]
     
;###################################################################################################     
    ifelse (Interest_score > medis) 
    and (Interest_score <= highis)
     [set highis? TRUE] 
     [set highis? FALSE]
     
    ifelse (Interest_score > lowis) 
   and (Interest_score < medis)
     [set medis? TRUE]
     [set medis? FALSE]
      
   ifelse (highis? != TRUE) and (medis? != TRUE) 
     [set lowis? TRUE]
     [set lowis? FALSE]
     
;###################################################################################################    
     
   ifelse (education > medEd) 
    and (education <= highEd)
     [set highEd? TRUE] 
     [set highEd? FALSE]
     
    ifelse (education > lowEd) 
   and (education < medEd)
     [set medEd? TRUE]
     [set medEd? FALSE]
      
   ifelse (highEd? != TRUE) and (medEd? != TRUE) 
     [set lowEd? TRUE]
     [set lowEd? FALSE]
;###################################################################################################     
     
      ifelse (Potential_profit > medPotProf) 
    and (Potential_profit <= highPotProf)
     [set highPotProf? TRUE] 
     [set highPotProf? FALSE]
     
    ifelse (Potential_profit > lowPotProf) 
   and (Potential_profit < medPotProf)
     [set medPotProf? TRUE]
     [set medPotProf? FALSE]
      
   ifelse (highPotProf? != TRUE) and (medPotProf? != TRUE) 
     [set lowPotProf? TRUE]
     [set lowPotProf? FALSE]  
     
 ;###################################################################################################    
    ifelse (Price_of_SG > medPriceSG) 
    and (Price_of_SG <= highPriceSG)
     [set highPriceSG? TRUE] 
     [set highPriceSG? FALSE]
     
    ifelse (Price_of_SG > lowPriceSG) 
   and (Price_of_SG < medPriceSG)
     [set medPriceSG? TRUE]
     [set medPriceSG? FALSE]
      
   ifelse (highPriceSG != TRUE) and (medPriceSG? != TRUE) 
     [set lowPriceSG? TRUE]
     [set lowPriceSG? FALSE]  
     
    
;###################################################################################################  
ifelse (Total_cost > medCost) 
    and (Total_cost <= highCost)
     [set highCost? TRUE] 
     [set highCost? FALSE]
     
    ifelse (Total_cost > lowCost) 
   and (Total_cost < medCost)
     [set medCost? TRUE]
     [set medCost? FALSE]
      
   ifelse (highCost? != TRUE) and (medCost? != TRUE) 
     [set lowCost? TRUE]
     [set lowCost? FALSE]   
     
     
     ;;;;;;;;;;;;;;;;
 
     
    ]
  ]
    []
; ask be_farmers [
; if (highra? = TRUE) and (Plant_SG? = TRUE)
 ;[set PHighra? TRUE]
 
 
 
  ]
  ;  ]
end
  
to update_patches
 ask patches 
    [  
   set SG_yield 2.5 ;; ton per acre
   set base_yield random-normal 2 0.5
   set area_to_plant? FALSE
    ]
end

to update_global
 set Price_of_SG precision random-normal 350 100 0
 set SGPriceMem lput Price_of_SG SGPriceMem
 
 

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;SHOCKS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to shock_price
  if ticks = Price_shock_at_step
   [
 ; Set Price_of_SG Price_of_SG  *  (1.2 + random-float 0.6)
  Set Price_of_SG Price_of_SG  * (0.2 + random-float 2)
  set new_sgprice Price_of_SG
  ]
end

to commodity_shock
  if ticks = Price_shock_at_step [
  Set Total_cost_peracre Total_cost_peracre  * (0.2 + random-float 2)
  set new_basecost Total_cost_peracre
]
end


;;;;==============================================================================================;;;
;;;;==============================================================================================;;;
;;;;==============================================================================================;;;
;                                         Turtles log for individual analysis with output file  
to exportview

if file-exists? "TESTcombineIMAGES.eps"
     [ file-delete "TESTcombineIMAGES.eps"]
file-open "TESTcombineIMAGES.eps"
;if ticks mod 10 = 0 (
;file-write 
;(task [export-view (word "View-on-tick-" ticks ".png")])
;)

file-close 
  end

to export-data
     ;; If the file already exists, we begin by deleting it, otherwise
    ;; new data would be appended to the old contents.
  if file-exists? "AgentResult.csv"
      [ file-delete "AgentResult.csv"]
   if ticks = 1 [create-files] 

end

to create-files
;; create the file and give the first row column headings
let spacer ","
file-open "AgentResult.csv"
file-print  (list 
  "BSRunNum" spacer
  "ticks" spacer 
  "Who" spacer 
  "Breed" spacer         
  "TotalCostPeracre" spacer   
  "TotalCost" spacer  
  "SalesPriceSGPeracre" spacer
  "SalesPriceBasePperacre"  spacer
  "CurrentRevenues" spacer
  "CurrentProfit" spacer
  "PotentialRevenues" spacer       
  "PotentialProfit"  spacer                
  "SGHarvested" spacer              
  "SGRevenues" spacer
  "SGProfit" spacer
  "ageOfFarmer" spacer
  "riskAversion" spacer
  "FamiliarityWithSG"  spacer     
  "highra" spacer
  "medra" spacer
  "lowra" spacer
  "highfa" spacer
  "medfa" spacer
  "lowfa" spacer
  "highis" spacer
  "medis" spacer
  "lowis" spacer
  "OnTheFence" spacer
  "mindchanged" spacer
  "InterestScore" spacer          
  "Happy" spacer                        
  "Rep" spacer
  "NumberOfFieldsOwned" spacer                                
  "AcresOwned" spacer              
  "NumbeOfFieldPlanted" spacer
  "PlantSG" spacer
  "InitialPercentOfPlanting" spacer
  "MyPercentIncrease" spacer      
  "PercentOfPlanting" spacer        
  "SGHarvArea" spacer
  "MyAreaPlanted" spacer
  "InfRadius" spacer
  "ComShock"  spacer
  "learn"     spacer
   "Priceshock" spacer
   "education" spacer
   "computer_use" spacer
   "successor"                      
        


 )
;file-newline 
 
file-close
export-files
end

to export-files
;; write the information to the file
let spacer ","
file-open "AgentResult.csv"
ask turtles [file-print
  
 (list 
   behaviorspace-run-number spacer
  ticks spacer 
 who spacer 
 breed spacer                 
  Total_cost_peracre spacer   
  Total_cost spacer  
  Sales_price_SG_peracre spacer
  Sales_price_Base_peracre  spacer
  Current_revenues spacer
  Current_profit spacer
  Potential_revenues spacer       
  Potential_profit  spacer           
  SG_harvested spacer              
  SG_revenues spacer
  SG_profit spacer
  age_of_farmer spacer
  risk_aversion spacer
  Familiarity_with_SG  spacer     
  highra? spacer
  medra? spacer
  lowra? spacer
  highfa? spacer
  medfa? spacer
  lowfa? spacer
  highis? spacer
  medis? spacer
  lowis? spacer
  On_the_fence? spacer
  mindchanged? spacer
  Interest_score spacer          
  Happy? spacer                        
  Rep? spacer
  Number_of_fields_owned spacer                                  
  Acres_owned spacer              
  number_of_field_planted spacer
  Plant_SG? spacer
  Initial_Percent_of_planting spacer
  My_percent_increase spacer      
  Percent_of_planting spacer        
  SG_harvArea spacer
  My_area_planted spacer
  influence_radius spacer
  commodity_shock? spacer
  learn? spacer
  Price_shock? spacer
  education spacer
  computer_use? spacer
  successor?                                 
                 

)
   
   
   ] 
file-close
end 



;;;;==============================================================================================;;;
;;;;========================================= SHORT =====================================================;;;
;;;;==============================================================================================;;;
;                                         Turtles log for individual analysis with output file  


to export-dataShort
     ;; If the file already exists, we begin by deleting it, otherwise
    ;; new data would be appended to the old contents.
  ;if file-exists? "IndiAna7.csv"
    ;  [ file-delete "IndiAna7.csv"]
    if ticks = 1 [create-filesShort] 

end

to create-filesShort
;; create the file and give the first row column headings
let spacer ","
file-open "Indi.2runsSHORT.csv"
file-print  (list 
  "BSRunNum" spacer
  "ticks" spacer 
  "Who" spacer 
  "Breed" spacer         
  "TotalCostPeracre" spacer   
  
  "SalesPriceSGPeracre" spacer
  "SalesPriceBasePperacre"  spacer
  
 
  "PotentialRevenues" spacer       
                
  "SGHarvested" spacer              
  "SGRevenues" spacer
  
  "ageOfFarmer" spacer
  "riskAversion" spacer
  "FamiliarityWithSG"  spacer     
  
  "OnTheFence" spacer
  "mindchanged" spacer
  "InterestScore" spacer          
  "Happy" spacer                        
  "Rep" spacer
                                
               
  
  "PlantSG" spacer
  "InitialPercentOfPlanting" spacer
  "MyPercentIncrease" spacer      
  "PercentOfPlanting" spacer        
  
  "MyAreaPlanted" spacer
  "InfRadius" spacer
  "ComShock"  spacer
  "learn"     spacer
   "Priceshock" spacer
   "education" spacer
   "computer_use" spacer
   "successor"                      
        


 )
;file-newline 
 
file-close
export-filesShort
end

to export-filesShort
;; write the information to the file
let spacer ","
file-open "Indi.2runsSHORT.csv"
ask turtles [file-print
  
 (list 
   behaviorspace-run-number spacer
  ticks spacer 
 who spacer 
 breed spacer                 
  Total_cost_peracre spacer   
  
  Sales_price_SG_peracre spacer
  Sales_price_Base_peracre  spacer
 
  Potential_revenues spacer       
          
  SG_harvested spacer              
  SG_revenues spacer
 
  age_of_farmer spacer
  risk_aversion spacer
  Familiarity_with_SG  spacer     
  
  On_the_fence? spacer
  mindchanged? spacer
  Interest_score spacer          
  Happy? spacer                        
  Rep? spacer
            
  
  Plant_SG? spacer
  Initial_Percent_of_planting spacer
  My_percent_increase spacer      
  Percent_of_planting spacer        
 
  My_area_planted spacer
  influence_radius spacer
  commodity_shock? spacer
  learn? spacer
  Price_shock? spacer
  education spacer
  computer_use? spacer
  successor?                                 
                 

)
   
   
   ] 
file-close
end 


;; makes a movie of the model; stops when there are 3000 turtles
;; and exports movie to a file
to make-movie

  ;; prompt user for movie location
  user-message "First, save your new movie file (choose a name ending with .mov)"
  let path user-new-file
  if not is-string? path [ stop ]  ;; stop if user canceled

  ;; run the model
  setup
  movie-start path
  movie-grab-view
  while [ count turtles < 3000 ]
    [ go
      movie-grab-view ]

  ;; export the movie
  movie-close
  user-message (word "Exported movie to " path)
end





to update-movie
  ifelse movie-status = "No movie."
  [let filename (word "insert-file-name-here.mov")
    movie-start filename]
  [ifelse capture-node = "entire interface"
    [movie-grab-interface]
    [movie-grab-view]]
end 
@#$#@#$#@
GRAPHICS-WINDOW
465
26
939
521
17
17
13.26
1
10
1
1
1
0
1
1
1
-17
17
-17
17
1
1
1
ticks
30.0

SLIDER
4
79
269
112
initial-number-of-farmers
initial-number-of-farmers
0
100
50
1
1
NIL
HORIZONTAL

BUTTON
3
10
91
69
Setup
setup
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
94
10
166
70
Go
Go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
3
189
128
234
Breakeven farmers
count be_farmers
0
1
11

MONITOR
3
145
130
190
No breakeven farmers
count no_be_farmers
0
1
11

PLOT
1
380
363
613
Farmers decision_point
years
number of farmers
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Planting farmers" 1.0 0 -7858858 true "" "plot count be_farmers with [Plant_SG? = true]"
"Breakeven-farmers" 1.0 0 -7500403 true "" "plot count be_farmers"
"No_breakeven" 1.0 0 -14070903 true "" "plot count no_be_farmers"

PLOT
945
10
1250
226
Switchgrass production
years
Ton of SG
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Profitable F" 1.0 0 -13791810 true "" "plot count turtles with [happy? = true]"
"No profit F" 1.0 0 -5298144 true "" "plot count turtles with [happy? = false]"

PLOT
944
478
1252
688
SG production
years
Ton of SG
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"area_planted" 1.0 0 -7500403 true "" "plot sum [My_area_planted] of turtles"
"area_harvested" 1.0 0 -2674135 true "" "plot sum [SG_harvArea ] of turtles"

SLIDER
282
146
461
179
influence_radius
influence_radius
0
12
12
4
1
patches
HORIZONTAL

BUTTON
168
10
255
69
go
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
70
280
162
325
Planting true
count be_farmers with [Plant_SG? = true]
17
1
11

MONITOR
70
239
165
284
Planting false
count be_farmers with [plant_SG? = false]
17
1
11

SWITCH
164
119
267
152
learn?
learn?
0
1
-1000

PLOT
944
234
1249
467
co2 Emissions Comparision
year
tons of Co2
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"SG" 1.0 0 -12087248 true "" "plot Co2_total_from_SG"
"gas&coal" 1.0 0 -7500403 true "" "plot Co2_from_gas_and_coal"
"gas&natgas" 1.0 0 -10146808 true "" "plot Co2_from_gas_and-natgas"

SWITCH
164
158
268
191
Price_shock?
Price_shock?
0
1
-1000

SLIDER
281
187
460
220
Price_shock_at_step
Price_shock_at_step
0
100
50
10
1
step
HORIZONTAL

MONITOR
163
282
256
327
Harvest True
count be_farmers with [harv? = true]
17
1
11

MONITOR
164
239
259
284
Harvest False
count be_farmers with [harv? = false]
17
1
11

MONITOR
258
328
398
373
Collect RevenueTrue
count be_farmers with [CollRev? = true]
17
1
11

MONITOR
258
283
400
328
Collect RevenueFalse
count be_farmers with [CollRev? = false]
17
1
11

SWITCH
164
196
268
229
commodity_shock?
commodity_shock?
0
1
-1000

CHOOSER
280
232
461
277
Transp_Scenarios
Transp_Scenarios
"FieldtoRefinery" "FieldtoStorageandRefinery"
0

SWITCH
365
17
457
50
record-movie?
record-movie?
0
1
-1000

CHOOSER
290
85
421
130
capture-node
capture-node
"view only" "entire interface"
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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

farm
false
0
Rectangle -7500403 true true 30 195 210 285
Polygon -7500403 true true 105 135 150 120 195 135
Rectangle -16777216 true false 105 255 150 285
Rectangle -7500403 true true 210 105 270 285
Rectangle -16777216 true false 105 120 120 120
Rectangle -2674135 true false 210 75 225 75
Rectangle -16777216 true false 225 105 240 120
Rectangle -16777216 true false 225 255 240 285
Rectangle -16777216 true false 210 60 225 60
Rectangle -7500403 true true 210 90 270 105
Polygon -1 true false 210 90 225 60 255 60 270 90
Polygon -7500403 true true 0 195 45 165 105 135 195 135 255 165 240 195
Rectangle -1 true false 45 210 75 240
Rectangle -1 true false 165 210 195 240
Rectangle -7500403 true true 45 195 225 195
Rectangle -1 true false 105 165 150 180
Rectangle -1 true false 15 195 285 195

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
  <experiment name="Noshock_5repMeasureYes" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-view "Noschok behaviorspace-run-number.PNG"</final>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count be_farmers with [plant_SG? = true]</metric>
    <metric>count be_farmers with [happy? = true]</metric>
    <metric>Mean_age</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>total_production_global</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <enumeratedValueSet variable="initial-number-of-farmers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_base">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_SG">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence_radius">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Shock1_5repMeasureYes" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count be_farmers with [plant_SG? = true]</metric>
    <metric>count be_farmers with [happy? = true]</metric>
    <metric>Mean_age</metric>
    <metric>SGprice1</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>total_production_global</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <enumeratedValueSet variable="initial-number-of-farmers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_base">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_SG">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence_radius">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock_1?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Shock2_5repMeasureYes" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-view " shock2.PNG"</final>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count be_farmers with [plant_SG? = true]</metric>
    <metric>count be_farmers with [happy? = true]</metric>
    <metric>Mean_age</metric>
    <metric>SGprice2</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>total_production_global</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <enumeratedValueSet variable="initial-number-of-farmers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_base">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_SG">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence_radius">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock_2?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Shock3_5repMeasureYes" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-view " shock3.PNG"</final>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count be_farmers with [plant_SG? = true]</metric>
    <metric>count be_farmers with [happy? = true]</metric>
    <metric>Mean_age</metric>
    <metric>SGprice3</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>total_production_global</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <enumeratedValueSet variable="initial-number-of-farmers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_base">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_SG">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence_radius">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock_3?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Shock4_5repMeasureYes" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-view " shock4.PNG"</final>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count be_farmers with [plant_SG? = true]</metric>
    <metric>count be_farmers with [happy? = true]</metric>
    <metric>Mean_age</metric>
    <metric>SGprice4</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>total_production_global</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <enumeratedValueSet variable="initial-number-of-farmers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_base">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_SG">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence_radius">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock_4?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Shock5_5repMeasureYes" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-view " shock5.PNG"</final>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count be_farmers with [plant_SG? = true]</metric>
    <metric>count be_farmers with [happy? = true]</metric>
    <metric>Mean_age</metric>
    <metric>SGprice5</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>total_production_global</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <enumeratedValueSet variable="initial-number-of-farmers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_base">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_SG">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence_radius">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock_5?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Shock6_5repMeasureYes" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-view " shock6.PNG"</final>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count be_farmers with [plant_SG? = true]</metric>
    <metric>count be_farmers with [happy? = true]</metric>
    <metric>Mean_age</metric>
    <metric>SGprice6</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>total_production_global</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <enumeratedValueSet variable="initial-number-of-farmers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_base">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_SG">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence_radius">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock_6?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Shock7_5repMeasureYes" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-view " shock7.PNG"</final>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count be_farmers with [plant_SG? = true]</metric>
    <metric>count be_farmers with [happy? = true]</metric>
    <metric>Mean_age</metric>
    <metric>SGprice7</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>total_production_global</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <enumeratedValueSet variable="initial-number-of-farmers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_base">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_SG">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence_radius">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock_7?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Shock8_5repMeasureYes" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-view " shock8.PNG"</final>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count be_farmers with [plant_SG? = true]</metric>
    <metric>count be_farmers with [happy? = true]</metric>
    <metric>Mean_age</metric>
    <metric>SGprice8</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>total_production_global</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <enumeratedValueSet variable="initial-number-of-farmers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_base">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_SG">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence_radius">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock_8?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Shock9_5repMeasureYes" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-view " shock9.PNG"</final>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count be_farmers with [plant_SG? = true]</metric>
    <metric>count be_farmers with [happy? = true]</metric>
    <metric>Mean_age</metric>
    <metric>SGprice9</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>total_production_global</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <enumeratedValueSet variable="initial-number-of-farmers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_base">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_SG">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence_radius">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock_9?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Shock10_5repMeasureYes" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-view " shock10.PNG"</final>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count be_farmers with [plant_SG? = true]</metric>
    <metric>count be_farmers with [happy? = true]</metric>
    <metric>Mean_age</metric>
    <metric>SGprice10</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>total_production_global</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <enumeratedValueSet variable="initial-number-of-farmers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_base">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_SG">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence_radius">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock_10?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Newset" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count be_farmers with [plant_SG? = true]</metric>
    <metric>count be_farmers with [happy? = true]</metric>
    <metric>Mean_age</metric>
    <metric>SGPrice1</metric>
    <metric>SGPrice2</metric>
    <metric>SGPrice3</metric>
    <metric>SGPrice4</metric>
    <metric>SGPrice5</metric>
    <metric>SGPrice6</metric>
    <metric>SGPrice7</metric>
    <metric>SGPrice8</metric>
    <metric>SGPrice9</metric>
    <metric>SGPrice10</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>total_production_global</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <enumeratedValueSet variable="Price_of_base">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_SG">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock_1?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock_2?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock_3?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock_4?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock_5?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock_6?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock_7?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock_8?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock_9?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock_10?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Newset3" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count be_farmers with [plant_SG? = true]</metric>
    <metric>count be_farmers with [happy? = true]</metric>
    <metric>Mean_age</metric>
    <metric>new_price</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>total_production_global</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <enumeratedValueSet variable="Price_of_base">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_SG">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_shock_at_step">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_shock?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="LAstSG" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>Price_of_baseBS</metric>
    <metric>Price_of_SGBS</metric>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count turtles with [plant_SG? = true]</metric>
    <metric>count turtles with [happy? = true]</metric>
    <metric>count turtles with [On_the_fence? = true]</metric>
    <metric>count turtles with [mindchanged? = true]</metric>
    <metric>Mean_age</metric>
    <metric>new_price</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>Mean_percent_of_planting</metric>
    <metric>Total_planted</metric>
    <metric>Total_planted_ton</metric>
    <metric>Total_Harvested_global</metric>
    <metric>Total_HarvArea_global</metric>
    <metric>Mean_risk_aversion</metric>
    <metric>Mean_Familiarity_with_SG</metric>
    <metric>Mean_my_percent_increase</metric>
    <metric>Mean_SG_yield</metric>
    <metric>Mean_base_yield</metric>
    <metric>Mean_My_area_planted</metric>
    <metric>Total_planted</metric>
    <metric>Total_SG_revenues</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <enumeratedValueSet variable="Price_shock_at_step">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_shock_Increase?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_shock_decrease?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence_radius">
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="LAstSG2" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>Price_of_baseBS</metric>
    <metric>Price_of_SGBS</metric>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count turtles with [plant_SG? = true]</metric>
    <metric>count turtles with [happy? = true]</metric>
    <metric>count turtles with [On_the_fence? = true]</metric>
    <metric>count turtles with [mindchanged? = true]</metric>
    <metric>Mean_age</metric>
    <metric>new_price</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>Mean_percent_of_planting</metric>
    <metric>Total_Harvested_ton</metric>
    <metric>Total_HarvArea_acres</metric>
    <metric>Total_planted_acres</metric>
    <metric>Total_planted_ton</metric>
    <metric>Mean_risk_aversion</metric>
    <metric>Mean_Familiarity_with_SG</metric>
    <metric>Mean_my_percent_increase</metric>
    <metric>Mean_SG_yield</metric>
    <metric>Mean_base_yield</metric>
    <metric>Mean_My_area_planted</metric>
    <metric>Total_SG_revenues</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <enumeratedValueSet variable="Price_shock_at_step">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_shock?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence_radius">
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="LAstSG3" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>Price_of_baseBS</metric>
    <metric>Price_of_SGBS</metric>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count turtles with [plant_SG? = true]</metric>
    <metric>count turtles with [happy? = true]</metric>
    <metric>count turtles with [On_the_fence? = true]</metric>
    <metric>count turtles with [mindchanged? = true]</metric>
    <metric>Mean_age</metric>
    <metric>new_price</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>Mean_percent_of_planting</metric>
    <metric>Total_Harvested_ton</metric>
    <metric>Total_HarvArea_acres</metric>
    <metric>Total_planted_acres</metric>
    <metric>Total_planted_ton</metric>
    <metric>Mean_risk_aversion</metric>
    <metric>Mean_Familiarity_with_SG</metric>
    <metric>Mean_my_percent_increase</metric>
    <metric>Mean_SG_yield</metric>
    <metric>Mean_base_yield</metric>
    <metric>Mean_My_area_planted</metric>
    <metric>Total_SG_revenues</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <enumeratedValueSet variable="Price_shock_at_step">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_shock?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence_radius">
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Vaexp0.100steps" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>Price_of_baseBS</metric>
    <metric>Price_of_SGBS</metric>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count turtles with [plant_SG? = true]</metric>
    <metric>count turtles with [happy? = true]</metric>
    <metric>count turtles with [On_the_fence? = true]</metric>
    <metric>count turtles with [mindchanged? = true]</metric>
    <metric>Mean_age</metric>
    <metric>new_price</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>Mean_percent_of_planting</metric>
    <metric>Total_Harvested_ton</metric>
    <metric>Total_HarvArea_acres</metric>
    <metric>Total_planted_acres</metric>
    <metric>Total_planted_ton</metric>
    <metric>Mean_risk_aversion</metric>
    <metric>Mean_Familiarity_with_SG</metric>
    <metric>Mean_my_percent_increase</metric>
    <metric>Mean_SG_yield</metric>
    <metric>Mean_base_yield</metric>
    <metric>Mean_My_area_planted</metric>
    <metric>Total_SG_revenues</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <metric>Mean_average_distance_to_refinery</metric>
    <enumeratedValueSet variable="Price_shock_at_step">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Influence_radius">
      <value value="4"/>
      <value value="8"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_shock?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Vaexp1.100steps" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>Price_of_baseBS</metric>
    <metric>Price_of_SGBS</metric>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count turtles with [plant_SG? = true]</metric>
    <metric>count turtles with [happy? = true]</metric>
    <metric>count turtles with [On_the_fence? = true]</metric>
    <metric>count turtles with [mindchanged? = true]</metric>
    <metric>Mean_age</metric>
    <metric>new_price</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>Mean_percent_of_planting</metric>
    <metric>Total_Harvested_ton</metric>
    <metric>Total_HarvArea_acres</metric>
    <metric>Total_planted_acres</metric>
    <metric>Total_planted_ton</metric>
    <metric>Mean_risk_aversion</metric>
    <metric>Mean_Familiarity_with_SG</metric>
    <metric>Mean_my_percent_increase</metric>
    <metric>Mean_SG_yield</metric>
    <metric>Mean_base_yield</metric>
    <metric>Mean_My_area_planted</metric>
    <metric>Total_SG_revenues</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <metric>Mean_average_distance_to_refinery</metric>
    <enumeratedValueSet variable="Price_shock_at_step">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Influence_radius">
      <value value="4"/>
      <value value="8"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_shock?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_of_SG">
      <value value="200"/>
      <value value="400"/>
      <value value="600"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Vaexp0.50steps" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>Price_of_baseBS</metric>
    <metric>Price_of_SGBS</metric>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count turtles with [plant_SG? = true]</metric>
    <metric>count turtles with [happy? = true]</metric>
    <metric>count turtles with [On_the_fence? = true]</metric>
    <metric>count turtles with [mindchanged? = true]</metric>
    <metric>Mean_age</metric>
    <metric>new_price</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>Mean_percent_of_planting</metric>
    <metric>Total_Harvested_ton</metric>
    <metric>Total_HarvArea_acres</metric>
    <metric>Total_planted_acres</metric>
    <metric>Total_planted_ton</metric>
    <metric>Mean_risk_aversion</metric>
    <metric>Mean_Familiarity_with_SG</metric>
    <metric>Mean_my_percent_increase</metric>
    <metric>Mean_SG_yield</metric>
    <metric>Mean_base_yield</metric>
    <metric>Mean_My_area_planted</metric>
    <metric>Total_SG_revenues</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <metric>Mean_average_distance_to_refinery</metric>
    <enumeratedValueSet variable="Price_shock_at_step">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Influence_radius">
      <value value="4"/>
      <value value="8"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_shock?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Vaexp1.50steps" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>Price_of_baseBS</metric>
    <metric>Price_of_SGBS</metric>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count turtles with [plant_SG? = true]</metric>
    <metric>count turtles with [happy? = true]</metric>
    <metric>count turtles with [On_the_fence? = true]</metric>
    <metric>count turtles with [mindchanged? = true]</metric>
    <metric>Mean_age</metric>
    <metric>new_price</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>Mean_percent_of_planting</metric>
    <metric>Total_Harvested_ton</metric>
    <metric>Total_HarvArea_acres</metric>
    <metric>Total_planted_acres</metric>
    <metric>Total_planted_ton</metric>
    <metric>Mean_risk_aversion</metric>
    <metric>Mean_Familiarity_with_SG</metric>
    <metric>Mean_my_percent_increase</metric>
    <metric>Mean_SG_yield</metric>
    <metric>Mean_base_yield</metric>
    <metric>Mean_My_area_planted</metric>
    <metric>Total_SG_revenues</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <metric>Mean_average_distance_to_refinery</metric>
    <metric>count turtles with [highra? = true]</metric>
    <metric>count turtles with [highfa? = true]</metric>
    <metric>count be_farmers with [highis? = true]</metric>
    <metric>count turtles with [medra? = true]</metric>
    <metric>count turtles with [medfa? = true]</metric>
    <metric>count be_farmers with [medis? = true]</metric>
    <metric>count turtles with [lowra? = true]</metric>
    <metric>count turtles with [lowfa? = true]</metric>
    <metric>count be_farmers with [lowis? = true]</metric>
    <enumeratedValueSet variable="Price_shock_at_step">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_shock?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Influence_radius">
      <value value="4"/>
      <value value="8"/>
      <value value="12"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="BS50steps" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>Price_of_baseBS</metric>
    <metric>Price_of_SGBS</metric>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count turtles with [plant_SG? = true]</metric>
    <metric>count turtles with [happy? = true]</metric>
    <metric>count turtles with [On_the_fence? = true]</metric>
    <metric>count turtles with [mindchanged? = true]</metric>
    <metric>Mean_age</metric>
    <metric>new_price</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>Mean_percent_of_planting</metric>
    <metric>Total_Harvested_ton</metric>
    <metric>Total_HarvArea_acres</metric>
    <metric>Total_planted_acres</metric>
    <metric>Total_planted_ton</metric>
    <metric>Mean_risk_aversion</metric>
    <metric>Mean_Familiarity_with_SG</metric>
    <metric>Mean_my_percent_increase</metric>
    <metric>Mean_SG_yield</metric>
    <metric>Mean_base_yield</metric>
    <metric>Mean_My_area_planted</metric>
    <metric>Total_SG_revenues</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <metric>Mean_average_distance_to_refinery</metric>
    <metric>count turtles with [highra? = true]</metric>
    <metric>count turtles with [highfa? = true]</metric>
    <metric>count be_farmers with [highis? = true]</metric>
    <metric>count turtles with [medra? = true]</metric>
    <metric>count turtles with [medfa? = true]</metric>
    <metric>count be_farmers with [medis? = true]</metric>
    <metric>count turtles with [lowra? = true]</metric>
    <metric>count turtles with [lowfa? = true]</metric>
    <metric>count be_farmers with [lowis? = true]</metric>
    <enumeratedValueSet variable="Price_shock_at_step">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_shock?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="commodity_shock?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Influence_radius">
      <value value="4"/>
      <value value="8"/>
      <value value="12"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="IndAna3.2rep" repetitions="2" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>Price_of_baseBS</metric>
    <metric>Price_of_SGBS</metric>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count turtles with [plant_SG? = true]</metric>
    <metric>count turtles with [happy? = true]</metric>
    <metric>count turtles with [On_the_fence? = true]</metric>
    <metric>count turtles with [mindchanged? = true]</metric>
    <metric>Mean_age</metric>
    <metric>new_price</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>Mean_percent_of_planting</metric>
    <metric>Total_Harvested_ton</metric>
    <metric>Total_HarvArea_acres</metric>
    <metric>Total_planted_acres</metric>
    <metric>Total_planted_ton</metric>
    <metric>Mean_risk_aversion</metric>
    <metric>Mean_Familiarity_with_SG</metric>
    <metric>Mean_my_percent_increase</metric>
    <metric>Mean_SG_yield</metric>
    <metric>Mean_base_yield</metric>
    <metric>Mean_My_area_planted</metric>
    <metric>Total_SG_revenues</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <metric>Mean_average_distance_to_refinery</metric>
    <metric>count turtles with [highra? = true]</metric>
    <metric>count turtles with [highfa? = true]</metric>
    <metric>count be_farmers with [highis? = true]</metric>
    <metric>count turtles with [medra? = true]</metric>
    <metric>count turtles with [medfa? = true]</metric>
    <metric>count be_farmers with [medis? = true]</metric>
    <metric>count turtles with [lowra? = true]</metric>
    <metric>count turtles with [lowfa? = true]</metric>
    <metric>count be_farmers with [lowis? = true]</metric>
    <enumeratedValueSet variable="Price_shock_at_step">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_shock?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="commodity_shock?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Influence_radius">
      <value value="4"/>
      <value value="8"/>
      <value value="12"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="IndiAna.2rep" repetitions="2" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>Price_of_baseBS</metric>
    <metric>Price_of_SGBS</metric>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count turtles with [plant_SG?= TRUE]</metric>
    <metric>count turtles with [happy? = TRUE]</metric>
    <metric>count turtles with [On_the_fence? = TRUE]</metric>
    <metric>count turtles with [mindchanged? = TRUE]</metric>
    <metric>Mean_age</metric>
    <metric>new_price</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>Mean_percent_of_planting</metric>
    <metric>Total_Harvested_ton</metric>
    <metric>Total_HarvArea_acres</metric>
    <metric>Total_planted_acres</metric>
    <metric>Total_planted_ton</metric>
    <metric>Mean_risk_aversion</metric>
    <metric>Mean_Familiarity_with_SG</metric>
    <metric>Mean_my_percent_increase</metric>
    <metric>Mean_SG_yield</metric>
    <metric>Mean_base_yield</metric>
    <metric>Mean_My_area_planted</metric>
    <metric>Total_SG_revenues</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <metric>Mean_average_distance_to_refinery</metric>
    <metric>count turtles with [highra? = TRUE]</metric>
    <metric>count turtles with [highfa? = TRUE]</metric>
    <metric>count be_farmers with [highis? = TRUE]</metric>
    <metric>count turtles with [medra? = TRUE]</metric>
    <metric>count turtles with [medfa? = TRUE]</metric>
    <metric>count be_farmers with [medis? = TRUE]</metric>
    <metric>count turtles with [lowra? = TRUE]</metric>
    <metric>count turtles with [lowfa? = TRUE]</metric>
    <metric>count be_farmers with [lowis? = TRUE]</metric>
    <metric>MeanAcOwned</metric>
    <metric>MeanPotRev</metric>
    <metric>MeanTotCost</metric>
    <metric>CountRepl</metric>
    <metric>MeanEducation</metric>
    <metric>CountComputeruse</metric>
    <metric>CountSuccessor</metric>
    <enumeratedValueSet variable="Price_shock_at_step">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_shock?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="commodity_shock?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Influence_radius">
      <value value="4"/>
      <value value="8"/>
      <value value="12"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="IndiAna.5rep" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>Price_of_baseBS</metric>
    <metric>Price_of_SGBS</metric>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count turtles with [plant_SG?= TRUE]</metric>
    <metric>count turtles with [happy? = TRUE]</metric>
    <metric>count turtles with [On_the_fence? = TRUE]</metric>
    <metric>count turtles with [mindchanged? = TRUE]</metric>
    <metric>Mean_age</metric>
    <metric>new_price</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>Mean_percent_of_planting</metric>
    <metric>Total_Harvested_ton</metric>
    <metric>Total_HarvArea_acres</metric>
    <metric>Total_planted_acres</metric>
    <metric>Total_planted_ton</metric>
    <metric>Mean_risk_aversion</metric>
    <metric>Mean_Familiarity_with_SG</metric>
    <metric>Mean_my_percent_increase</metric>
    <metric>Mean_SG_yield</metric>
    <metric>Mean_base_yield</metric>
    <metric>Mean_My_area_planted</metric>
    <metric>Total_SG_revenues</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <metric>Mean_average_distance_to_refinery</metric>
    <metric>count turtles with [highra? = TRUE]</metric>
    <metric>count turtles with [highfa? = TRUE]</metric>
    <metric>count be_farmers with [highis? = TRUE]</metric>
    <metric>count turtles with [medra? = TRUE]</metric>
    <metric>count turtles with [medfa? = TRUE]</metric>
    <metric>count be_farmers with [medis? = TRUE]</metric>
    <metric>count turtles with [lowra? = TRUE]</metric>
    <metric>count turtles with [lowfa? = TRUE]</metric>
    <metric>count be_farmers with [lowis? = TRUE]</metric>
    <metric>MeanAcOwned</metric>
    <metric>MeanPotRev</metric>
    <metric>MeanTotCostGlob</metric>
    <metric>CountRepl</metric>
    <metric>MeanEducation</metric>
    <metric>CountComputeruse</metric>
    <metric>CountSuccessor</metric>
    <metric>MeanTotCostPerAcre</metric>
    <metric>MeanPotProfit</metric>
    <enumeratedValueSet variable="Price_shock_at_step">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_shock?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="commodity_shock?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Influence_radius">
      <value value="4"/>
      <value value="8"/>
      <value value="12"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="NewExpAvecView" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>Price_of_baseBS</metric>
    <metric>Price_of_SGBS</metric>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count turtles with [plant_SG?= TRUE]</metric>
    <metric>count turtles with [happy? = TRUE]</metric>
    <metric>count turtles with [On_the_fence? = TRUE]</metric>
    <metric>count turtles with [mindchanged? = TRUE]</metric>
    <metric>Mean_age</metric>
    <metric>new_price</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>Mean_percent_of_planting</metric>
    <metric>Total_Harvested_ton</metric>
    <metric>Total_HarvArea_acres</metric>
    <metric>Total_planted_acres</metric>
    <metric>Total_planted_ton</metric>
    <metric>Mean_risk_aversion</metric>
    <metric>Mean_Familiarity_with_SG</metric>
    <metric>Mean_my_percent_increase</metric>
    <metric>Mean_SG_yield</metric>
    <metric>Mean_base_yield</metric>
    <metric>Mean_My_area_planted</metric>
    <metric>Total_SG_revenues</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <metric>Mean_average_distance_to_refinery</metric>
    <metric>count turtles with [highra? = TRUE]</metric>
    <metric>count turtles with [highfa? = TRUE]</metric>
    <metric>count be_farmers with [highis? = TRUE]</metric>
    <metric>count turtles with [medra? = TRUE]</metric>
    <metric>count turtles with [medfa? = TRUE]</metric>
    <metric>count be_farmers with [medis? = TRUE]</metric>
    <metric>count turtles with [lowra? = TRUE]</metric>
    <metric>count turtles with [lowfa? = TRUE]</metric>
    <metric>count be_farmers with [lowis? = TRUE]</metric>
    <metric>MeanAcOwned</metric>
    <metric>MeanPotRev</metric>
    <metric>MeanTotCostGlob</metric>
    <metric>CountRepl</metric>
    <metric>MeanEducation</metric>
    <metric>CountComputeruse</metric>
    <metric>CountSuccessor</metric>
    <metric>MeanTotCostPerAcre</metric>
    <metric>MeanPotProfit</metric>
    <enumeratedValueSet variable="Price_shock_at_step">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_shock?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="commodity_shock?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Transp_Scenarios">
      <value value="&quot;Field_to_Refinery&quot;"/>
      <value value="&quot;Field_to_StorageandRefinery&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="NewExp1200" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>Price_of_baseBS</metric>
    <metric>Price_of_SGBS</metric>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count turtles with [plant_SG? = TRUE]</metric>
    <metric>count turtles with [happy? = TRUE]</metric>
    <metric>count be_farmers with [On_the_fence? = TRUE]</metric>
    <metric>count be_farmers with [mindchanged? = TRUE]</metric>
    <metric>Mean_age</metric>
    <metric>new_sgprice</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>Mean_percent_of_planting</metric>
    <metric>Total_Harvested_ton</metric>
    <metric>Total_HarvArea_acres</metric>
    <metric>Total_planted_acres</metric>
    <metric>Total_planted_ton</metric>
    <metric>Mean_risk_aversion</metric>
    <metric>Mean_Familiarity_with_SG</metric>
    <metric>Mean_my_percent_increase</metric>
    <metric>Mean_SG_yield</metric>
    <metric>Mean_base_yield</metric>
    <metric>Mean_My_area_planted</metric>
    <metric>Total_SG_revenues</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <metric>count turtles with [highra? = TRUE]</metric>
    <metric>count turtles with [highfa? = TRUE]</metric>
    <metric>count turtles with [highis? = TRUE]</metric>
    <metric>count turtles with [medra? = TRUE]</metric>
    <metric>count turtles with [medfa? = TRUE]</metric>
    <metric>count turtles with [medis? = TRUE]</metric>
    <metric>count turtles with [lowra? = TRUE]</metric>
    <metric>count turtles with [lowfa? = TRUE]</metric>
    <metric>count turtles with [lowis? = TRUE]</metric>
    <metric>MeanAcOwned</metric>
    <metric>MeanPotRev</metric>
    <metric>MeanTotCost</metric>
    <metric>CountRepl</metric>
    <metric>MeanEducation</metric>
    <metric>CountSuccessor</metric>
    <metric>MeanTotCostPerAcre</metric>
    <metric>MeanPotProfit</metric>
    <metric>CountUserStorage</metric>
    <metric>MeanTransCost</metric>
    <metric>new_basecost</metric>
    <metric>count be_farmers with [highEd? = TRUE]</metric>
    <metric>count be_farmers with [medEd? = TRUE]</metric>
    <metric>count be_farmers with [lowEd? = TRUE]</metric>
    <metric>count be_farmers with [highPotProf? = TRUE]</metric>
    <metric>count be_farmers with [medPotProf? = TRUE]</metric>
    <metric>count be_farmers with [lowPotProf? = TRUE]</metric>
    <metric>count be_farmers with [highPriceSG? = TRUE]</metric>
    <metric>count be_farmers with [medPriceSG? = TRUE]</metric>
    <metric>count be_farmers with [lowPriceSG? = TRUE]</metric>
    <metric>count be_farmers with [highCost? = TRUE]</metric>
    <metric>count be_farmers with [medCost? = TRUE]</metric>
    <metric>count be_farmers with [lowCost? = TRUE]</metric>
    <metric>MeanMyco2_growth</metric>
    <metric>MeanMyco2_delivery</metric>
    <metric>MeanMyco2_electric_gen</metric>
    <metric>MeanMyco2_fuel_gen</metric>
    <metric>MeanMyco2_fuel_burn</metric>
    <metric>MeanMyyear_fuel_transfo_co2</metric>
    <metric>MeanMyyear_eplant_co2</metric>
    <metric>Mean_DistFieldToRefinery</metric>
    <metric>Mean_DistFieldToStorage</metric>
    <metric>Mean_DistFieldToStoragetoRefinery</metric>
    <metric>TOTDistFieldToRefinery</metric>
    <metric>TOTDistFieldToStorage</metric>
    <metric>TOTDistFieldToStoragetoRefinery</metric>
    <metric>TOTMyco2_delivery</metric>
    <enumeratedValueSet variable="Price_shock_at_step">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_shock?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="commodity_shock?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Influence_radius">
      <value value="4"/>
      <value value="8"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Transp_Scenarios">
      <value value="&quot;FieldtoRefinery&quot;"/>
      <value value="&quot;FieldtoStorageandRefinery&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="NewExp240" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>Price_of_baseBS</metric>
    <metric>Price_of_SGBS</metric>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count turtles with [plant_SG? = TRUE]</metric>
    <metric>count turtles with [happy? = TRUE]</metric>
    <metric>count be_farmers with [On_the_fence? = TRUE]</metric>
    <metric>count be_farmers with [mindchanged? = TRUE]</metric>
    <metric>Mean_age</metric>
    <metric>new_sgprice</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>Mean_percent_of_planting</metric>
    <metric>Total_Harvested_ton</metric>
    <metric>Total_HarvArea_acres</metric>
    <metric>Total_planted_acres</metric>
    <metric>Total_planted_ton</metric>
    <metric>Mean_risk_aversion</metric>
    <metric>Mean_Familiarity_with_SG</metric>
    <metric>Mean_my_percent_increase</metric>
    <metric>Mean_SG_yield</metric>
    <metric>Mean_base_yield</metric>
    <metric>Mean_My_area_planted</metric>
    <metric>Total_SG_revenues</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <metric>count turtles with [highra? = TRUE]</metric>
    <metric>count turtles with [highfa? = TRUE]</metric>
    <metric>count turtles with [highis? = TRUE]</metric>
    <metric>count turtles with [medra? = TRUE]</metric>
    <metric>count turtles with [medfa? = TRUE]</metric>
    <metric>count turtles with [medis? = TRUE]</metric>
    <metric>count turtles with [lowra? = TRUE]</metric>
    <metric>count turtles with [lowfa? = TRUE]</metric>
    <metric>count turtles with [lowis? = TRUE]</metric>
    <metric>MeanAcOwned</metric>
    <metric>MeanPotRev</metric>
    <metric>MeanTotCost</metric>
    <metric>CountRepl</metric>
    <metric>MeanEducation</metric>
    <metric>CountSuccessor</metric>
    <metric>MeanTotCostPerAcre</metric>
    <metric>MeanPotProfit</metric>
    <metric>CountUserStorage</metric>
    <metric>MeanTransCost</metric>
    <metric>new_basecost</metric>
    <metric>count be_farmers with [highEd? = TRUE]</metric>
    <metric>count be_farmers with [medEd? = TRUE]</metric>
    <metric>count be_farmers with [lowEd? = TRUE]</metric>
    <metric>count be_farmers with [highPotProf? = TRUE]</metric>
    <metric>count be_farmers with [medPotProf? = TRUE]</metric>
    <metric>count be_farmers with [lowPotProf? = TRUE]</metric>
    <metric>count be_farmers with [highPriceSG? = TRUE]</metric>
    <metric>count be_farmers with [medPriceSG? = TRUE]</metric>
    <metric>count be_farmers with [lowPriceSG? = TRUE]</metric>
    <metric>count be_farmers with [highCost? = TRUE]</metric>
    <metric>count be_farmers with [medCost? = TRUE]</metric>
    <metric>count be_farmers with [lowCost? = TRUE]</metric>
    <metric>MeanMyco2_growth</metric>
    <metric>MeanMyco2_delivery</metric>
    <metric>MeanMyco2_electric_gen</metric>
    <metric>MeanMyco2_fuel_gen</metric>
    <metric>MeanMyco2_fuel_burn</metric>
    <metric>MeanMyyear_fuel_transfo_co2</metric>
    <metric>MeanMyyear_eplant_co2</metric>
    <metric>Mean_DistFieldToRefinery</metric>
    <metric>Mean_DistFieldToStorage</metric>
    <metric>Mean_DistFieldToStoragetoRefinery</metric>
    <metric>TOTDistFieldToRefinery</metric>
    <metric>TOTDistFieldToStorage</metric>
    <metric>TOTDistFieldToStoragetoRefinery</metric>
    <metric>TOTMyco2_delivery</metric>
    <enumeratedValueSet variable="Price_shock_at_step">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_shock?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="commodity_shock?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Influence_radius">
      <value value="4"/>
      <value value="8"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Transp_Scenarios">
      <value value="&quot;FieldtoRefinery&quot;"/>
      <value value="&quot;FieldtoStorageandRefinery&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="NewExp240Perso" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>Price_of_baseBS</metric>
    <metric>Price_of_SGBS</metric>
    <metric>count be_farmers</metric>
    <metric>count no_be_farmers</metric>
    <metric>count turtles with [plant_SG? = TRUE]</metric>
    <metric>count turtles with [happy? = TRUE]</metric>
    <metric>count be_farmers with [On_the_fence? = TRUE]</metric>
    <metric>count be_farmers with [mindchanged? = TRUE]</metric>
    <metric>Mean_age</metric>
    <metric>new_sgprice</metric>
    <metric>Mean_SG_revenues</metric>
    <metric>Mean_Interest_score</metric>
    <metric>Mean_percent_of_planting</metric>
    <metric>Total_Harvested_ton</metric>
    <metric>Total_HarvArea_acres</metric>
    <metric>Total_planted_acres</metric>
    <metric>Total_planted_ton</metric>
    <metric>Mean_risk_aversion</metric>
    <metric>Mean_Familiarity_with_SG</metric>
    <metric>Mean_my_percent_increase</metric>
    <metric>Mean_SG_yield</metric>
    <metric>Mean_base_yield</metric>
    <metric>Mean_My_area_planted</metric>
    <metric>Total_SG_revenues</metric>
    <metric>co2_total_from_SG</metric>
    <metric>Co2_from_gas_and_coal</metric>
    <metric>co2_from_gas_and-natgas</metric>
    <metric>count turtles with [highra? = TRUE]</metric>
    <metric>count turtles with [highfa? = TRUE]</metric>
    <metric>count turtles with [highis? = TRUE]</metric>
    <metric>count turtles with [medra? = TRUE]</metric>
    <metric>count turtles with [medfa? = TRUE]</metric>
    <metric>count turtles with [medis? = TRUE]</metric>
    <metric>count turtles with [lowra? = TRUE]</metric>
    <metric>count turtles with [lowfa? = TRUE]</metric>
    <metric>count turtles with [lowis? = TRUE]</metric>
    <metric>MeanAcOwned</metric>
    <metric>MeanPotRev</metric>
    <metric>MeanTotCost</metric>
    <metric>CountRepl</metric>
    <metric>MeanEducation</metric>
    <metric>CountSuccessor</metric>
    <metric>MeanTotCostPerAcre</metric>
    <metric>MeanPotProfit</metric>
    <metric>CountUserStorage</metric>
    <metric>MeanTransCost</metric>
    <metric>new_basecost</metric>
    <metric>count be_farmers with [highEd? = TRUE]</metric>
    <metric>count be_farmers with [medEd? = TRUE]</metric>
    <metric>count be_farmers with [lowEd? = TRUE]</metric>
    <metric>count be_farmers with [highPotProf? = TRUE]</metric>
    <metric>count be_farmers with [medPotProf? = TRUE]</metric>
    <metric>count be_farmers with [lowPotProf? = TRUE]</metric>
    <metric>count be_farmers with [highPriceSG? = TRUE]</metric>
    <metric>count be_farmers with [medPriceSG? = TRUE]</metric>
    <metric>count be_farmers with [lowPriceSG? = TRUE]</metric>
    <metric>count be_farmers with [highCost? = TRUE]</metric>
    <metric>count be_farmers with [medCost? = TRUE]</metric>
    <metric>count be_farmers with [lowCost? = TRUE]</metric>
    <metric>MeanMyco2_growth</metric>
    <metric>MeanMyco2_delivery</metric>
    <metric>MeanMyco2_electric_gen</metric>
    <metric>MeanMyco2_fuel_gen</metric>
    <metric>MeanMyco2_fuel_burn</metric>
    <metric>MeanMyyear_fuel_transfo_co2</metric>
    <metric>MeanMyyear_eplant_co2</metric>
    <metric>Mean_DistFieldToRefinery</metric>
    <metric>Mean_DistFieldToStorage</metric>
    <metric>Mean_DistFieldToStoragetoRefinery</metric>
    <metric>TOTDistFieldToRefinery</metric>
    <metric>TOTDistFieldToStorage</metric>
    <metric>TOTDistFieldToStoragetoRefinery</metric>
    <metric>TOTMyco2_delivery</metric>
    <enumeratedValueSet variable="Price_shock_at_step">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learn?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price_shock?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="commodity_shock?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Influence_radius">
      <value value="4"/>
      <value value="8"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Transp_Scenarios">
      <value value="&quot;FieldtoRefinery&quot;"/>
      <value value="&quot;FieldtoStorageandRefinery&quot;"/>
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
