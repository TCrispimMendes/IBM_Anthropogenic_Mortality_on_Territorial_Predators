extensions [time]

globals[

;; schedule

  start-time ;; simulation start date (time extension)
  sim_year ;; simulation current date (time extension)
  max_ticks ;; maximum simulation ticks


;; bird's related variables

  SImp_non-territorial ;; survival rate of non-territorial individuals in patches with additional mortality
  SImp_territorial ;; survival rate of territorial individuals in patches with additional mortality

  R2 ;; propensity to become territorial in immature birds
  R3 ;; propensity to become territorial in subadults
  R4 ;; propensity to become territorial in first-year adults
  RA ;; propensity to become territorial in adults


;;  Outputs

  N_Total ;; total number of individuals
  N_pairs_before_S ;; number of pairs before the survival procedure
  N_pairs_after_S ;; number of pairs after the survival procedure
  N_adults ;; number of adults
  N_non_adults ;; number of non-adults
  N_territorials ;; number of territorial individuals
  N_non_territorials ;; number of non-territorial individuals
  Pop_persistence ;; number of years of population persistence
  N_initial ;; number of individuals at the beginning of the simulation
  N_final ;; number of individuals at the end of the simulation
  N_emigrated ;; number of individuals who emigrated during the simulation
  Lambda ;; annual population growth rate

  N_deaths_NT_with_AM ;; number of non-territorials that died in patches with additional mortality
  N_deaths_NT_without_AM ;; number of non-territorials that died in patches without additional mortality
  P_NT_AM ;; percentage of non-territorials that died in patches with additional mortality
  P_deaths_NT_AM ;; percentage of non-territorials that died in patches without additional mortality

  N_deaths_T_with_AM ;; number of territorials that died in patches with additional mortality
  N_deaths_T_without_AM ;; number of territorials that died in patches without additional mortality
  P_T_AM ;; percentage f territorials that died in patches with additional mortality
  P_deaths_T_AM ;; percentage of territorials that died in patches without additional mortality
  P_pairs_AM ;; percentage of pairs in patches with additional mortality

  N_NT_before_S ;; number of non-territorial individuals before the survival procedure
  N_T_before_S ;; number of territorial individuals before the survival procedure
  N_NT_after_S ;; number of non-territorial individuals after the survival procedure
  N_T_after_S ;; number of territorial individuals after the survival procedure
  list_NT_survival ;; list to store the annual survival rates of non-territorials
  list_T_survival ;; list to store the annual survival rates of territorials
  mean_NT_survival ;; mean of the annual survival rates of non-territorials
  mean_T_survival ;; mean of the annual survival rates of territorials

]

breed [birds  bird]

birds-own [
  sex
  age
  age_class
  territorial
  patch_type
]

patches-own [
  p_type
  additional_mortality
  occupied_M
  occupied_F
  non_territorials
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Setup procedures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to Setup

  clear-all

  (ifelse
    population = "SPT" [setup_globals_SPT] ;; South Portuguese population
    population = "CAT" [setup_globals_CAT] ;; Catalan population
    population = "Sensitivity_Analysis" [setup_globals_Sensitivity_Analysis])

  setup_patches
  setup_individuals

  Outputs_Collection

  reset-ticks

end


to setup_globals_SPT

  ;; schedule

  set start-time time:create "0000/01/01" ;; start the simulation on January 1 of the year 0000
  set sim_year time:anchor-to-ticks start-time 1 "years" ;;  sim_year starts at start-time and has 1-year increments for each tick
  time:anchor-schedule start-time 1 "years" ;; schedule events (time extension) starts at start-time and has 1-year increments for each tick

  set max_ticks 51  ;; maximum simulation ticks

  time:schedule-event "observer" [ ->  ;; add a event to the discrete event schedule
    set N_initial count birds
    setup_additional_mortality
    set SImp_non-territorial (1 - mortality_AM_NT)
    set SImp_territorial (1 - Mortality_AM_T)
    set N_deaths_NT_with_AM 0
    set N_deaths_NT_without_AM 0
    set N_deaths_T_with_AM 0
    set N_deaths_T_without_AM 0]
  start-time


  ;; bird's related variables

  set F2 0.285714 ;; fertility of two-year olds
  set F3 0.5 ;; fertility of three-year olds
  set FA 0.830328 ;; fertility of adults

  set S1 0.662946  ;; survival rate of individuals during their first year of life
  set S23 0.719976 ;; survival rate of individuals during their second and third years of life
  set S4 0.874833  ;; survival rate of individuals during their fourth year of life
  set SA 0.937492  ;; survival rate of individuals during their fifth and subsequent (adult) years of life
  set SImp_non-territorial 1
  set SImp_territorial 1

  set R2 0.160763 ;; propensity to become territorial in immature birds
  set R3 0.679674 ;; propensity to become territorial in subadults
  set R4 0.934197 ;; propensity to become territorial in first-year adults
  set RA 1 ;;propensity to become territorial in adults

  set Pop_persistence 50

  set list_NT_survival []
  set list_T_survival []

end


to setup_globals_CAT

  ;; schedule

  set start-time time:create "0000/01/01" ;; start the simulation on January 1, 1992
  set sim_year time:anchor-to-ticks start-time 1 "years" ;;  sim_year starts at start-time and has 1-year increments for each tick
  time:anchor-schedule start-time 1 "years" ;; schedule events (time extension) starts at start-time and has 1-year increments for each tick

  set max_ticks 51 ;; maximum simulation ticks

  time:schedule-event "observer" [ ->  ;; add a event to the discrete event schedule
    set N_initial count birds
    setup_additional_mortality
    set SImp_non-territorial (1 - mortality_AM_NT)
    set SImp_territorial (1 - Mortality_AM_T)
    set N_deaths_NT_with_AM 0
    set N_deaths_NT_without_AM 0
    set N_deaths_T_with_AM 0
    set N_deaths_T_without_AM 0]
 start-time ;;  perform the event  "PopulatioN_initial" years after the start of the simulation

  ;; bird's related variables

  set F2 0.285714 ;; fertility of two-year olds
  set F3 0.4 ;; fertility of three-year olds
  set FA 1.129109 ;; fertility of adults

  set S1 0.480266  ;; survival rate of individuals during their first year of life
  set S23 0.573883  ;; survival rate of individuals during their second and third years of life
  set S4 0.829643  ;; survival rate of individuals during their fourth year of life
  set SA 0.889066  ;; survival rate of individuals during their fifth and subsequent (adult) years of life
  set SImp_non-territorial 1
  set SImp_territorial 1

  set R2 0.160763 ;; propensity to become territorial in immature birds
  set R3 0.679674 ;; propensity to become territorial in subadults
  set R4 0.934197 ;; propensity to become territorial in first-year adults
  set RA 1 ;;propensity to become territorial in adults

  set Pop_persistence 50

end


to setup_globals_Sensitivity_Analysis

  ;; schedule

  set start-time time:create "0000/01/01" ;; start the simulation on January 1 of the year 0000
  set sim_year time:anchor-to-ticks start-time 1 "years" ;;  sim_year starts at start-time and has 1-year increments for each tick
  time:anchor-schedule start-time 1 "years" ;; schedule events (time extension) starts at start-time and has 1-year increments for each tick

  set max_ticks 51 ;; maximum simulation ticks

  time:schedule-event "observer" [ ->  ;; add a event to the discrete event schedule
    set N_initial count birds
    setup_additional_mortality
    set SImp_non-territorial (1 - mortality_AM_NT)
    set SImp_territorial (1 - Mortality_AM_T)
    set N_deaths_NT_with_AM 0
    set N_deaths_NT_without_AM 0
    set N_deaths_T_with_AM 0
    set N_deaths_T_without_AM 0]
  start-time ;;  perform the event  "PopulatioN_initial" years after the start of the simulation


  ;; bird's related variables

  set SImp_non-territorial 1
  set SImp_territorial 1

  set R2 0.160763 ;; propensity to become territorial in immature birds
  set R3 0.679674 ;; propensity to become territorial in subadults
  set R4 0.934197 ;; propensity to become territorial in first-year adults
  set RA 1 ;;propensity to become territorial in adults

  set Pop_persistence 50

end


to setup_patches

  if %_Unoccupiable + %_B + %_A != 100 [error "Sum of patch type percentages is different from 100!"] ;; Causes a runtime error if the sum of percetages is different from 100


  ask n-of round ((%_B * 0.01) * count patches) patches with [p_type = 0] [ ;; ask to a number of patches corresponding to the % defined of %_B to
    set p_type "B"] ;; define p_type "B"

  ask n-of round ((%_A * 0.01) * count patches) patches with [p_type = 0] [ ;; ask to a number of patches corresponding to the % defined of %_A to
    set p_type "A"] ;; define p_type "A"

  ask patches with [p_type = 0] [ ;; ask the remaining patches whit p_type = 0 to
    set p_type "unoccupiable"] ;; define p_type "unoccupiable"

  ask patches[
    set additional_mortality False
    set occupied_M False
    set occupied_F False
    (ifelse
      p_type = "unoccupiable" [set pcolor grey] ;; if the p_type is unoccupiable it defines the color grey
      p_type = "B" [set pcolor red] ;; if the p_type is B it defines the color red
      p_type = "A"  [set pcolor green] ;; if it is A, define the color green
    )
  ]

end


to setup_additional_mortality

  ;; define patches with additional mortality according to the percentage

  (ifelse
    Patches_AM != 0 [
      let B_patches patches with [p_type = "B"]
      let A_patches patches with [p_type = "A"]
      ask n-of round ((Patches_AM * 0.01) * count B_patches) B_patches [set additional_mortality True]
      ask n-of round ((Patches_AM * 0.01) * count A_patches) A_patches [set additional_mortality True]]
    Patches_AM = 0 [
      let B_patches patches with [p_type = "B"]
      let A_patches patches with [p_type = "A"]
      ask n-of round ((B_AM * 0.01) * count B_patches) B_patches [set additional_mortality True]
      ask n-of round ((A_AM * 0.01) * count A_patches) A_patches [set additional_mortality True]]
  )

end


to setup_individuals

 ;; create territorials

  create-birds  2 [set age 2 set age_class "juvenile"]
  create-birds  7 [set age 3 set age_class "immature"]
  create-birds  7 [set age 4 set age_class "subadult"]
  create-birds  6 [set age 5 set age_class "adult"]
  create-birds  6 [set age 6 set age_class "adult"]
  create-birds  5 [set age 7 set age_class "adult"]
  create-birds  4 [set age 8 set age_class "adult"]
  create-birds  4 [set age 9 set age_class "adult"]
  create-birds  4 [set age 10 set age_class "adult"]
  create-birds  3 [set age 11 set age_class "adult"]
  create-birds  3 [set age 12 set age_class "adult"]
  create-birds  3 [set age 13 set age_class "adult"]
  create-birds  2 [set age 14 set age_class "adult"]
  create-birds  2 [set age 15 set age_class "adult"]
  create-birds  2 [set age 16 set age_class "adult"]
  create-birds  2 [set age 17 set age_class "adult"]
  create-birds  2 [set age 18 set age_class "adult"]
  create-birds  1 [set age 19 set age_class "adult"]
  create-birds  1 [set age 20 set age_class "adult"]

  ask n-of 33 birds  [
    set sex "F"
    set color pink
    set territorial False
  ]

  ask birds  with [sex = 0][
    set sex "M"
    set color blue
    set territorial False
  ]


  ask birds  with [sex = "M"] [
    Set_Territory who]

  ask birds  with [sex = "F"] [
    Set_Territory who]


 ;; create non-territorials

  create-birds  24 [custom-bird set age 1 set age_class "subadult"]
  create-birds  14 [custom-bird set age 2 set age_class "juvenile"]
  create-birds  4 [custom-bird set age 3 set age_class "immature"]

  let non-territorials birds  with [territorial = False]
  Dispersal  non-territorials

  ask patch 0 0 [set non_territorials count birds-here with [territorial = False]]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Go procedures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to Go

  if ticks = max_ticks [stop] ;; stops the simulation when it reaches max_ticks
  tick ;; add one tick

  time:go-until sim_year ;; Dispatch all of the events in the discrete event schedule that are scheduled for times up until sim_year

  Ageing
  Territorial_recruitment
  Reproduction
  Disperse_from_Nest

  Survival

  Outputs_Collection

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  processes at agent level  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to Ageing

  ask birds  [
    set age age + 1] ;; add +1 year to age

  ask birds  with [age <= 5] [ ;; ask birds  with 5 or less years
    (ifelse
      age = 2 [set age_class "juvenile"] ;; update age_class to juvenile
      age = 3 [set age_class "immature"] ;; update age_class to immature
      age = 4 [set age_class "subadult"] ;; update age_class to subadult
      age = 5 [set age_class "adult"]) ;; update age_class to adult
  ]

end


to Territorial_recruitment

  ask birds  [

   (ifelse
      age = 2 and territorial = False [ ;; if immature without territory
        if random-bernoulli R2 [ ;; check if it becomes territorial according to the probability of become territorial in immature birds (R2), if yes
          Set_Territory who]] ;; the individual execute the "Set_Territory" process

      age = 3 and territorial = False [ ;; if subadult without territory
        if random-bernoulli R3 [ ;; check if it becomes territorial according to the probability of become territorial in subadult birds (R3), if yes
          Set_Territory who]] ;; the individual execute the "Set_Territory" process

      age = 4 and territorial = False [ ;; if aged 5 without territory
        if random-bernoulli R4 [ ;; check if it becomes territorial according to the probability of become territorial in adult birds (R4), if yes
          Set_Territory who]] ;; the individual execute the "Set_Territory" process

      age > 4 and territorial = False [ ;; if adult without territory
        if random-bernoulli RA [ ;; check if it becomes territorial according to the probability of become territorial in adult birds (R4), if yes
          Set_Territory who]]) ;; the individual execute the "Set_Territory" process
  ]

end


to Set_Territory [id]

  ask bird id [ ;; ask to the bird with the "who" equal to "id" to
    ifelse sex = "M" ;; differentiates between males and females

    ; Males
    [let me self ;; set me as myself

      let possible-pair birds  with [ ;; define characteristics of possible pair
        sex = "F" and  ;; male and
        not any? link-neighbors and  ;; without pair and
        territorial = True] ;; with territory and

      ifelse any? possible-pair ;; differentiates when there are patches with a lonely territorial female

      ; with possible-pair
      [ask patch-here [set non_territorials non_territorials - 1] ;; ask the patch where it is to subtract 1 from the non_territorials
        move-to one-of possible-pair ;; choose one of the possible pairs and move to their patch
        set patch_type [p_type] of patch-here ;; set the "patch_type" variable as the p_type of the patch where the individual is
        set territorial True ;; set territorial True
        create-link-with one-of other birds-here with [territorial = True] ;; create a link with a bird with territory here, other than me
        ask patch-here [ ;; ask the patch where it is
          set occupied_M True ;; to set occupied_M True
          if non_territorials > 0 [ ;; if there is any non_territorial individual in the patch
            let dispersers turtles-here with [territorial = False]  ;; define "dispersers" as the individuals in this patch with no defined territory
            Dispersal  dispersers ;; run the Dispersal  procedure for the dispersers
            set non_territorials 0]]] ;; define non_territorials as 0

      ; without patches with a lonely territorial female
      [let patches_A_male patches with [p_type = "A" and occupied_M = False and occupied_F = False] ;; define patches_A_male as patches with A p_type and not occupied by males
        ifelse any? patches_A_male ;; differentiates when there are patches with A p_type and not occupied by males

        ; with patches with A p_type and not occupied by males
        [ask patch-here [set non_territorials non_territorials - 1] ;; ask the patch where it is to subtract 1 from the non_territorials
          move-to one-of patches_A_male ;; choose a patch with "A" p_type and without any male territory, and move there
          set patch_type [p_type] of patch-here ;; set the "patch_type" variable as the p_type of the patch where the individual is
          set territorial True ;; set territorial True
          ask patch-here [ ;; ask the patch where it is
            set occupied_M True ;; to set occupied_M True
            if non_territorials = 5 [ ;; if there is 5 non_territorial individuals in the patch
              let dispersers one-of turtles-here with [territorial = False]  ;; define "dispersers" as one of the individuals in this patch with no defined territory
              Dispersal  dispersers ;; run the Dispersal  procedure for the dispersers
              set non_territorials 4] ;; define non_territorials as 4
        ]]

        ; without patches with A p_type and not occupied by males
        []
    ]]


    ; Females
    [let me self ;; set me as myself

      let possible-pair birds  with [ ;; define characteristics of possible pair
        sex = "M" and  ;; male and
        not any? link-neighbors and  ;; without pair and
        territorial = True] ;; with territory and

      ifelse any? possible-pair ;; differentiates when there are patches with a lonely territorial female

      ; with possible-pair
      [ask patch-here [set non_territorials non_territorials - 1] ;; ask the patch where it is to subtract 1 from the non_territorials
        move-to one-of possible-pair ;; choose one of the possible pairs and move to their patch
        set patch_type [p_type] of patch-here ;; set the "patch_type" variable as the p_type of the patch where the individual is
        set territorial True ;; set territorial True
        create-link-with one-of other birds-here with [territorial = True] ;; create a link with a bird with territory here, other than me
        ask patch-here [ ;; ask the patch where it is
          set occupied_F True ;; to set occupied_M True
          if non_territorials > 0 [ ;; if there is any non_territorial individual in the patch
            let dispersers turtles-here with [territorial = False]  ;; define "dispersers" as the individuals in this patch with no defined territory
            Dispersal  dispersers ;; run the Dispersal  procedure for the dispersers
            set non_territorials 0]]] ;; define non_territorials as 0

      ; without possible-pair
      []
  ]]

end


to Reproduction

    ask birds  with [sex = "F" and any? link-neighbors][ ;; ask female birds  with pair

    (ifelse
      age_class = "subadult" or age_class = "adult" [ ;; if subadult or adult
        let fertility FA  ;; define the fertility as FA * type_effect
        let n_chicks number_of_fledgers fertility ;; define the n_chicks as the result of the number_of_fledgers procedure (poisson distribution) with mean "fertility"
        if n_chicks > 0 [ ;; if the defined number of chicks is greater than 0
          Hatch_Chicks who n_chicks ;; the individual execute the "Hatch_Chicks" process
      ]]

      age_class = "immature" [ ;; if immature
        let fertility F3  ;; define the fertility as F3 * type_effect
        let n_chicks number_of_fledgers fertility ;; define the n_chicks as the result of the number_of_fledgers procedure (poisson distribution) with mean "fertility"
        if n_chicks > 0 [ ;; if the defined number of chicks is greater than 0
          Hatch_Chicks who n_chicks ;; the individual execute the "Hatch_Chicks" process
      ]]

      age_class = "juvenile" [ ;; if juvenile
        let fertility F2 ;; define the fertility as F2 * type_effect
        let n_chicks number_of_fledgers fertility ;; define the n_chicks as the result of the number_of_fledgers procedure (poisson distribution) with mean "fertility"
        if n_chicks > 0 [ ;; if the defined number of chicks is greater than 0
          Hatch_Chicks who n_chicks ;; the individual execute the "Hatch_Chicks" process
      ]]
    )
  ]

end


to Hatch_Chicks [id n_chicks]

  ask bird id [
    repeat n_chicks [ ;; repeat next procedure as many times as n_chicks
      hatch 1 [ ;; give birth to a chick
        custom-bird ;; with the characteristics defined in custom-bird
]]]

end


to Disperse_from_Nest

  let dispersers birds  with [age = 1]
  Dispersal dispersers

end


to Dispersal [dispersers]

  ask dispersers [ ;; ask dispersers
    let patches_A patches with [p_type = "A" and occupied_M = False and occupied_F = False and non_territorials < 5] ;; define the temporary variable "patches_A" as the patches with A p_type, without territories and less than 5 non territorial indiiduals
    ifelse any? patches_A ;; if there is any patches_A
   ; with A patches
    [move-to one-of patches_A ;; move to one of the patches_A
      set patch_type [p_type] of patch-here] ;; set the "patch_type" variable as the p_type of the patch where the individual is
   ; without A patches
    [let patches_B patches with [p_type = "B" and non_territorials < 5] ;; define the temporary variable "patches_B" as the patches with B p_type and less than 5 non territorial individuals
      ifelse any? patches_B ;; if there is any patches_B
    ; with B patches
      [move-to one-of patches_B ;; move to one of the patches_mediumbad
        set patch_type [p_type] of patch-here] ;; set the "patch_type" variable as the p_type of the patch where the individual is
    ; without B patches
      [set N_emigrated N_emigrated + 1  ;; add 1 to N_emigrated
        die] ;; die (equivalent to migrate)
    ]
    ask patch-here [set non_territorials non_territorials + 1]
  ]

end


to Survival ;; individual additionl mortality

  set N_pairs_before_S count links
  set N_NT_before_S Count birds  with [territorial = False]
  set N_T_before_S Count birds  with [territorial = TRUE]

  ask birds  [

    (ifelse
      age = 1 [ ;; individuals aged 1 have a probability of dying equal to 1-S1
        let mortality_rate 0  ;; create the temporary variable "mortality_rate"
        ifelse [additional_mortality] of patch-here = True [set mortality_rate (1 - (S1 * SImp_non-territorial))] [set mortality_rate (1 - S1)] ;; if the individual is in a patch with "additional_mortality" set to "TRUE" the additional survival probability value (terriotial or non-territorial) is multipied to the baseline probability of survival
        if random-bernoulli (mortality_rate) [ ;; have a probability of dying equal to "mortality_rate"
          ask patch-here [set non_territorials non_territorials - 1] ;; ask the patch where it is to subtract 1 from the non_territorials
          ifelse [additional_mortality] of patch-here = True [set N_deaths_NT_with_AM N_deaths_NT_with_AM + 1][set N_deaths_NT_without_AM N_deaths_NT_without_AM + 1]
          die]] ;; die

      age = 2 or age = 3 [ ;; individuals aged 2 or 3
        (ifelse
          territorial = True [ ;; if territorial
            let mortality_rate 0  ;; create the temporary variable "mortality_rate"
            ifelse [additional_mortality] of patch-here = True [set mortality_rate (1 - (S23 * SImp_territorial))] [set mortality_rate (1 - S23)] ;; if the individual is in a patch with "additional_mortality" set to "TRUE" the additional survival probability value (terriotial or non-territorial) is multipied to the baseline probability of survival
            if random-bernoulli (mortality_rate) [ ;; have a probability of dying equal to "mortality_rate"
              (ifelse
                sex = "M" [ ;; if it's a male
                  ask patch-here [set occupied_M False]] ;; ask the patch to set occupied_M False
                sex = "F" [ ;; if it's a female
                  ask patch-here [set occupied_F False]]) ;; ask the patch to set occupied_F False
              ifelse [additional_mortality] of patch-here = True [set N_deaths_T_with_AM N_deaths_T_with_AM + 1][set N_deaths_T_without_AM N_deaths_T_without_AM + 1]
              die]] ;; die
          territorial = False [ ;; if non-territorial
            let mortality_rate 0  ;; create the temporary variable "mortality_rate"
            ifelse [additional_mortality] of patch-here = True [set mortality_rate (1 - (S23 * SImp_non-territorial))] [set mortality_rate (1 - S23)] ;; if the individual is in a patch with "additional_mortality" set to "TRUE" the additional survival probability value (terriotial or non-territorial) is multipied to the baseline probability of survival
            if random-bernoulli (mortality_rate) [ ;; have a probability of dying equal to "mortality_rate"
              ask patch-here [set non_territorials non_territorials - 1] ;; ask the patch where it is to subtract 1 from the non_territorials
              ifelse [additional_mortality] of patch-here = True [set N_deaths_NT_with_AM N_deaths_NT_with_AM + 1][set N_deaths_NT_without_AM N_deaths_NT_without_AM + 1]
              die]] ;; die
      )]

      age = 4 [ ;; individuals aged 4
        (ifelse
          territorial = True [ ;; if territorial
            let mortality_rate 0  ;; create the temporary variable "mortality_rate"
            ifelse [additional_mortality] of patch-here = True [set mortality_rate (1 - (S4 * SImp_territorial))] [set mortality_rate (1 - S4)] ;; if the individual is in a patch with "additional_mortality" set to "TRUE" the additional survival probability value (terriotial or non-territorial) is multipied to the baseline probability of survival
            if random-bernoulli (mortality_rate) [ ;; ;; have a probability of dying equal to "mortality_rate"
              (ifelse
                sex = "M" [ ;; if it's a male
                  ask patch-here [set occupied_M False]] ;; ask the patch to set occupied_M False
                sex = "F" [ ;; if it's a female
                  ask patch-here [set occupied_F False]]) ;; ask the patch to set occupied_F False
              ifelse [additional_mortality] of patch-here = True [set N_deaths_T_with_AM N_deaths_T_with_AM + 1][set N_deaths_T_without_AM N_deaths_T_without_AM + 1]
              die]] ;; die
          territorial = False [ ;; if non-territorial
            let mortality_rate 0  ;; create the temporary variable "mortality_rate"
            ifelse [additional_mortality] of patch-here = True [set mortality_rate (1 - (S4 * SImp_non-territorial))] [set mortality_rate (1 - S4)] ;; if the individual is in a patch with "additional_mortality" set to "TRUE" the additional survival probability value (terriotial or non-territorial) is multipied to the baseline probability of survival
            if random-bernoulli (mortality_rate) [ ;; have a probability of dying equal to "mortality_rate"
              ask patch-here [set non_territorials non_territorials - 1] ;; ask the patch where it is to subtract 1 from the non_territorials
              ifelse [additional_mortality] of patch-here = True [set N_deaths_NT_with_AM N_deaths_NT_with_AM + 1][set N_deaths_NT_without_AM N_deaths_NT_without_AM + 1]
              die]] ;; die
      )]

      age > 4 and age < 20 [ ;; individuals over the age of 4
        (ifelse
          territorial = True [ ;; if territorial
            let mortality_rate 0  ;; create the temporary variable "mortality_rate"
            ifelse [additional_mortality] of patch-here = True [set mortality_rate (1 - (SA * SImp_territorial))] [set mortality_rate (1 - SA)] ;; if the individual is in a patch with "additional_mortality" set to "TRUE" the additional survival probability value (terriotial or non-territorial) is multipied to the baseline probability of survival
            if random-bernoulli (mortality_rate) [ ;; have a probability of dying equal to "mortality_rate"
              (ifelse
                sex = "M" [ ;; if it's a male
                  ask patch-here [set occupied_M False]] ;; ask the patch to set occupied_M False
                sex = "F" [ ;; if it's a female
                  ask patch-here [set occupied_F False]]) ;; ask the patch to set occupied_F False
              ifelse [additional_mortality] of patch-here = True [set N_deaths_T_with_AM N_deaths_T_with_AM + 1][set N_deaths_T_without_AM N_deaths_T_without_AM + 1]
              die]] ;; die
          territorial = False [ ;; if non-territorial
            let mortality_rate 0  ;; create the temporary variable "mortality_rate"
            ifelse [additional_mortality] of patch-here = True [set mortality_rate (1 - (SA * SImp_non-territorial))] [set mortality_rate (1 - SA)] ;; if the individual is in a patch with "additional_mortality" set to "TRUE" the additional survival probability value (terriotial or non-territorial) is multipied to the baseline probability of survival
            if random-bernoulli (mortality_rate) [ ;; have a probability of dying equal to "mortality_rate"
              ask patch-here [set non_territorials non_territorials - 1] ;; ask the patch where it is to subtract 1 from the non_territorials
              ifelse [additional_mortality] of patch-here = True [set N_deaths_NT_with_AM N_deaths_NT_with_AM + 1][set N_deaths_NT_without_AM N_deaths_NT_without_AM + 1]
              die]] ;; die
      )]

      age >= 20 [ ;; individuals aged 20
        (ifelse
          territorial = True [ ;; if territorial
            (ifelse
              sex = "M" [ ;; if it's a male
                ask patch-here [set occupied_M False]] ;; ask the patch to set occupied_M False
              sex = "F" [ ;; if it's a female
                ask patch-here [set occupied_F False]]) ;; ask the patch to set occupied_F False
            ifelse [additional_mortality] of patch-here = True [set N_deaths_T_with_AM N_deaths_T_with_AM + 1][set N_deaths_T_without_AM N_deaths_T_without_AM + 1]
            die] ;; die
          territorial = False [ ;; if non-territorial
            ask patch-here [set non_territorials non_territorials - 1] ;; ask the patch where it is to subtract 1 from the non_territorials
            ifelse [additional_mortality] of patch-here = True [set N_deaths_NT_with_AM N_deaths_NT_with_AM + 1][set N_deaths_NT_without_AM N_deaths_NT_without_AM + 1]
            die] ;; die
      )]

  )]

  set N_pairs_after_S count links
  set N_NT_after_S Count birds  with [territorial = False]
  set N_T_after_S Count birds  with [territorial = TRUE]

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  general procedures  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to Outputs_Collection

  set N_Total count birds

  set N_adults count birds  with [age > 4]

  set N_non_adults count birds  with [age < 5]

  set N_territorials count birds  with [territorial = True]

  set N_non_territorials count birds  with [territorial = False]

  if count birds  with [sex = "F"] = 0 or count birds  with [sex = "M"] = 0 [if Pop_persistence = 50 [set Pop_persistence time:get "year" sim_year]]

  if count birds  with [sex = "F"] > 0 and count birds  with [sex = "M"] > 0 [set N_final count birds ]


  carefully [set P_NT_AM (count birds  with [territorial = False and [additional_mortality] of patch-here = true]) / N_non_territorials][set P_NT_AM 0]
  carefully [set P_pairs_AM count (patches with [additional_mortality = True and occupied_M = true and occupied_F = true]) / count links] [set P_pairs_AM 0]
  carefully [set P_T_AM (count birds  with [territorial = True and [additional_mortality] of patch-here = true]) / N_territorials][set P_T_AM 0]

  carefully [set P_deaths_NT_AM N_deaths_NT_with_AM / (N_deaths_NT_with_AM + N_deaths_NT_without_AM)] [set P_deaths_NT_AM 0]
  carefully [set P_deaths_T_AM N_deaths_T_with_AM / (N_deaths_T_with_AM + N_deaths_T_without_AM)] [set P_deaths_T_AM 0]


  carefully [set list_NT_survival lput (N_NT_after_S / N_NT_before_S) list_NT_survival] []
  carefully [set list_T_survival lput (N_T_after_S / N_T_before_S) list_T_survival][]


  if time:get "year" sim_year = 50 [
    ifelse N_total > 0 [set Lambda (N_Total / N_initial)^(1 / 50)][
      ifelse Pop_persistence = 1 [set Lambda 0][
        set Lambda (N_final / N_initial)^(1 / (Pop_persistence - 1))]
    ]

    set mean_NT_survival mean list_NT_survival
    set mean_T_survival mean list_T_survival
  ]

end


to-report number_of_fledgers [fertility] ;; set the clutch size

  let result random-poisson fertility ;; defines a value based on a poisson distribution averaged over fertility
  if result > 3 [report number_of_fledgers fertility]  ;; check that the generated value is not greater than 3, if it is, recalculate
  report result  ;; reports the result generated that meets the previous conditions

end


to custom-bird

  set sex one-of ["M" "F"] ;; male or female
  ifelse (sex = "F")[set color pink][set color blue] ;; define color by sex: females = pink, males = blue
  set age 1 ;; age 1
  set age_class "fledgling" ;; age class "fledgling"
  set patch_type [p_type] of patch-here ;; set the "patch_type" variable as the p_type of the patch where the individual is
  set territorial False ;; without territory

end


to-report random-bernoulli [probability-True]  ;; function to define the occurrence or not of an action, according to a probability

  ifelse random-float 1.0 < probability-True  ;; generate random number between 0 and 1
    [report True][report False]  ;; if the value generated is less than the probability of occurrence it returns "True" otherwise it returns "False"

end
@#$#@#$#@
GRAPHICS-WINDOW
448
41
816
530
-1
-1
24.0
1
10
1
1
1
0
0
0
1
0
14
0
19
0
0
1
ticks
30.0

BUTTON
129
24
193
57
NIL
Setup
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
201
24
264
57
NIL
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

BUTTON
264
24
327
57
NIL
Go
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
124
80
181
125
Year
time:get \"year\" sim_year
17
1
11

INPUTBOX
281
170
378
230
%_B
16.5
1
0
Number

INPUTBOX
185
170
281
230
%_A
43.5
1
0
Number

SLIDER
109
376
350
409
Mortality_AM_NT
Mortality_AM_NT
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
109
409
350
442
Mortality_AM_T
Mortality_AM_T
0
1
0.0
0.01
1
NIL
HORIZONTAL

MONITOR
1045
218
1148
263
NIL
N_pairs_after_S
0
1
11

MONITOR
843
218
944
263
NIL
N_Adults
17
1
11

MONITOR
843
173
944
218
NIL
N_Non_Adults
17
1
11

MONITOR
944
218
1045
263
NIL
N_Territorials
17
1
11

MONITOR
944
173
1045
218
NIL
N_Non_Territorials
17
1
11

MONITOR
843
308
944
353
NIL
N_Total
17
1
11

MONITOR
1148
218
1248
263
NIL
Pop_persistence
17
1
11

INPUTBOX
185
271
289
331
A_AM
10.0
1
0
Number

TEXTBOX
162
142
354
160
Patch type percentage
15
0.0
1

TEXTBOX
90
245
396
263
Percentage of patches with additional mortality
15
0.0
1

TEXTBOX
130
350
342
368
Probability of additional mortality
15
0.0
1

TEXTBOX
961
145
1111
164
Output Parameters
15
0.0
1

INPUTBOX
81
271
185
331
Patches_AM
25.0
1
0
Number

MONITOR
1148
173
1248
218
NIL
N_emigrated
17
1
11

MONITOR
1045
173
1148
218
NIL
N_pairs_before_S
17
1
11

INPUTBOX
87
170
185
230
%_Unoccupiable
40.0
1
0
Number

INPUTBOX
289
271
393
331
B_AM
10.0
1
0
Number

MONITOR
944
308
1046
353
NIL
N_initial
17
1
11

MONITOR
843
263
915
308
NIL
P_pairs_AM
2
1
11

MONITOR
985
263
1059
308
NIL
P_T_AM
2
1
11

MONITOR
1059
263
1150
308
NIL
P_deaths_T_AM
2
1
11

MONITOR
1046
308
1149
353
NIL
N_final
17
1
11

MONITOR
915
263
985
308
NIL
P_NT_AM
2
1
11

MONITOR
1149
263
1248
308
NIL
P_deaths_NT_AM
2
1
11

INPUTBOX
341
466
391
526
FA
0.830328
1
0
Number

INPUTBOX
192
466
242
526
SA
0.937492
1
0
Number

INPUTBOX
42
466
92
526
S1
0.662946
1
0
Number

INPUTBOX
92
466
142
526
S23
0.719976
1
0
Number

INPUTBOX
142
466
192
526
S4
0.874833
1
0
Number

INPUTBOX
241
466
291
526
F2
0.285714
1
0
Number

INPUTBOX
291
466
341
526
F3
0.5
1
0
Number

MONITOR
1149
308
1248
353
NIL
Lambda
2
1
11

CHOOSER
195
79
352
124
Population
Population
"SPT" "CAT" "Sensitivity_Analysis"
0

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
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
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
