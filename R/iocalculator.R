#' Sort Catch Numbers Into Species Categories
#'
#' This function uses itis_reclassify(), developed by Emily Markowitz, to create species category classifications and then sorts catch numbers into those classifications.
#'
#' @param data A data frame that includes catch numbers in dollars and TSN.
#' @param list A list of lists that includes categories at the top level and TSN numbers at the second level. Defaults to Comm.Catch.Spp.List.
#' @param year A numeric variable that can be used to filter for a specific year. Defaults to NA, which returns all years.
#' @param recal A binary variable that determines whether the API should be queried or a default value used. Defaults to TRUE
#' @param tsn A dataset that can be used in place of the API call. Defaults to NULL
#' @importFrom magrittr %>%
#' @export
io_classifier <- function(data, species = Comm.Catch.Spp.List, year = NA, recall = T, tsn = NULL){

  commercial_data = data %>%
    dplyr::mutate(State = as.character(State), abbvst = as.character(abbvst), abbvreg = as.character(abbvreg))
  species_list = species

  if(recall == T){
    temp <- unique(commercial_data$TSN) %>% FishEconProdOutput::itis_reclassify(tsn = .,
                                                                                categories = species_list)
    tsn_id = as.data.frame(temp[1][[1]])

    temp2 <- unique(commercial_data$TSN) %>% FishEconProdOutput::itis_reclassify(tsn = .,
                                                                                 categories = list("Finfish" = c(914179, #  Infraphylum  Gnathostomata
                                                                                                                -914181))) # Tetrapoda; - = do NOT include
    tsn_id2 = as.data.frame(temp2[1][[1]]) %>% dplyr::rename(category2 = category) %>% dplyr::select(-valid, -rank, -sciname)

    tsn_id = tsn_id %>% full_join(tsn_id2)

  } else {

    tsn_id = tsn

  }

  tsn_id = tsn_id %>%
    #dplyr::filter(category != "Other" & category != "Uncategorized"  & category2 != "Other" & category2 != "Uncategorized") %>%
    dplyr::select(TSN, category, category2)

  tsn_id$TSN<-as.numeric(as.character(tsn_id$TSN))


  if(length(commercial_data$abbvst[commercial_data$abbvst=="WFL"])>0){
    commercial_data$State[commercial_data$abbvst=="EFL"]<-"Florida"
    commercial_data$Region[commercial_data$abbvst=="EFL"]<-"Gulf of Mexico"
    commercial_data$State[commercial_data$abbvst=="WFL"]<-"Florida"
    commercial_data$abbvst[commercial_data$abbvst=="WFL"]<-"FL"
    commercial_data$abbvst[commercial_data$abbvst=="EFL"]<-"FL"
  }

  commercial.data.merged<-dplyr::left_join(x = commercial_data,
                                           y = tsn_id,
                                           by = "TSN")


  commercial.data.out = commercial.data.merged %>%
    dplyr::mutate(category = case_when(
      is.na(category) ~ category2,
      category == "Other" & category2 == "Finfish" ~ category2,
      category == "Uncategorized" & category2 == "Finfish" ~ category2,
      TRUE ~ category
    )) %>%
    dplyr::select(-category2) %>%
    dplyr::filter(DOLLARS>0 & !is.na(DOLLARS) & !is.na(category)) %>%
    dplyr::select(State, year, DOLLARS, fips, Region, category) %>%
    dplyr::group_by(Region, State, fips, year, category) %>%
    dplyr::summarize(dollars = sum(DOLLARS))

  if(!is.na(year)){
    commercial.data.out = commercial.data.out %>%
      dplyr::rename(`Species Category` = category, base_catch = dollars, Year = year) %>%
      dplyr::filter(Year == year)
  }
  if(is.na(year)){
    commercial.data.out = commercial.data.out %>%
      dplyr::rename(`Species Category` = category, base_catch = dollars, Year = year)
  }

  commercial.data.out$`Species Category`[commercial.data.out$`Species Category`=="Finfish"]<-"All Other Finfish"

  return(commercial.data.out)

}


#' Run a Fisheries Input/Output Model
#'
#' This function takes in commercial fisheries catch numbers, IMPLAN multipliers, a GDP deflator, and imports numbers and outputs economic impacts.
#'
#' @param catch A data frame that details catch numbers at the state-species category level for a single year, including variables fips (FIPs number, 0 for US), spec_no (a numeric variable for species category), and base_catch (raw catch numbers in dollars).
#' @param import_numbers A data frame that includes imports numbers in dollars at the state level for a single year, including fips (FIPs number, 0 for US) and imports. Defaults to False for no imports.
#' @param implan_multipliers A data frame that includes 17 multipliers at the state-species category-economic category for a single year. Defaults to David Records numbers.
#' @param deflator A numeric value that adjusts jobs numbers from the current year to the year the multipliers were made; defaults to 0.8734298, which represents 2017 to 2014.
#' @param imports_state_multipliers A data frame that includes multipliers governing the percentages of imports going to each economic category for a single year. Defaults to 2017 numbers. Set to False for no imports.
#' @param manual A binary value indicating whether values were manually input. Defaults to FALSE
#' @importFrom magrittr %>%
#' @export
io_calculator <- function(catch, import_numbers = F, implan_multipliers = multipliers, deflator = 0.8734298, import_state_multipliers = imports_states, manual = FALSE) {

  base_catch = catch %>% dplyr::mutate(spec_no = dplyr::case_when(
    `Species Category` == "Shrimp" ~ 1,
    `Species Category` == "Crab" ~ 2,
    `Species Category` == "Lobster" ~ 3,
    `Species Category` == "East Coast Groundfish" ~ 4,
    `Species Category` == "HMS" ~ 5,
    `Species Category` == "Reef Fish" ~ 6,
    `Species Category` == "West Coast Groundfish " ~ 7,
    `Species Category` == "West Coast Groundfish" ~ 7,
    `Species Category` == "West Coast Whiting " ~ 8,
    `Species Category` == "West Coast Whiting" ~ 8,
    `Species Category` == "Halibut" ~ 9,
    `Species Category` == "Menhaden and Industrial" ~ 10,
    `Species Category` == "Salmon" ~ 11,
    `Species Category` == "Sea Scallop" ~ 12,
    `Species Category` == "Pelagic Herring and Mackerel" ~ 13,
    `Species Category` == "Surf Clam and Ocean Quahog " ~ 14,
    `Species Category` == "Surf Clam and Ocean Quahog" ~ 14,
    `Species Category` == "Surf Clam/Ocean Quahog" ~ 14,
    `Species Category` == "Other Trawl" ~ 15,
    `Species Category` == "All Other Finfish" ~ 16,
    `Species Category` == "All Other Shellfish  " ~ 17,
    `Species Category` == "All Other Shellfish" ~ 17,
    `Species Category` == "Freshwater " ~ 18,
    `Species Category` == "Freshwater" ~ 18,
    `Species Category` == "Inshore and Miscellaneous" ~ 19,
    `Species Category` == "Bait" ~ 20)) %>%
    dplyr::select(-`Species Category`)


US_base_catch = base_catch %>%
  select(base_catch, spec_no, Year) %>%
  group_by(spec_no, Year) %>%
  summarize(base_catch = sum(base_catch)) %>%
  mutate(Region = "National", State = "US", fips = 0)

base_catch = bind_rows(US_base_catch, base_catch)

  imports = import_numbers
  multipliers = implan_multipliers
  import_states = import_state_multipliers

  ############
  # Cleaning #
  ############

  if(!imports==F) {
    imports = imports_states %>%
      dplyr::left_join(imports) %>%
      dplyr::mutate(imports = imports * value) %>%
      dplyr::select(fips, `Economic Category` = name, base_catch = imports) %>%
      dplyr::mutate(`Species Category` = "Imports")
  }

  if(manual == TRUE){
    multipliers = multipliers %>%
      filter(fips %in% unique(base_catch$fips))
  }


  multipliers_harvesters = multipliers %>% dplyr::filter(`Economic Category` == "Harvesters")

  multipliers_processors = multipliers %>% dplyr::filter(`Economic Category` == "Processors")

  multipliers_wholesalers = multipliers %>% dplyr::filter(`Economic Category` == "Wholesalers")

  multipliers_grocers = multipliers %>% dplyr::filter(`Economic Category` == "Grocers")

  multipliers_restaurants = multipliers %>% dplyr::filter(`Economic Category` == "Restaurants")



  ##############
  # Harvesters #
  ##############

  multipliers_harvesters = multipliers_harvesters %>%
    dplyr::mutate(Species.Category = `Species Category`) %>%
    dplyr::left_join(base_catch, by = c("spec_no", "fips")) %>%
    dplyr::mutate(
      PI_Direct_Impact = `Personal Income Direct Impacts` * base_catch,
      PI_Indirect_Impact = `Personal Income Indirect Impacts` * `RPC RPC` *
        base_catch,
      PI_Induced_Impact = `Personal Income Induced Impacts` * `RPC RPC` *
        base_catch,
      PI_Total = PI_Direct_Impact + PI_Indirect_Impact + PI_Induced_Impact,
      TV_Direct_Impact = `Total Value Direct Impacts` * base_catch,
      TV_Indirect_Impact = `Total Value Indirect Impacts` * `RPC RPC` *
        base_catch,
      TV_Induced_Impact = `Total Value Induced Impacts` * `RPC RPC` *
        base_catch,
      TV_Total = TV_Direct_Impact + TV_Indirect_Impact + TV_Induced_Impact,
      O_Direct_Impact = `Output Direct Impacts` * base_catch,
      O_Indirect_Impact = `Output Indirect Impacts` * `RPC RPC` * base_catch,
      O_Induced_Impact = `Output Induced Impacts` * `RPC RPC` * base_catch,
      O_Total = O_Direct_Impact + O_Indirect_Impact + O_Induced_Impact,
      E_Direct_Impact = `Employment Direct Impacts` * base_catch * deflator / 1000,
      E_Indirect_Impact = `Employment Indirect Impacts` * `RPC RPC` *
        base_catch * deflator / 1000,
      E_Induced_Impact = `Employment Induced Impacts` * `RPC RPC` * base_catch * deflator / 1000,
      E_Total = E_Direct_Impact + E_Indirect_Impact + E_Induced_Impact
    )

  harvesters_output = multipliers_harvesters %>% dplyr::select(
    fips,
    `Economic Category`,
    `Species Category`,
    spec_no,
    PI_Direct_Impact,
    PI_Indirect_Impact,
    PI_Induced_Impact,
    PI_Total,
    TV_Direct_Impact,
    TV_Indirect_Impact,
    TV_Induced_Impact,
    TV_Total,
    O_Direct_Impact,
    O_Indirect_Impact,
    O_Induced_Impact,
    O_Total,
    E_Direct_Impact,
    E_Indirect_Impact,
    E_Induced_Impact,
    E_Total
  )



  ##############
  # Processors #
  ##############

  multipliers_processors = multipliers_processors %>%
    dplyr::mutate(Species.Category = `Species Category`) %>%
    dplyr::left_join(base_catch, by = c("spec_no", "fips")) %>%
    dplyr::mutate(processor_inputs = base_catch * Harvesters)

  if (imports != F) {
    for (n in unique(multipliers_processors$fips)) {
      multipliers_processors$processor_inputs[multipliers_processors$`Species Category` == "Imports"] <-
        imports$base_catch[imports$fips == n &
                             imports$`Economic Category` == "Processors"]
    }
  }

  multipliers_processors = multipliers_processors %>%
    dplyr::mutate(processor_markup = processor_inputs * markup) %>%
    dplyr::mutate(
      PI_Direct_Impact = `Personal Income Direct Impacts` * processor_markup,
      PI_Indirect_Impact = `Personal Income Indirect Impacts` * `RPC RPC` *
        processor_markup,
      PI_Induced_Impact = `Personal Income Induced Impacts` * `RPC RPC` *
        processor_markup,
      PI_Total = PI_Direct_Impact + PI_Indirect_Impact + PI_Induced_Impact,
      TV_Direct_Impact = `Total Value Direct Impacts` * processor_markup,
      TV_Indirect_Impact = `Total Value Indirect Impacts` * `RPC RPC` *
        processor_markup,
      TV_Induced_Impact = `Total Value Induced Impacts` * `RPC RPC` *
        processor_markup,
      TV_Total = TV_Direct_Impact + TV_Indirect_Impact + TV_Induced_Impact,
      O_Direct_Impact = `Output Direct Impacts` * processor_markup,
      O_Indirect_Impact = `Output Indirect Impacts` * `RPC RPC` * processor_markup,
      O_Induced_Impact = `Output Induced Impacts` * `RPC RPC` * processor_markup,
      O_Total = O_Direct_Impact + O_Indirect_Impact + O_Induced_Impact,
      E_Direct_Impact = `Employment Direct Impacts` * processor_markup * deflator / 1000,
      E_Indirect_Impact = `Employment Indirect Impacts` * `RPC RPC` *
        processor_markup * deflator / 1000,
      E_Induced_Impact = `Employment Induced Impacts` * `RPC RPC` * processor_markup * deflator / 1000,
      E_Total = E_Direct_Impact + E_Indirect_Impact + E_Induced_Impact
    )

  processors_output = multipliers_processors %>% dplyr::select(
    fips,
    `Economic Category`,
    `Species Category`,
    spec_no,
    PI_Direct_Impact,
    PI_Indirect_Impact,
    PI_Induced_Impact,
    PI_Total,
    TV_Direct_Impact,
    TV_Indirect_Impact,
    TV_Induced_Impact,
    TV_Total,
    O_Direct_Impact,
    O_Indirect_Impact,
    O_Induced_Impact,
    O_Total,
    E_Direct_Impact,
    E_Indirect_Impact,
    E_Induced_Impact,
    E_Total
  )

  processor_inputs = multipliers_processors %>% dplyr::select(fips, spec_no, processor_inputs, processor_markup)



  ###############
  # Wholesalers #
  ###############

  multipliers_wholesalers = multipliers_wholesalers %>%
    dplyr::mutate(Species.Category = `Species Category`) %>%
    dplyr::left_join(base_catch, by = c("spec_no", "fips")) %>%
    dplyr::left_join(processor_inputs, by = c("spec_no", "fips")) %>%
    dplyr::mutate(wholesaler_inputs = base_catch * Harvesters + Processors * (processor_inputs +
                                                                                processor_markup))
  if(imports != F) {
    for (n in unique(multipliers_wholesalers$fips)) {
      multipliers_wholesalers$wholesaler_inputs[multipliers_wholesalers$`Species Category` == "Imports"] <-
        imports$base_catch[imports$fips == n &
                             imports$`Economic Category` == "Wholesalers"]
    }
  }

  multipliers_wholesalers = multipliers_wholesalers %>%
    dplyr::mutate(wholesaler_markup = wholesaler_inputs * markup) %>%
    dplyr::mutate(
      PI_Direct_Impact = `Personal Income Direct Impacts` * wholesaler_markup,
      PI_Indirect_Impact = `Personal Income Indirect Impacts` * `RPC RPC` *
        wholesaler_markup,
      PI_Induced_Impact = `Personal Income Induced Impacts` * `RPC RPC` *
        wholesaler_markup,
      PI_Total = PI_Direct_Impact + PI_Indirect_Impact + PI_Induced_Impact,
      TV_Direct_Impact = `Total Value Direct Impacts` * wholesaler_markup,
      TV_Indirect_Impact = `Total Value Indirect Impacts` * `RPC RPC` *
        wholesaler_markup,
      TV_Induced_Impact = `Total Value Induced Impacts` * `RPC RPC` *
        wholesaler_markup,
      TV_Total = TV_Direct_Impact + TV_Indirect_Impact + TV_Induced_Impact,
      O_Direct_Impact = `Output Direct Impacts` * wholesaler_markup,
      O_Indirect_Impact = `Output Indirect Impacts` * `RPC RPC` * wholesaler_markup,
      O_Induced_Impact = `Output Induced Impacts` * `RPC RPC` * wholesaler_markup,
      O_Total = O_Direct_Impact + O_Indirect_Impact + O_Induced_Impact,
      E_Direct_Impact = `Employment Direct Impacts` * wholesaler_markup * deflator / 1000,
      E_Indirect_Impact = `Employment Indirect Impacts` * `RPC RPC` *
        wholesaler_markup * deflator / 1000,
      E_Induced_Impact = `Employment Induced Impacts` * `RPC RPC` * wholesaler_markup * deflator / 1000,
      E_Total = E_Direct_Impact + E_Indirect_Impact + E_Induced_Impact
    )

  wholesalers_output = multipliers_wholesalers %>% dplyr::select(
    fips,
    `Economic Category`,
    `Species Category`,
    spec_no,
    PI_Direct_Impact,
    PI_Indirect_Impact,
    PI_Induced_Impact,
    PI_Total,
    TV_Direct_Impact,
    TV_Indirect_Impact,
    TV_Induced_Impact,
    TV_Total,
    O_Direct_Impact,
    O_Indirect_Impact,
    O_Induced_Impact,
    O_Total,
    E_Direct_Impact,
    E_Indirect_Impact,
    E_Induced_Impact,
    E_Total
  )

  wholesaler_inputs = multipliers_wholesalers %>% dplyr::select(fips,
                                                                spec_no,
                                                                wholesaler_inputs,
                                                                wholesaler_markup)


  ###########
  # Grocers #
  ###########

  multipliers_grocers = multipliers_grocers %>%
    dplyr::mutate(Species.Category = `Species Category`) %>%
    dplyr::left_join(base_catch, by = c("spec_no", "fips")) %>%
    dplyr::left_join(processor_inputs, by = c("spec_no", "fips")) %>%
    dplyr::left_join(wholesaler_inputs, by = c("spec_no", "fips")) %>%
    dplyr::mutate(
      grocer_inputs = base_catch * Harvesters + Processors * (processor_inputs +
                                                                processor_markup) + Wholesalers * (wholesaler_inputs + wholesaler_markup)
    )

  if (imports != F) {
    for (n in unique(multipliers_grocers$fips)) {
      multipliers_grocers$grocer_inputs[multipliers_grocers$`Species Category` == "Imports"] <-
        imports$base_catch[imports$fips == n &
                             imports$`Economic Category` == "Grocers"]
    }
  }

  multipliers_grocers = multipliers_grocers %>%
    dplyr::mutate(grocer_markup = grocer_inputs * markup) %>%
    dplyr::mutate(
      PI_Direct_Impact = `Personal Income Direct Impacts` * grocer_markup,
      PI_Indirect_Impact = `Personal Income Indirect Impacts` * `RPC RPC` *
        grocer_markup,
      PI_Induced_Impact = `Personal Income Induced Impacts` * `RPC RPC` *
        grocer_markup,
      PI_Total = PI_Direct_Impact + PI_Indirect_Impact + PI_Induced_Impact,
      TV_Direct_Impact = `Total Value Direct Impacts` * grocer_markup,
      TV_Indirect_Impact = `Total Value Indirect Impacts` * `RPC RPC` *
        grocer_markup,
      TV_Induced_Impact = `Total Value Induced Impacts` * `RPC RPC` *
        grocer_markup,
      TV_Total = TV_Direct_Impact + TV_Indirect_Impact + TV_Induced_Impact,
      O_Direct_Impact = `Output Direct Impacts` * grocer_markup,
      O_Indirect_Impact = `Output Indirect Impacts` * `RPC RPC` * grocer_markup,
      O_Induced_Impact = `Output Induced Impacts` * `RPC RPC` * grocer_markup,
      O_Total = O_Direct_Impact + O_Indirect_Impact + O_Induced_Impact,
      E_Direct_Impact = `Employment Direct Impacts` * grocer_markup * deflator / 1000,
      E_Indirect_Impact = `Employment Indirect Impacts` * `RPC RPC` *
        grocer_markup * deflator / 1000,
      E_Induced_Impact = `Employment Induced Impacts` * `RPC RPC` * grocer_markup * deflator / 1000,
      E_Total = E_Direct_Impact + E_Indirect_Impact + E_Induced_Impact
    )

  grocers_output = multipliers_grocers %>% dplyr::select(
    fips,
    `Economic Category`,
    `Species Category`,
    spec_no,
    PI_Direct_Impact,
    PI_Indirect_Impact,
    PI_Induced_Impact,
    PI_Total,
    TV_Direct_Impact,
    TV_Indirect_Impact,
    TV_Induced_Impact,
    TV_Total,
    O_Direct_Impact,
    O_Indirect_Impact,
    O_Induced_Impact,
    O_Total,
    E_Direct_Impact,
    E_Indirect_Impact,
    E_Induced_Impact,
    E_Total
  )

  grocer_inputs = multipliers_grocers %>% dplyr::select(fips, spec_no, grocer_inputs, grocer_markup)



  ###############
  # Restaurants #
  ###############

  multipliers_restaurants = multipliers_restaurants %>%
    dplyr::mutate(Species.Category = `Species Category`) %>%
    dplyr::left_join(base_catch, by = c("spec_no", "fips")) %>%
    dplyr::left_join(processor_inputs, by = c("spec_no", "fips")) %>%
    dplyr::left_join(wholesaler_inputs, by = c("spec_no", "fips")) %>%
    dplyr::left_join(grocer_inputs, by = c("spec_no", "fips")) %>%
    dplyr::mutate(
      restaurant_inputs = base_catch * Harvesters + Processors * (processor_inputs +
                                                                    processor_markup) + Wholesalers * (wholesaler_inputs + wholesaler_markup) + Grocers *
        (grocer_inputs + grocer_markup)
    )

  if(imports != F) {
    for (n in unique(multipliers_restaurants$fips)) {
      multipliers_restaurants$restaurants_inputs[multipliers_restaurants$`Species Category` == "Imports"] <-
        imports$base_catch[imports$fips == n &
                             imports$`Economic Category` == "Restaurants"]
    }
  }

  multipliers_restaurants = multipliers_restaurants %>%
    dplyr::mutate(restaurant_markup = restaurant_inputs * markup) %>%
    dplyr::mutate(
      PI_Direct_Impact = `Personal Income Direct Impacts` * restaurant_markup,
      PI_Indirect_Impact = `Personal Income Indirect Impacts` * `RPC RPC` *
        restaurant_markup,
      PI_Induced_Impact = `Personal Income Induced Impacts` * `RPC RPC` *
        restaurant_markup,
      PI_Total = PI_Direct_Impact + PI_Indirect_Impact + PI_Induced_Impact,
      TV_Direct_Impact = `Total Value Direct Impacts` * restaurant_markup,
      TV_Indirect_Impact = `Total Value Indirect Impacts` * `RPC RPC` *
        restaurant_markup,
      TV_Induced_Impact = `Total Value Induced Impacts` * `RPC RPC` *
        restaurant_markup,
      TV_Total = TV_Direct_Impact + TV_Indirect_Impact + TV_Induced_Impact,
      O_Direct_Impact = `Output Direct Impacts` * restaurant_markup,
      O_Indirect_Impact = `Output Indirect Impacts` * `RPC RPC` * restaurant_markup,
      O_Induced_Impact = `Output Induced Impacts` * `RPC RPC` * restaurant_markup,
      O_Total = O_Direct_Impact + O_Indirect_Impact + O_Induced_Impact,
      E_Direct_Impact = `Employment Direct Impacts` * restaurant_markup * deflator / 1000,
      E_Indirect_Impact = `Employment Indirect Impacts` * `RPC RPC` *
        restaurant_markup * deflator / 1000,
      E_Induced_Impact = `Employment Induced Impacts` * `RPC RPC` * restaurant_markup * deflator / 1000,
      E_Total = E_Direct_Impact + E_Indirect_Impact + E_Induced_Impact
    )

  restaurants_output = multipliers_restaurants %>% dplyr::select(
    fips,
    `Economic Category`,
    `Species Category`,
    spec_no,
    PI_Direct_Impact,
    PI_Indirect_Impact,
    PI_Induced_Impact,
    PI_Total,
    TV_Direct_Impact,
    TV_Indirect_Impact,
    TV_Induced_Impact,
    TV_Total,
    O_Direct_Impact,
    O_Indirect_Impact,
    O_Induced_Impact,
    O_Total,
    E_Direct_Impact,
    E_Indirect_Impact,
    E_Induced_Impact,
    E_Total
  )


  ################
  # Final Output #
  ################

  final_output = dplyr::bind_rows(
    harvesters_output,
    processors_output,
    wholesalers_output,
    grocers_output,
    restaurants_output
  )
  final_output[is.na(final_output)] <- 0

  if(imports==F){
    final_output = final_output %>%
      dplyr::mutate(Imports = "No")
  }
  if(imports!=F){
    final_output = final_output %>%
      dplyr::mutate(Imports = "Yes")
  }

  return(final_output)

}



#' Clean the Output From the IO Model
#'
#' This function presents the output from io_calculator() in a variety of specifiable formats.
#'
#' @param impact A data frame that was output by io_calculator().
#' @param format A string variable with several options for specifying output format.
#' @param xlsx A boolean variable that exports the output as tabs in an xlsx file if True.
#' @param fp A data frame containing state names and fips codes. Defaults to standard with EFL = 12 and WFL = 12.5.
#' @param maxyr The catch year - only used for FEUS.
#' @importFrom magrittr %>%
#' @export
io_cleaner <- function(impact, format = "summary", xlsx = F, fp = fips, maxyr = 2018) {
  output = c()
  fips = fp

  impacts = impact %>%
    tidyr::pivot_longer(PI_Direct_Impact:E_Total,
                        names_to = "names",
                        values_to = "values") %>%
    dplyr::mutate(
      Impact_Type = dplyr::case_when(
        stringr::str_detect(names, "PI_") ~ "Income Impacts",
        stringr::str_detect(names, "TV_") ~ "Total Value Added",
        stringr::str_detect(names, "O_") ~ "Output Impacts",
        stringr::str_detect(names, "E_") ~ "Employment Impacts"
      )
    ) %>%
    dplyr::mutate(
      Group = dplyr::case_when(
        stringr::str_detect(names, "Direct_Impact") ~ "Direct",
        stringr::str_detect(names, "Indirect_Impact") ~ "Indirect",
        stringr::str_detect(names, "Induced_Impact") ~ "Induced",
        stringr::str_detect(names, "Total") ~ "Total"
      )
    ) %>%
    dplyr::select(-names) %>%
    tidyr::pivot_wider(
      id_cols = c(
        fips,
        `Economic Category`,
        `Species Category`,
        spec_no,
        Impact_Type,
        Group,
        Imports
      ),
      names_from = Group,
      values_from = values
    ) %>%
    dplyr::mutate(`Economic Category` = factor(
      `Economic Category`,
      levels = c(
        "Harvesters",
        "Processors",
        "Wholesalers",
        "Grocers",
        "Restaurants"
      )
    )) %>%
    dplyr::arrange(fips, `Economic Category`, Impact_Type, spec_no)

  if (format == "national" | format == "all") {
    impacts_national = impacts %>% dplyr::filter(fips == 0)

    impacts_national_imports = impacts_national %>%
      dplyr::filter(spec_no == 0) %>%
      dplyr::group_by(`Economic Category`, Impact_Type) %>%
      dplyr::summarize(
        Direct = sum(Direct),
        Indirect = sum(Indirect),
        Induced = sum(Induced),
        Total = sum(Total)
      ) %>%
      dplyr::arrange(`Economic Category`, Impact_Type) %>%
      dplyr::mutate(`Economic Category` = "Imports and Brokers") %>%
      dplyr::group_by(`Economic Category`, Impact_Type) %>%
      dplyr::summarize(
        Direct = sum(Direct),
        Indirect = sum(Indirect),
        Induced = sum(Induced),
        Total = sum(Total)
      )

    impacts_national = impacts_national %>%
      dplyr::group_by(`Economic Category`, Impact_Type) %>%
      dplyr::summarize(
        Direct = sum(Direct),
        Indirect = sum(Indirect),
        Induced = sum(Induced),
        Total = sum(Total)
      ) %>%
      dplyr::arrange(`Economic Category`, Impact_Type)

    impacts_national_seafood = impacts_national %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Impact_Type) %>%
      dplyr::summarize(
        Direct = sum(Direct),
        Indirect = sum(Indirect),
        Induced = sum(Induced),
        Total = sum(Total)
      ) %>%
      dplyr::arrange(Impact_Type) %>%
      dplyr::mutate(`Economic Category` = "Harvesters and Seafood Industry")


    impacts_national_out = impacts_national %>%
      dplyr::bind_rows(impacts_national_imports) %>%
      dplyr::bind_rows(impacts_national_seafood)

    output = append(output, list("national_impacts" = impacts_national_out))

  }

  if (format == "state summary" | format == "all") {
    impacts_state_sum = impacts %>% dplyr::filter(fips != 0)

    impacts_state_sum_imports = impacts_state_sum %>%
      dplyr::filter(spec_no == 0) %>%
      dplyr::group_by(`Economic Category`, Impact_Type) %>%
      dplyr::summarize(
        Direct = sum(Direct),
        Indirect = sum(Indirect),
        Induced = sum(Induced),
        Total = sum(Total)
      ) %>%
      dplyr::arrange(`Economic Category`, Impact_Type) %>%
      dplyr::mutate(`Economic Category` = "Imports and Brokers") %>%
      dplyr::group_by(`Economic Category`, Impact_Type) %>%
      dplyr::summarize(
        Direct = sum(Direct),
        Indirect = sum(Indirect),
        Induced = sum(Induced),
        Total = sum(Total)
      )

    impacts_state_sum = impacts_state_sum %>%
      dplyr::group_by(`Economic Category`, Impact_Type) %>%
      dplyr::summarize(
        Direct = sum(Direct),
        Indirect = sum(Indirect),
        Induced = sum(Induced),
        Total = sum(Total)
      ) %>%
      dplyr::arrange(`Economic Category`, Impact_Type)

    impacts_state_sum_seafood = impacts_state_sum %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Impact_Type) %>%
      dplyr::summarize(
        Direct = sum(Direct),
        Indirect = sum(Indirect),
        Induced = sum(Induced),
        Total = sum(Total)
      ) %>%
      dplyr::arrange(Impact_Type) %>%
      dplyr::mutate(`Economic Category` = "Harvesters and Seafood Industry")


    impacts_state_sum_out = impacts_state_sum %>%
      dplyr::bind_rows(impacts_state_sum_imports) %>%
      dplyr::bind_rows(impacts_state_sum_seafood)

    output = append(output, list("state_impacts" = impacts_state_sum_out))

  }



  if (format == "sector" | format == "all") {
    for (n in unique(impacts$`Economic Category`)) {
      impacts_sum = impacts %>%
        dplyr::filter(`Economic Category` == n)

      if (length(impacts_sum$fips) > 0) {
        impacts_sum = impacts_sum %>%
          dplyr::group_by(spec_no, Impact_Type) %>%
          dplyr::summarize(
            Direct = sum(Direct),
            Indirect = sum(Indirect),
            Induced = sum(Induced),
            Total = sum(Total)
          ) %>%
          dplyr::arrange(spec_no, Impact_Type)


        assign(paste0("impacts_sum_", n), impacts_sum)

        temp = list(get(paste0("impacts_sum_", n)))
        names(temp) <- paste0("impacts_sum_", n)
        output = append(output, temp)
      }
    }
  }

  if (format == "states" | format == "all") {
    for (n in 1:length(fp$fips)) {
      impacts_sum = impacts %>%
        dplyr::filter(fips == fp$fips[n])

      if (length(impacts_sum$fips) > 0 & impacts_sum$fips[1] != 0) {
        impacts_sum_imports = impacts_sum %>%
          dplyr::filter(spec_no == 0) %>%
          dplyr::group_by(`Economic Category`, Impact_Type) %>%
          dplyr::summarize(
            Direct = sum(Direct),
            Indirect = sum(Indirect),
            Induced = sum(Induced),
            Total = sum(Total)
          ) %>%
          dplyr::arrange(`Economic Category`, Impact_Type) %>%
          dplyr::mutate(`Economic Category` = "Imports and Brokers")

        impacts_sum = impacts_sum %>%
          dplyr::group_by(`Economic Category`, Impact_Type) %>%
          dplyr::summarize(
            Direct = sum(Direct),
            Indirect = sum(Indirect),
            Induced = sum(Induced),
            Total = sum(Total)
          ) %>%
          dplyr::arrange(`Economic Category`, Impact_Type)

        impacts_sum_seafood = impacts_sum %>%
          dplyr::ungroup() %>%
          dplyr::group_by(Impact_Type) %>%
          dplyr::summarize(
            Direct = sum(Direct),
            Indirect = sum(Indirect),
            Induced = sum(Induced),
            Total = sum(Total)
          ) %>%
          dplyr::arrange(Impact_Type) %>%
          dplyr::mutate(`Economic Category` = "Harvesters and Seafood Industry")


        impacts_sum %>%
          dplyr::bind_rows(impacts_sum_imports) %>%
          dplyr::bind_rows(impacts_sum_seafood)

        assign(paste0("impacts_sum_", fips$state_abbr[n]), impacts_sum)

        temp = list(get(paste0("impacts_sum_", fips$state_abbr[n])))
        names(temp) <- paste0("impacts_sum_", fips$state_abbr[n])
        output = append(output, temp)
      }
    }
  }

  if(format == "impact" | format == "all"){
    for (n in unique(impacts$Impact_Type)) {
      impacts_sum = impacts %>%
        dplyr::filter(Impact_Type == n & fips != 0)

      if (length(impacts_sum$fips) > 0) {
        impacts_sum = impacts_sum %>%
          dplyr::group_by(`Economic Category`, spec_no) %>%
          dplyr::summarize(
            Direct = sum(Direct),
            Indirect = sum(Indirect),
            Induced = sum(Induced),
            Total = sum(Total)
          ) %>%
          dplyr::arrange(`Economic Category`, spec_no)

        assign(paste0("impacts_sum_", stringr::str_replace_all(n, " ", "_")), impacts_sum)

        temp = list(get(paste0("impacts_sum_", stringr::str_replace_all(n, " ", "_"))))
        names(temp) <- paste0("impacts_sum_", stringr::str_replace_all(n, " ", "_"))
        output = append(output, temp)
      }
    }
  }

  if(format == "FEUS" | format == "Manual"){
    impacts.imports.states = impacts %>%
      dplyr::filter(spec_no == 0 & fips != 0) %>%
      dplyr::group_by(fips, `Economic Category`, Impact_Type, Imports) %>%
      dplyr::summarize(Direct = sum(Direct),
                       Indirect = sum(Indirect),
                       Induced = sum(Induced),
                       Total = sum(Total)) %>%
      dplyr::left_join(fips) %>%
      dplyr::ungroup() %>%
      dplyr::select(-fips) %>%
      dplyr::rename(Metric = `Economic Category`, Sector = Impact_Type, State1 = state_abbr) %>%
      dplyr::mutate(Year = maxyr,
                    Direct = Direct/1000,
                    Indirect = Indirect/1000,
                    Induced = Induced/1000,
                    Total = Total/1000) %>%
      dplyr::mutate(Metric = "Importers")  %>%
      dplyr::group_by(Metric, Sector, Imports, State1, Year) %>%
      dplyr::summarize(Direct = sum(Direct),
                       Indirect = sum(Indirect),
                       Induced = sum(Induced),
                       Total = sum(Total))

    impacts.imports.us <- impacts %>%
      dplyr::filter(spec_no == 0 & fips == 0) %>%
      dplyr::group_by(fips, `Economic Category`, Impact_Type, Imports) %>%
      dplyr::summarize(Direct = sum(Direct),
                       Indirect = sum(Indirect),
                       Induced = sum(Induced),
                       Total = sum(Total)) %>%
      dplyr::left_join(fips) %>%
      dplyr::ungroup() %>%
      dplyr::select(-fips) %>%
      dplyr::rename(Metric = `Economic Category`, Sector = Impact_Type, State1 = state_abbr) %>%
      dplyr::mutate(Year = maxyr,
                    Direct = Direct/1000,
                    Indirect = Indirect/1000,
                    Induced = Induced/1000,
                    Total = Total/1000) %>%
      dplyr::mutate(Metric = "Importers")  %>%
      dplyr::group_by(Metric, Sector, Imports, State1, Year) %>%
      dplyr::summarize(Direct = sum(Direct),
                       Indirect = sum(Indirect),
                       Induced = sum(Induced),
                       Total = sum(Total)) %>%
      dplyr::mutate(State1 = "US")


    impacts.states = impacts %>%
      dplyr::filter(fips != 0) %>%
      dplyr::group_by(fips, `Economic Category`, Impact_Type, Imports) %>%
      dplyr::summarize(Direct = sum(Direct),
                       Indirect = sum(Indirect),
                       Induced = sum(Induced),
                       Total = sum(Total)) %>%
      dplyr::left_join(fips) %>%
      dplyr::ungroup() %>%
      dplyr::select(-fips) %>%
      dplyr::rename(Metric = `Economic Category`, Sector = Impact_Type, State1 = state_abbr) %>%
      dplyr::mutate(Year = maxyr,
                    Direct = Direct/1000,
                    Indirect = Indirect/1000,
                    Induced = Induced/1000,
                    Total = Total/1000)

    impacts.us = impacts %>%
      dplyr::filter(fips == 0) %>%
      dplyr::group_by(fips, `Economic Category`, Impact_Type, Imports) %>%
      dplyr::summarize(Direct = sum(Direct),
                       Indirect = sum(Indirect),
                       Induced = sum(Induced),
                       Total = sum(Total)) %>%
      dplyr::left_join(fips) %>%
      dplyr::ungroup() %>%
      dplyr::select(-fips) %>%
      dplyr::rename(Metric = `Economic Category`, Sector = Impact_Type, State1 = state_abbr) %>%
      dplyr::mutate(Year = maxyr,
                    Direct = Direct/1000,
                    Indirect = Indirect/1000,
                    Induced = Induced/1000,
                    Total = Total/1000)

    impacts.allsectors.states = impacts.states %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Sector, Imports, Year, State1) %>%
      dplyr::summarize(Direct = sum(Direct),
                       Indirect = sum(Indirect),
                       Induced = sum(Induced),
                       Total = sum(Total)) %>%
      dplyr::mutate(Metric = "Total Impacts")

    impacts.allsectors.us = impacts.us %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Metric, Sector, Imports, Year) %>%
      dplyr::summarize(Direct = sum(Direct),
                       Indirect = sum(Indirect),
                       Induced = sum(Induced),
                       Total = sum(Total)) %>%
      dplyr::mutate(State1 = "US")

    output = dplyr::bind_rows(impacts.states, impacts.us, impacts.imports.states, impacts.imports.us, impacts.allsectors.states, impacts.allsectors.us) %>%
      dplyr::mutate(Index_Local = row_number())
  }

  if(xlsx == F){
    return(output)
  }

  if(xlsx == T){
    dir = getwd()
    for(n in 1:length(output)){
      xlsx::write.xlsx(output[n], file = paste0(dir, "/",output,"_impacts.xlsx"), sheetName = names(output)[n], append = T, row.names = F)
    }
    return(output)
  }
}


#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
