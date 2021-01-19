#' Sort Catch Numbers Into Species Categories
#'
#' This function uses FishEconProdOutput::itis_reclassify(), developed and maintained by Emily Markowitz, to create species category classifications and then sorts catch numbers into those classifications.
#'
#' @param commercial_data A data frame that includes catch numbers in dollars and TSN.
#' @param species_list A list of lists that includes categories at the top level and TSN numbers at the second level. Defaults to Comm.Catch.Spp.List.
#' @param year A numeric variable that can be used to filter for a specific year. Defaults to NA, which returns all years.
#' @importFrom magrittr %>%
#' @export
io_classifier <- function(commercial_data, species_list = Comm.Catch.Spp.List, year = NA){

  temp <- unique(commercial_data$TSN) %>% FishEconProdOutput::itis_reclassify(tsn = .,
                                                          categories = species_list,
                                                          missing_name = "Uncategorized")

  tsn_id = as.data.frame(temp[1][[1]])

  if (sum(tsn_id$category %in% c("Other", "Uncategorized"))>0) {
    tsn_id<-tsn_id[!(tsn_id$category %in% c("Other", "Uncategorized")),
                   c("TSN", "category")]
  }

  tsn_id$TSN<-as.numeric(as.character(tsn_id$TSN))

  commercial.data.merged<-dplyr::left_join(x = commercial.data,
                                           y = tsn_id,
                                           by = "TSN")

  commercial.data.out = commercial.data.merged %>%
    dplyr::filter(DOLLARS>0 & !is.na(DOLLARS) & !is.na(valid)) %>%
    dplyr::select(State, year, DOLLARS, fips, Region, category) %>%
    dplyr::group_by(Region, State, fips, year, category) %>%
    dplyr::summarize(dollars = sum(DOLLARS))

  if(!is.na(year)){
  commercial.data.out = commercial.data %>%
    dplyr::rename(`Species Category` = category, base_catch = dollars, Year = year) %>%
    dplyr::filter(Year == year)
  }

  return(commercial.data)

}

#' Run a Fisheries Input/Output Model
#'
#' This function takes in commercial fisheries catch numbers, IMPLAN multipliers, a GDP deflator, and imports numbers and outputs economic impacts.
#'
#' @param base_catch A data frame that details catch numbers at the state-species category level for a single year, including variables fips (FIPs number, 0 for US), spec_no (a numeric variable for species category), and base_catch (raw catch numbers in dollars).
#' @param imports A data frame that includes imports numbers in dollars at the state level for a single year, including fips (FIPs number, 0 for US) and imports.
#' @param multiplieArs A data frame that includes 17 multipliers at the state-species category-economic category for a single year. Defaults to David Records numbers.
#' @param deflator A numeric value that adjusts jobs numbers from the current year to the year the multipliers were made; defaults to 0.8734298, which represents 2017 to 2014.
#' @param imports_states A data frame that includes multipliers governing the percentages of imports going to each economic category for a single year. Defaults to 2017 numbers. Set to False for no imports.
#' @importFrom magrittr %>%
#' @export
io_calculator <- function(base_catch, imports, multipliers = multipliers, deflator = 0.8734298, imports_states = imports_states) {


  ############
  # Cleaning #
  ############

  imports = imports_states %>%
    dplyr::left_join(imports) %>%
    dplyr::mutate(imports = imports*value) %>%
    dplyr::select(fips, `Economic Category` = name, base_catch = imports) %>%
    dplyr::mutate(`Species Category` = "Imports")

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
      E_Direct_Impact = `Employment Direct Impacts` * base_catch * deflator,
      E_Indirect_Impact = `Employment Indirect Impacts` * `RPC RPC` *
        base_catch * deflator,
      E_Induced_Impact = `Employment Induced Impacts` * `RPC RPC` * base_catch * deflator,
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
      E_Direct_Impact = `Employment Direct Impacts` * processor_markup * deflator,
      E_Indirect_Impact = `Employment Indirect Impacts` * `RPC RPC` *
        processor_markup * deflator,
      E_Induced_Impact = `Employment Induced Impacts` * `RPC RPC` * processor_markup * deflator,
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
      E_Direct_Impact = `Employment Direct Impacts` * wholesaler_markup * deflator,
      E_Indirect_Impact = `Employment Indirect Impacts` * `RPC RPC` *
        wholesaler_markup * deflator,
      E_Induced_Impact = `Employment Induced Impacts` * `RPC RPC` * wholesaler_markup * deflator,
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
      E_Direct_Impact = `Employment Direct Impacts` * grocer_markup * deflator,
      E_Indirect_Impact = `Employment Indirect Impacts` * `RPC RPC` *
        grocer_markup * deflator,
      E_Induced_Impact = `Employment Induced Impacts` * `RPC RPC` * grocer_markup * deflator,
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
      E_Direct_Impact = `Employment Direct Impacts` * restaurant_markup * deflator,
      E_Indirect_Impact = `Employment Indirect Impacts` * `RPC RPC` *
        restaurant_markup * deflator,
      E_Induced_Impact = `Employment Induced Impacts` * `RPC RPC` * restaurant_markup * deflator,
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

  return(final_output)

}



#' Clean the Output From the IO Model
#'
#' This function presents the output from io_calculator() in a variety of specifiable formats.
#'
#' @param impacts A data frame that was output by io_calculator().
#' @param format A string variable with several options for specifying output format.
#' @param csv A boolean variable that exports the output as comma-separated value file(s) if True.
#' @export
io_cleaner <- function(impacts, format, csv){
  importFrom(magrittr,"%>%")

  if(format == "summary" | format == "all"){
    impacts_sum = impacts

    impacts_sum_imports = impacts_sum %>%
      dplyr::filter(`Species Category` == "Impacts")

  }

}

#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
