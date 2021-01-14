# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

iocalculator <- function(base_catch, multipliers, deflator = 0.8734298, imports) {
  multipliers = multipliers %>%
    left_join(spec_no)
  base_catch = base_catch %>% mutate(spec_no = case_when(
    `Species Category` == "Shrimp" ~ 1,
    `Species Category` == "Crab" ~ 2,
    `Species Category` == "Lobster" ~ 3,
    `Species Category` == "East Coast Groundfish" ~ 4,
    `Species Category` == "HMS" ~ 5,
    `Species Category` == "Reef Fish" ~ 6,
    `Species Category` == "West Coast Groundfish " ~ 7,
    `Species Category` == "West Coast Whiting " ~ 8,
    `Species Category` == "Halibut" ~ 9,
    `Species Category` == "Menhaden and Industrial" ~ 10,
    `Species Category` == "Salmon" ~ 11,
    `Species Category` == "Sea Scallop" ~ 12,
    `Species Category` == "Pelagic Herring and Mackerel" ~ 13,
    `Species Category` == "Surf Clam and Ocean Quahog " ~ 14,
    `Species Category` == "Other Trawl" ~ 15,
    `Species Category` == "All Other Finfish" ~ 16,
    `Species Category` == "All Other Shellfish  " ~ 17,
    `Species Category` == "Freshwater " ~ 18,
    `Species Category` == "Inshore and Miscellaneous" ~ 19,
    `Species Category` == "Bait" ~ 20)) %>%
    select(-`Species Category`)
  multipliers_harvesters = multipliers %>% filter(`Economic Category` == "Harvesters")
  multipliers_processors = multipliers %>% filter(`Economic Category` == "Processors")
  multipliers_wholesalers = multipliers %>% filter(`Economic Category` == "Wholesalers")
  multipliers_grocers = multipliers %>% filter(`Economic Category` == "Grocers")
  multipliers_restaurants = multipliers %>% filter(`Economic Category` == "Restaurants")

  multipliers_harvesters = multipliers_harvesters %>%
    mutate(Species.Category = `Species Category`) %>%
    left_join(base_catch, by = c("spec_no", "fips")) %>%
    mutate(
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

  harvesters_output = multipliers_harvesters %>% select(
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


  multipliers_processors = multipliers_processors %>%
    mutate(Species.Category = `Species Category`) %>%
    left_join(base_catch, by = c("spec_no", "fips")) %>%
    mutate(processor_inputs = base_catch * Harvesters)

  if (imports != F) {
    for (n in unique(multipliers_processors$fips)) {
      multipliers_processors$processor_inputs[multipliers_processors$`Species Category` == "Imports"] <-
        imports$base_catch[imports$fips == n &
                             imports$`Economic Category` == "Processors"]
    }
  }
  multipliers_processors = multipliers_processors %>%
    mutate(processor_markup = processor_inputs * markup) %>%
    mutate(
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

  processors_output = multipliers_processors %>% select(
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

  processor_inputs = multipliers_processors %>% select(fips, spec_no, processor_inputs, processor_markup)

  multipliers_wholesalers = multipliers_wholesalers %>%
    mutate(Species.Category = `Species Category`) %>%
    left_join(base_catch, by = c("spec_no", "fips")) %>%
    left_join(processor_inputs, by = c("spec_no", "fips")) %>%
    mutate(wholesaler_inputs = base_catch * Harvesters + Processors * (processor_inputs +
                                                                         processor_markup))
  if(imports != F) {
    for (n in unique(multipliers_wholesalers$fips)) {
      multipliers_wholesalers$wholesaler_inputs[multipliers_wholesalers$`Species Category` == "Imports"] <-
        imports$base_catch[imports$fips == n &
                             imports$`Economic Category` == "Wholesalers"]
    }
  }

  multipliers_wholesalers = multipliers_wholesalers %>%
    mutate(wholesaler_markup = wholesaler_inputs * markup) %>%
    mutate(
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

  wholesalers_output = multipliers_wholesalers %>% select(
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

  wholesaler_inputs = multipliers_wholesalers %>% select(fips,
                                                         spec_no,
                                                         wholesaler_inputs,
                                                         wholesaler_markup)

  multipliers_grocers = multipliers_grocers %>%
    mutate(Species.Category = `Species Category`) %>%
    left_join(base_catch, by = c("spec_no", "fips")) %>%
    left_join(processor_inputs, by = c("spec_no", "fips")) %>%
    left_join(wholesaler_inputs, by = c("spec_no", "fips")) %>%
    mutate(
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
    mutate(grocer_markup = grocer_inputs * markup) %>%
    mutate(
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

  grocers_output = multipliers_grocers %>% select(
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

  grocer_inputs = multipliers_grocers %>% select(fips, spec_no, grocer_inputs, grocer_markup)

  multipliers_restaurants = multipliers_restaurants %>%
    mutate(Species.Category = `Species Category`) %>%
    left_join(base_catch, by = c("spec_no", "fips")) %>%
    left_join(processor_inputs, by = c("spec_no", "fips")) %>%
    left_join(wholesaler_inputs, by = c("spec_no", "fips")) %>%
    left_join(grocer_inputs, by = c("spec_no", "fips")) %>%
    mutate(
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
    mutate(restaurant_markup = restaurant_inputs * markup) %>%
    mutate(
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

  restaurants_output = multipliers_restaurants %>% select(
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

  final_output = bind_rows(
    harvesters_output,
    processors_output,
    wholesalers_output,
    grocers_output,
    restaurants_output
  )
  final_output[is.na(final_output)] <- 0
  output_sum_spec = final_output %>%
    group_by(fips, `Economic Category`) %>%
    summarize(
      `Species Category` = "All",
      PI_Direct_Impact = sum(PI_Direct_Impact),
      PI_Indirect_Impact = sum(PI_Indirect_Impact),
      PI_Induced_Impact = sum(PI_Induced_Impact),
      PI_Total = sum(PI_Total),
      TV_Direct_Impact = sum(TV_Direct_Impact),
      TV_Indirect_Impact = sum(TV_Indirect_Impact),
      TV_Induced_Impact = sum(TV_Induced_Impact),
      TV_Total = sum(TV_Total),
      O_Direct_Impact = sum(O_Direct_Impact),
      O_Indirect_Impact = sum(O_Indirect_Impact),
      O_Induced_Impact = sum(O_Induced_Impact),
      O_Total = sum(O_Total),
      E_Direct_Impact = sum(E_Direct_Impact),
      E_Indirect_Impact = sum(E_Indirect_Impact),
      E_Induced_Impact = sum(E_Induced_Impact),
      E_Total = sum(E_Total)
    )
  output_sum_total = output_sum_spec %>%
    ungroup() %>%
    group_by(fips) %>%
    summarize(
      `Economic Category` = "All",
      `Species Category` = "All",
      PI_Direct_Impact = sum(PI_Direct_Impact),
      PI_Indirect_Impact = sum(PI_Indirect_Impact),
      PI_Induced_Impact = sum(PI_Induced_Impact),
      PI_Total = sum(PI_Total),
      TV_Direct_Impact = sum(TV_Direct_Impact),
      TV_Indirect_Impact = sum(TV_Indirect_Impact),
      TV_Induced_Impact = sum(TV_Induced_Impact),
      TV_Total = sum(TV_Total),
      O_Direct_Impact = sum(O_Direct_Impact),
      O_Indirect_Impact = sum(O_Indirect_Impact),
      O_Induced_Impact = sum(O_Induced_Impact),
      O_Total = sum(O_Total),
      E_Direct_Impact = sum(E_Direct_Impact),
      E_Indirect_Impact = sum(E_Indirect_Impact),
      E_Induced_Impact = sum(E_Induced_Impact),
      E_Total = sum(E_Total)
    )

  final_output = bind_rows(final_output, output_sum_spec, output_sum_total)

  return(final_output)

}
