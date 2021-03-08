#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

list_of_packages = c("magrittr", "readr", "readxl", "dplyr", "DT", "shinycssloaders", "shinyjs", "fredr", "writexl")

lapply(list_of_packages, function(x) if(!require(x,character.only = TRUE)) install.packages(x))

ui <- fluidPage(

  shinyjs::useShinyjs(),
  # Application title
  titlePanel("Fisheries Input/Output Model"),

  # Sidebar
  sidebarLayout(

    sidebarPanel(

      shinyjs::hidden(
        numericInput("holder","holder",0)
      ),

      # Catch data input
      fileInput(
        "catch_data",
        "Upload Catch Data",
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv",
          ".xls",
          ".xlsx",
          ".RData"
        )
      ),


      # File input for the priors file
      fileInput("all_priors", "All Prior Inputs",
                accept = c(".RData",
                           ".rda")),


      # Numeric input for the catch year
      numericInput("year", "Catch Year", 2018),

      actionLink("manual", "Insert Catch Manually"),

      shinyjs::hidden(

        div(id = "advanced",

      actionButton("calc_button", "Read Catch"))),

      # actionButton("edit_button", "Edit Catch"))),

      # Numeric input for the gdp deflator
      numericInput("deflator", "Deflator to Multiplier Year (default=2008)", 0.8548349),

      # Option list for the output type
      selectInput(
        "output",
        "Output Type:",
        c(
          "All" = "all",
          "Summary" = "summary",
          "Economic Category" = "econ",
          "States" = "states",
          "Impact Type" = "impact",
          "FEUS" = "FEUS",
          "Manual" = "Manual"
        )
      ),

      # Do we want to call the itis API for new categories?
      checkboxInput("recall", "Pull New Catch Categories?", FALSE),

      # Checkbox for whether we're doing imports or not
      checkboxInput("imports", "Imports", TRUE),


      # Alex - Don't forget to make the FEUS outputtable in Excel!

      # A button that runs the model
      actionButton("button", "Run Model"),

      actionButton("refresh", "Refresh")
    ),

    # The display window
    mainPanel(
      shinyjs::hidden(
      div(id = "selectors",

          fluidRow(

          column(4,
          numericInput("shrimp", "Shrimp", 0),
          numericInput("crab", "Crab", 0),
          numericInput("lobster", "Lobster", 0),
          numericInput("east_coast", "East Coast Groundfish", 0),
          numericInput("hms", "HMS", 0),
          numericInput("reef_fish", "Reef Fish", 0),
          selectInput("fips", "State:",
                      c(
                        "United States" = "US",
                        "Alabama" = "AL",
                        "Florida" = "FL",
                        "Louisiana" = "LA",
                        "Mississippi" = "MS",
                        "Texas" = "TX",
                        "Delaware" = "DE",
                        "Maryland" = "MD",
                        "New Jersey" = "NJ",
                        "New York" = "NY",
                        "Virginia" = "VA",
                        "Connecticut" = "CT",
                        "Maine" = "ME",
                        "Massachusetts" = "MA",
                        "New Hampshire" = "NH",
                        "Rhode Island" = "RI",
                        "Alaska" = "AK",
                        "California" = "CA",
                        "Oregon" = "OR",
                        "Washington" = "WA",
                        "Georgia" = "GA",
                        "North Carolina" = "NC",
                        "South Carolina" = "SC",
                        "Hawai`i" = "HI"
                      ))),

          column(4,
          numericInput("west_coast", "West Coast Groundfish", 0),
          numericInput("halibut", "Halibut", 0),
          numericInput("menhaden", "Menhaden/Industrial", 0),
          numericInput("salmon", "Salmon", 0),
          numericInput("scallop", "Sea Scallop", 0),
          numericInput("clam_quahog", "Surf Clam/Ocean Quahog", 0),
          numericInput("import_num", "Imports", 0)),

          column(4,
          numericInput("trawl", "Other Trawl", 0),
          numericInput("finfish", "All Other Finfish", 0),
          numericInput("shellfish", "All Other Shellfish", 0),
          numericInput("freshwater", "Freshwater", 0),
          numericInput("inshore", "Inshore/Miscellaneous", 0),
          numericInput("bait", "Bait", 0))

          ),

          fluidRow(
            actionButton("reset_manual", "Reset")
          )

          )),

      div(
        id = "body",

        # It starts off hidden
        shinyjs::hidden(
          div(
            id = "table",

            fluidRow(# Option list for the table type
              selectInput(
                "table_out",
                "Choose Table",
                c("Totals" = "placeholder")
              )),

            fluidRow(shinycssloaders::withSpinner(DT::dataTableOutput("impacts"), type = 6))
          )),
        fluidRow(shinyjs::hidden(div(
          id = "lt1",
          # The loading text
          p("Categorizing catch...")
        ))),
        fluidRow(shinyjs::hidden(div(
          id = "lt1.5",
          # The loading text
          p("Categorizing catch...Done!")
        ))),
        fluidRow(shinyjs::hidden(div(
          id = "lt2",
          # The loading text
          p("Calculating impacts...")
        ))),
        fluidRow(shinyjs::hidden(div(
          id = "lt2.5",
          # The loading text
          p("Calculating impacts...Done!")
        ))),
        fluidRow(shinyjs::hidden(div(
          id = "lt3",
          # The loading text
          p("Cleaning tables...")
        ))),
        fluidRow(shinyjs::hidden(div(
          id = "down",
          # Download button
          downloadButton("downloadData","Download")
        )))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  session <- getDefaultReactiveDomain()

  observeEvent(input$refresh, {
    session$reload()
  })

  observeEvent(input$manual, {
    shinyjs::toggle(id = "advanced", anim = TRUE)
  })

  # Only enable the run button if there is data
  observe({
    if (is.null(input$all_priors)) {
      shinyjs::disable("button")
    } else {
      shinyjs::enable("button")
    }
  })

  observe({
    if (is.null(input$all_priors) | is.null(input$catch_data)) {
      shinyjs::disable("calc_button")
    } else {
      shinyjs::enable("calc_button")
    }
  })

  observeEvent(input$calc_button, {

    updateNumericInput(inputId = "holder", value = 1)

    # Write the first load text
    shinyjs::toggle(id = "lt1", anim = TRUE)

    # Load catch_data and reading it based on its extension
    if ( is.null(input$catch_data)) return(NULL)
    catch_path = input$catch_data
    ext <- tools::file_ext(catch_path$datapath)

    if (ext == "csv") {
      catch_data = readr::read_csv(catch_path$datapath)
    }

    if (ext == "xlsx") {
      catch_data = readxl::read_excel(catch_path$datapath)
    }

    if (ext == "RData" | ext == "rda") {
      catch_file <- catch_path$datapath
      e = new.env()
      name <- load(catch_file, envir = e)
      assign(catch_data, e[[name]])
    }

    # Read in priors data
    if ( is.null(input$all_priors)) return(NULL)

    priors_file = input$all_priors
    priors_path <- priors_file$datapath

    e = new.env()
    name <- load(priors_path, envir = e)

    Comm.Catch.Spp.List <- e$Comm.Catch.Spp.List
    fp <- e$fp %>% filter(fips != 12.5)
    fp$state_abbr[fp$fips==12]<-"FL"
    imports <- e$imports
    imports_states <- e$imports_states
    multipliers <- e$multipliers
    tsn_id <- e$tsn_id

    # Grab the Year
    current_year = input$year

    # Grab the deflator
    defl = input$deflator

    # Deal with imports
    imp = input$imports
    if(imp == T){
      imp = imports
    }

    # Grab recall
    recat = input$recall

    # Classify Species using itis_reclassify
    base_catch = io_classifier(catch_data,  species = Comm.Catch.Spp.List, year = current_year, recall = recat, tsn = tsn_id)

    # And change the load text
    shinyjs::toggle(id = "lt1", anim = TRUE)
    shinyjs::toggle(id = "lt1.5", anim = TRUE)

    fip = input$fips

    if(fip == "US"){
      base_nums = base_catch %>%
        group_by(`Species Category`) %>%
        summarize(catch = sum(base_catch))
    } else {
      base_nums = base_catch %>%
        left_join(fp) %>%
        filter(state_abbr == fip) %>%
        select(-state_abbr) %>%
        group_by(`Species Category`) %>%
        summarize(catch = sum(base_catch))
    }


    updateNumericInput(inputId = "shrimp", value = base_nums$catch[base_nums$`Species Category`=="Shrimp"])
    updateNumericInput(inputId = "crab", value = base_nums$catch[base_nums$`Species Category`=="Crab"])
    updateNumericInput(inputId = "lobster", value = base_nums$catch[base_nums$`Species Category`=="Lobster"])
    updateNumericInput(inputId = "east_coast", value = base_nums$catch[base_nums$`Species Category`=="East Coast Groundfish"])
    updateNumericInput(inputId = "hms", value = base_nums$catch[base_nums$`Species Category`=="HMS"])
    updateNumericInput(inputId = "reef_fish", value = base_nums$catch[base_nums$`Species Category`=="Reef Fish"])
    updateNumericInput(inputId = "west_coast", value = base_nums$catch[base_nums$`Species Category`=="West Coast Groundfish"])
    updateNumericInput(inputId = "halibut", value = base_nums$catch[base_nums$`Species Category`=="Halibut"])
    updateNumericInput(inputId = "menhaden", value = base_nums$catch[base_nums$`Species Category`=="Menhaden and Industrial"])
    updateNumericInput(inputId = "salmon", value = base_nums$catch[base_nums$`Species Category`=="Salmon"])
    updateNumericInput(inputId = "scallop", value = base_nums$catch[base_nums$`Species Category`=="Sea Scallop"])
    updateNumericInput(inputId = "clam_quahog", value = base_nums$catch[base_nums$`Species Category`=="Surf Clam and Ocean Quahog"])
    updateNumericInput(inputId = "trawl", value = base_nums$catch[base_nums$`Species Category`=="Other Trawl"])
    updateNumericInput(inputId = "finfish", value = base_nums$catch[base_nums$`Species Category`=="All Other Finfish"])
    updateNumericInput(inputId = "shellfish", value = base_nums$catch[base_nums$`Species Category`=="All Other Shellfish"])
    updateNumericInput(inputId = "freshwater", value = base_nums$catch[base_nums$`Species Category`=="Freshwater"])
    updateNumericInput(inputId = "inshore", value = base_nums$catch[base_nums$`Species Category`=="Inshore and Miscellaneous"])
    updateNumericInput(inputId = "bait", value = base_nums$catch[base_nums$`Species Category`=="Bait"])


    filtered_import = imports[imports$state_abbr == fip,]
    basic_import = filtered_import$imports[1]

    updateNumericInput(inputId = "import_num", value = basic_import)

    shinyjs::toggle(id = "lt1.5", anim = TRUE)

  })

  # observeEvent(input$edit_button, {
  #   shinyjs::toggle(id = "selectors", anim = TRUE)
  #   manual = 1
  # })

  observeEvent(input$reset_manual, {
    shinyjs::hide(id = "selectors", anim = TRUE)

    updateNumericInput(inputId = "holder", value = 0)

    updateNumericInput(inputId = "shrimp", value = 0)
    updateNumericInput(inputId = "crab", value = 0)
    updateNumericInput(inputId = "lobster", value = 0)
    updateNumericInput(inputId = "east_coast", value = 0)
    updateNumericInput(inputId = "hms", value = 0)
    updateNumericInput(inputId = "reef_fish", value = 0)
    updateNumericInput(inputId = "west_coast", value = 0)
    updateNumericInput(inputId = "halibut", value = 0)
    updateNumericInput(inputId = "menhaden", value = 0)
    updateNumericInput(inputId = "salmon", value = 0)
    updateNumericInput(inputId = "scallop", value = 0)
    updateNumericInput(inputId = "clam_quahog", value = 0)
    updateNumericInput(inputId = "trawl", value = 0)
    updateNumericInput(inputId = "finfish", value = 0)
    updateNumericInput(inputId = "shellfish", value = 0)
    updateNumericInput(inputId = "freshwater", value = 0)
    updateNumericInput(inputId = "inshore", value = 0)
    updateNumericInput(inputId = "bait", value = 0)
    updateNumericInput(inputId = "import_num", value = 0)

  })

  observeEvent(input$manual, {
    shinyjs::show(id = "selectors", anim = TRUE)
    updateNumericInput(inputId = "holder", value = 1)
  })

  # Calling the FRED API to get deflators - I may want to change this.
  api.key = "71604655b73da6ec59cfa3593d99d0cf"
  fredr_set_key(api.key)
  gdp <- fredr(series_id = 'USAGDPDEFAISMEI')
  gdp$YEAR<-as.numeric(substr(x = gdp$date, start = 1, stop = 4))
  gdp = as.data.frame(gdp) %>%
    filter(!is.na(value)) %>%
    select(YEAR, value) %>%
    mutate(inflator = value/gdp$value[gdp$YEAR==2008]) %>%
    select(-value) %>%
    mutate(deflator = 1/inflator)


  # Suggest a deflator
  observeEvent(input$year, {

    current_year <- input$year

    if (current_year %in% gdp$YEAR) {
      deflat <- gdp$deflator[gdp$YEAR == current_year]
    } else {
      deflat <- gdp$deflator[gdp$YEAR == max(gdp$YEAR)]
    }

    updateNumericInput(inputId = "deflator", value = deflat)

  })


  # When the button is pressed...
  observeEvent(input$button, {

    shinyjs::hideElement(id = "selectors", anim = TRUE)

    manual = input$holder

    if(manual == 0){

      # Write the first load text
      shinyjs::toggle(id = "lt1", anim = TRUE)

      # Load catch_data and reading it based on its extension
      if ( is.null(input$catch_data)) return(NULL)
      catch_path = input$catch_data
      ext <- tools::file_ext(catch_path$datapath)

      if (ext == "csv") {
        catch_data = readr::read_csv(catch_path$datapath)
      }

      if (ext == "xlsx") {
        catch_data = readxl::read_excel(catch_path$datapath)
      }

      if (ext == "RData" | ext == "rda") {
        catch_file <- catch_path$datapath
        e = new.env()
        name <- load(catch_file, envir = e)
        assign(catch_data, e[[name]])
      }

      # Read in priors data
      if ( is.null(input$all_priors)) return(NULL)

      priors_file = input$all_priors
      priors_path <- priors_file$datapath

      e = new.env()
      name <- load(priors_path, envir = e)

      Comm.Catch.Spp.List <- e$Comm.Catch.Spp.List
      fp <- e$fp %>% filter(fips != 12.5)
      fp$state_abbr[fp$fips==12]<-"FL"
      imports <- e$imports
      imports_states <- e$imports_states
      multipliers <- e$multipliers
      tsn_id <- e$tsn_id

      # Grab the Year
      current_year = input$year

      # Grab the deflator
      defl = input$deflator

      # Deal with imports
      imp = input$imports
      if(imp == T){
        imp = imports
      }

      # Grab recall
      recat = input$recall

      # Classify Species using itis_reclassify
      base_catch = io_classifier(catch_data,  species = Comm.Catch.Spp.List, year = current_year, recall = recat, tsn = tsn_id)

      # And change the load text
      shinyjs::toggle(id = "lt1", anim = TRUE)
      shinyjs::toggle(id = "lt1.5", anim = TRUE)
      shinyjs::toggle(id = "lt2", anim = TRUE)

    } else {

      updateSelectInput(inputId = "output", selected = "manual")

      # Write the load text
      shinyjs::toggle(id = "lt1", anim = TRUE)

      # Create base_catch using manual inputs
      base_catch = data.frame(Species = c("Shrimp",
                                                     "Crab",
                                                     "Lobster",
                                                     "East Coast Groundfish",
                                                     "HMS",
                                                     "Reef Fish",
                                                     "West Coast Groundfish",
                                                     "Halibut",
                                                     "Menhaden and Industrial",
                                                     "Salmon",
                                                     "Sea Scallop",
                                                     "Surf Clam/Ocean Quahog",
                                                     "Other Trawl",
                                                     "All Other Finfish",
                                                     "All Other Shellfish",
                                                     "Freshwater",
                                                     "Inshore and Miscellaneous",
                                                     "Bait"),
                              base_catch = c(
                                input$shrimp,
                                input$crab,
                                input$lobster,
                                input$east_coast,
                                input$hms,
                                input$reef_fish,
                                input$west_coast,
                                input$halibut,
                                input$menhaden,
                                input$salmon,
                                input$scallop,
                                input$clam_quahog,
                                input$trawl,
                                input$finfish,
                                input$shellfish,
                                input$freshwater,
                                input$inshore,
                                input$bait
                              )) %>%
        rename(`Species Category` = Species) %>%
        mutate(state_abbr = input$fips, Year = input$year) %>%
        left_join(fp) %>%
        select(-state_abbr)
      base_catch$base_catch[is.na(base_catch$base_catch)]<-0
      print(base_catch)

      imp = imports_states %>%
        filter(state_abbr == input$fips) %>%
        mutate(base_catch = value * input$import_num, `Species Category` = "Imports", `Economic Category` = name) %>%
        select(fips, `Economic Category`, base_catch, `Species Category`)


      # And change the load text
      shinyjs::toggle(id = "lt1", anim = TRUE)
      shinyjs::toggle(id = "lt1.5", anim = TRUE)
      shinyjs::toggle(id = "lt2", anim = TRUE)
    }

    # Unhide the "table" div
    shinyjs::toggle(id = "table", anim = TRUE)
    print(base_catch)

    # Run the model (FEUS has to be run differently)
    if(input$output == "FEUS"){

      # Calculate the impacts
      impacts.i <- io_calculator(catch = base_catch, import_numbers = imp, implan_multipliers = multipliers, deflator = defl, import_state_multipliers = imports_states)
      impacts.ni <- io_calculator(catch = base_catch, import_numbers = F, implan_multipliers = multipliers, deflator = defl, import_state_multipliers = imports_states)

      # Update the load text
      shinyjs::toggle(id = "lt2", anim = TRUE)
      shinyjs::toggle(id = "lt2.5", anim = TRUE)
      shinyjs::toggle(id = "lt3", anim = TRUE)


      # Clean the impacts
      impacts.commercial.i <- io_cleaner(impact = impacts.i, format = "FEUS", xlsx = F, fp = fp, maxyr = 2018)
      impacts.commercial.ni <- io_cleaner(impact = impacts.ni, format = "FEUS", xlsx = F, fp = fp, maxyr = 2018)
      impacts.commercial= bind_rows(impacts.commercial.i, impacts.commercial.ni) %>%
        mutate(Index = row_number())



      # Render the impacts
      output$impacts <- renderTable(impacts.commercial)


      # Then get rid of it
      shinyjs::toggle(id = "lt1.5", anim = TRUE)
      shinyjs::toggle(id = "lt2.5", anim = TRUE)
      shinyjs::toggle(id = "lt3", anim = TRUE)
      shinyjs::toggle(id = "down", anim = TRUE)

      output$downloadData <- downloadHandler(
        filename = function() {
          paste0("io_impacts_", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          writexl::write_xlsx(impacts.commercial, path = file)
        }
      )


      # All other options
    } else if(input$output == "manual"){

      impacts <- io_calculator(catch = base_catch, import_numbers = imp, implan_multipliers = multipliers, deflator = defl, import_state_multipliers = imports_states)
      print(base_catch)
      #print(impacts)

    } else {

      # Grab the out format
      out_format <- input$output

      # Get rid of the alternate Floridas
      base_catch_nofl = base_catch %>% filter(fips != 12)
      base_catch_fl = base_catch %>% filter(fips == 12) %>%
        group_by(`Species Category`, Year) %>%
        summarize(base_catch = sum(base_catch)) %>%
        mutate(Region = "Gulf of Mexico", State = "Florida")
      base_catch = bind_rows(base_catch_nofl, base_catch_fl)

      # Calculate impacts
      impacts <- io_calculator(catch = base_catch, import_numbers = imp, implan_multipliers = multipliers, deflator = defl, import_state_multipliers = imports_states)
      print(impacts)

      # And change the load text
      shinyjs::toggle(id = "lt2", anim = TRUE)
      shinyjs::toggle(id = "lt2.5", anim = TRUE)
      shinyjs::toggle(id = "lt3", anim = TRUE)

      # Clean up the tabels
      impacts.commercial <- io_cleaner(impact = impacts, format = out_format, xlsx = F, fp = fp)

      names = names(impacts.commercial)

      updateSelectInput(session, "table_out", choices = names)

      observeEvent(input$table_out,{
        output$impacts <- DT::renderDataTable(DT::datatable({impacts.commercial[input$table_out][[1]]}))
      })

      # ...And get rid of it
      shinyjs::toggle(id = "lt1.5", anim = TRUE)
      shinyjs::toggle(id = "lt2.5", anim = TRUE)
      shinyjs::toggle(id = "lt3", anim = TRUE)
      shinyjs::toggle(id = "down", anim = TRUE)

      output$downloadData <- downloadHandler(
        filename = function() {
          paste0("io_impacts_", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          writexl::write_xlsx(impacts.commercial, path = file)
        }
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

