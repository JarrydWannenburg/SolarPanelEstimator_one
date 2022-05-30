# To view code effectively, go to Code > Soft Wrap Long Lines

# Import libraries
library(data.table) # Used mainly for data frame manipulation
library(DT) # For data table styling
library(httr) # API calls
library(shiny) # Web app
library(shinyalert) # Used to create the popups/alerts
library(shinyBS) # Tooltip descriptions
library(shinydashboard) # Value Boxes
library(shinyjs) # 'Quit' page to prevent freezes on local machine
library(shinythemes) # Style web app
library(tidygeocoder) # Obtain lat and Lon from zip code
library(tidyverse) # Standard import that contains many useful libraries

# Simple function to close window (not needed in shiny apps version) but useful when testing (sometimes shiny can glitch and force a restart)
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# Create a header to be used repeatedly in the code.
Header_Details <- list(h1(em("Welcome to my ‘How large should my solar system be’ estimator!"), align="center"),
                       br(),
                       br(),
                       h2(em("With this estimator, users can specify detailed information of a solar panel system of interest or use default values provided by the National Renewable Energy Laboratory (NREL). After entering information specific to the solar panels and selecting the preferred datasource, the estimator will provide a breakdown of energy costs and outputs expected by using these solar panels. This estimator also estimates the optimal number of panels needed to meet a user's specified annual energy demand. This estimator is particularly useful for the design of new solar panel installations. An (unrealistic) assumption that this estimator makes is that you want to install all panels in year one which will not be as cost effective as installing panels when needed.")),
                       br(),
                       h2("User information entered in this estimator is not stored.", align = "center"),
                       br(),br(),br(),br())

# Create a header to be used repeatedly in the code.
Footer_Details <- list(hr(),
                       p("Questions? Please contact me at", tags$a(href="jarrydwannenburg@gmail.com","jarrydwannenburg@gmail.com"), align="center", style = "font-size:20px;"), 
                       p("Copyright 2022", align="center", style = "font-size:20px;"))

# Create a FAQ page. This could be done within the UI function but I moved it here for readability
FAQ_Details <- list(
  h1("Frequently Asked Questions"),
  br(),
  #h2(tags$b("What is the difference between alternating and direct current (AC vs. DC)?")),
  #h3("",
  #tags$a(href="https://www.greenhousemag.com/article/does-light-transmission-through-greenhouse-glazing-matter/", tags$u("Insert article describing AC vs. DC"))),
  
  h2(tags$b("What are my system's losses?")),
  h3("",
     tags$a(href="https://diysolarshack.com/10-solar-pv-system-losses-their-impact-on-solar-panel-output/", tags$u("Article describing solar panel losses"))),
  
  h2(tags$b("What is azimuth and how can I calculate what my value would be?")),
  h3("",
     tags$a(href="https://www.gardelelectrical.com.au/blog/solar-tilt-angle-and-azimuth#:~:text=Azimuth%20is%20the%20angle%20that,a%20clockwise%20direction%20from%20north.", tags$u("Article describing azimuth"))),
  
  h2(tags$b("How do I determine my electricity rate?")),
  h3("Your electricity rate is reported in cost per kilowatt hour ($/kWh) and can be found on your power bill or by contacting your electricity provider. Cost per kilowatt hour may be written clearly on your bill or you can determine it by taking you total power bill (minus taxes) and divide that by your power consumption. Electricity costs typically range from $0.05 - $0.30 / kWh."),
  
  #h2(tags$b("How do I find decide on what my tilt would be?")),
  #h3("", tags$a(href="https://gpnmag.com/article/plant-lighting-efficiency-and-efficacy-%CE%BCmol%C2%B7j-%C2%B9/", tags$u("Insert article describing tilt"))),
  
  #h2(tags$b("How do I find decide on what my ground coverage ratio would be?")),
  #h3("", tags$a(href="https://gpnmag.com/article/plant-lighting-efficiency-and-efficacy-%CE%BCmol%C2%B7j-%C2%B9/", tags$u("Insert article describing ground coverage ratio"))),
  
  h2(tags$b("How do I find decide on what my inverter efficiency would be?")),
  h3("", tags$a(href="https://palmetto.com/learning-center/blog/solar-inverter-guide-types-benefits-cost-how-solar-inverters-work", tags$u("Article describing inverter efficiency"))),
  
  h2(tags$b("What are the differences between each array type?")),
  h3("", tags$a(href="https://www.solarpowerworldonline.com/2020/01/what-is-a-solar-tracker-and-how-does-it-work/#:~:text=There%20are%20two%20main%20types,panels%20track%20the%20sun%20directly.", tags$u("Article describing tracking array types"))),
  
  h2(tags$b("What are the differences between each module type?")),
  h3("", tags$a(href="https://solar.kernsteel.com/what-are-the-different-types-of-pv-modules/", tags$u("Article describing module types"))),
  
  #h2(tags$b("What are the differences between each dataset source?")),
  #h3("", tags$a(href="https://gpnmag.com/article/plant-lighting-efficiency-and-efficacy-%CE%BCmol%C2%B7j-%C2%B9/", tags$u("Insert article describing different sources"))),
  
  h2(tags$em("For more information on greenhouse lighting, visit ", tags$u(tags$a(href="https://hortlamp.org/outreach", "www.hortlamp.com/outreach"))))
)

# Reads the file where NSRDB and other API keys are located
keys <-  'VirtualGrowerAppKeys.csv'
k <-  read_csv(keys)

# Geocodio API Key
Sys.setenv(GEOCODIO_API_KEY = k$GEOCODIO_API_KEY) # Geocodio Key

# Reactive list used for geolocation validation
Loc_values <- reactiveValues() 

# Beginning of UI shiny object
ui <- fluidPage(
  navbarPage(
    title = "SolarPanelEstimator_one",
    theme = shinytheme('united'),
    
    tabPanel("Home",
             isolate({Header_Details}),
             p(strong("How to use this estimator"), align="left", style="font-size:30px"),
             p("1. Start by selecting the 'Estimations' tab where you provide a zip code, electricity rate in $/kWh, and energy demanded (kWh) in the 'Background Inputs' section.", align="left", style="font-size:30px"),
             p("2. Then you can add information about the solar panel system of interested in the 'System Inputs' section.", align="left", style="font-size:30px"),
             p("3. Next, add additional information about the system in the 'Finer Details' section to get the most accurate results.", align="left", style="font-size:30px"),
             p("4. Voila! You've calculated your predicted energy usage and optimal number of solar panels!", align="left", style="font-size:30px"),
             br(),
             p("NOTE: ALL information is discarded once you exit the estimator.", align="center", style="font-size:30px"),
             isolate({Footer_Details})),
    
    tabPanel("Estimations",
             sidebarPanel(titlePanel("Enter Solar Panel Details"), # The sidebar panel is used to display inputs
                          tipify(h3("Background Inputs", align = 'right'),title = 'Location and electricity inputs',placement = 'right',trigger = 'hover',options = list(container = 'body')),
                          
                          textInput('zipcode', 'Zipcode', value = ''),
                            bsTooltip('zipcode', "Enter a valid 5 digit zipcode.",'right',options = list(container = 'body')),
                          
                          numericInput('energy_demand', 'Annual energy demanded (kWh)', min=0, max=1000000, value = '', step = 0.0001),
                            bsTooltip('energy_demand', "Estimated annual electricity demanded in kWh alternating current (which houses use).",'right',options = list(container = 'body')),
                          
                          numericInput('elec_rate', 'Electricity Rate ($/kWh)', min=0, max=1, value ='', step = 0.01),
                            bsTooltip('elec_rate', "Typically, between $0.05 and $0.30. Only use numbers with a decimal. Do NOT include a dollar sign ($).",'right',options = list(container = 'body')),
                          
                          tipify(h3("System Inputs", align = 'right'),title = 'Solar system inputs and forecast horizon.',placement = 'right',trigger = 'hover',options = list(container = 'body')),
                          
                          numericInput('capital_cost', 'Total System Cost ($)', min=0, max=10000000, value = '', step = 0.01),
                            bsTooltip('capital_cost', "Solar System capital cost. Do NOT include a dollar sign ($).",'right',options = list(container = 'body')),
                          
                          numericInput('installation_cost', 'System Installation Cost (% of capital cost)', min=0, max=100, value = '', step = 0.0001),
                            bsTooltip('installation_cost', "Panels installation cost. Typically around 20% of the capital cost.",'right',options = list(container = 'body')),
                          
                          numericInput('subsidies', 'Subsidies (%)', min=0, max=100, value = '', step = 0.01),
                            bsTooltip('subsidies', "Subsidy discount as a percent of capital.",'right',options = list(container = 'body')),
                          
                          numericInput('system_life', 'Expected life span (Years)', min=0, max=100, value = '', step = 1),
                            bsTooltip('system_life', "Estimated lifespan of solar system. Decimals are okay.",'right',options = list(container = 'body')),
                          
                          numericInput('nameplate_capacity', 'solar panel output in kW',min = 0.05, max = 500000, value = '', step = 0.0001), #max of API is 500,000 kW
                            bsTooltip('nameplate_capacity', "The sum of the nameplate Capacities from each panel, which is defined as the number of watts the generator can provide at full rated power. Generators are typically called by their Nameplate Capacity.",'right',options = list(container = 'body')),
                          
                          tipify(h3("Finer Details", align = 'right'),title = 'If unknown, leave values as is as these are default values.',placement = 'right',trigger = 'hover',options = list(container = 'body')),
                          
                          numericInput('losses','Losses (%)', min = -5, max = 100, value = 10, step = 0.0001),
                            bsTooltip('losses', "System losses (do NOT include a percent sign %).",'right',options = list(container = 'body')),
                          
                          numericInput('azimuth', 'Azimuth (degrees)', min=0, max=359.9999, value = 180, step = 0.0001),
                            bsTooltip('azimuth', "Azimuth is the angle that the solar panels are facing and is measured in a clockwise direction from north.",'right',options = list(container = 'body')),
                          
                          numericInput('dc_ac_ratio','DC to AC ratio', min=0, max=50, value = 1.2, step = 0.0001),
                            bsTooltip('dc_ac_ratio', "The ratio of installed DC capacity to the inverters AC power rating.",'right',options = list(container = 'body')),
                          
                          numericInput('inv_eff','Inverter Efficiency (%)', min=90, max=99.99, value = 98, step = 0.0001),
                            bsTooltip('inv_eff', "Inverter efficiency is the ratio of the usable AC output power to the sum of the DC input power and any AC input power. Typically, between 95 to 98%.",'right',options = list(container = 'body')),
                          
                          radioButtons('array_type', 'Array Type', choiceNames = c('Fixed - Open Rack','Fixed - Roof Mounted','1-Axis','1-Axis Backtracking','2-Axis'), choiceValues = c(0,1,2,3,4)),
                          
                          radioButtons('module_type', 'Module Type', choiceNames = c('Standard: Polycrystalline','Premium: Monocrystalline','Thin film'), choiceValues = c(0,1,2)),
                          
                          numericInput('elec_price_percent_change','Average Annual Electricity Price Change (%)', min=-10, max=10, value = 2.5, step = 0.01),
                            bsTooltip('elec_price_percent_change', "Predicted average percent increase/decrease of electricity prices. The EIA estimates electricity prices increase by an average of 1.8% annually.",'right',options = list(container = 'body')),
                          
                          numericInput('demand_percent_change','Average Electricity Demand Change (%)', min=-10, max=10, value = 1, step = 0.01),
                            bsTooltip('demand_percent_change', "Predicted average percent increase/decrease of electricity demand over the system life. The EIA estimates electricity demand increased by an average of 1% annually.",'right',options = list(container = 'body')),
                          
                          numericInput('solar_panel_degradation_rate','Average Solar Panel Degradation (%)', min=-10, max=10, value = 0.5, step = 0.01),
                            bsTooltip('solar_panel_degradation_rate', "Predicted average solar system decrease of production over the system life. The National Renewable Energy Laboratory (NREL) estimates this at a 0.5% decrease yearly.",'right',options = list(container = 'body')),
                          
                          actionButton('calculate', 'Estimate'),
                          width = 3),
             
             mainPanel( # Main panel is used to display outputs
               fluidRow( # Upper row of value boxes
                 column(2,
                        valueBoxOutput("panel_purchase", width = NULL),
                          bsTooltip('panel_purchase', "The number of panels to eventually install to have the most cost effective setup over the forecast horizon",'bottom',options = list(container = 'body'))),
                 column(2,
                        valueBoxOutput("ac_annual_tile", width = NULL),
                          bsTooltip('ac_annual_tile', "Estimated AC output calculated by NRELs PVWatts Calculator",'bottom',options = list(container = 'body'))),
                 column(2,
                        valueBoxOutput("total_solar_cost", width = NULL),
                          bsTooltip('total_solar_cost', "Total cost (1 year) from solar capital (depreciated evenly over the lifespan)",'bottom',options = list(container = 'body'))),
                 column(2,
                        valueBoxOutput("solrad_annual_tile", width = NULL),
                          bsTooltip('solrad_annual_tile', "Estimated annual solar radiation calculated by NRELs PVWatts Calculator",'bottom',options = list(container = 'body'))),
                 column(2,
                        valueBoxOutput("capacity_factor_tile", width = NULL),
                          bsTooltip('capacity_factor_tile', "The ratio of the systems predicted electrical output in the first year of operation to the nameplate output, which is equivalent to the quantity of energy the system would generate if it operated at its nameplate capacity for every hour of the year. (AC-to-DC)",'bottom',options = list(container = 'body'))),
                 column(2,
                        valueBoxOutput("station_distance_tile", width = NULL),
                          bsTooltip('station_distance_tile', "Distance to weather tracker used for weather data", 'bottom', options = list(container = 'body')))
               ), # Fluid row close
               br(), br(),
               fluidRow( # Contains the main table and column of value boxes
                 column(9,
                        h3("Estimated Yearly Values"),
                        dataTableOutput('main_df'),
                          bsTooltip('main_df', "Yearly Detailed Table",'top',options = list(container = 'body'))),
                 column(3,
                        br(),
                        valueBoxOutput("solar_energy_provided", width = NULL),
                          bsTooltip('solar_energy_provided', "Estimated solar energy (kWh) provided by the system over the forecast horizon", 'bottom', options = list(container = 'body')),
                        br(),
                        valueBoxOutput("energy_purchase", width = NULL),
                          bsTooltip('energy_purchase', "Estimated supplemental electricity purchased from the grid", 'bottom', options = list(container = 'body')),
                        br(),
                        valueBoxOutput("total_grid_cost", width = NULL),
                          bsTooltip('total_grid_cost', "Estimated cost associated with supplemental energy purcahses from the grid", 'bottom', options = list(container = 'body')),
                        br(),
                        valueBoxOutput("levelized_cost", width = NULL),
                          bsTooltip('levelized_cost', "Describes the cost of the power produced by solar over the forecast horizon. By purchasing solar you are essentially creating a hedge against rising utility costs by fixing the per kWh rate at a known cost", 'bottom', options = list(container = 'body')),
                        br(),
                        valueBoxOutput("solar_savings_tile_lifetime", width = NULL),
                          bsTooltip('solar_savings_tile_lifetime', "Estimate of the dollar savings associated with installing the optimal system compared to purchasing solely from the grid", 'bottom', options = list(container = 'body')),
                        br(),
                        valueBoxOutput("total_cost", width = NULL),
                          bsTooltip('total_cost', "Estimated total cost for the optimal system including supplemental grid purchases. May be off because of rounding to nearest integer in table", 'bottom', options = list(container = 'body'))
                        )
               ), # Fluid row close
               fluidRow(
                 column(6,
                        h3("Energy Demand/Production Plot"),
                        plotOutput('degradation_plot')),
                 column(6,
                        h3("Electricity Cost Plot"),
                        plotOutput('annual_levelized_plot'))
               ) # Fluid row close
             )), # Main panel and tab panel
    
    tabPanel(title="FAQ",
             FAQ_Details),
    
    tabPanel(title = "Quit",
             actionButton("close", "Click Here to End Session"))
  )) # Navbar page and fluid page which marks the end of the UI object

# Beginning of server object
server <- function(input, output, session) {
  useShinyjs()
  extendShinyjs(text = jscode, functions = c("closeWindow"))
  
  
  # Close/exit the server session (useful when coding locally. Can be bypassed via shiny apps)
  observeEvent(input$close, {
    js$closeWindow()
    stopApp()
  })
  
  
  # Create an observe event to trigger the calculations only once a user clicks the calculate button (visibly Estimate, code wise its calculate)
  observeEvent(input$calculate,{
    # Location validation
    geo_test <- FALSE # Location must be found, so default is FALSE
    if(nchar(input$zipcode)!=5 | input$zipcode==""){
      shinyalert("Please enter a 5 digit zipcode. Values defaulted to zipcode: 30606 (Athens, GA)", type="error")
      Loc_values$lat <- 33.950001
      Loc_values$long <-  -83.383331
    } else{
      t <- try(geo(postalcode=as.character(input$zipcode), lat = "lat", long = "long", verbose = FALSE, method="geocodio"))
      if ("try-error" %in% class(t)) {
        shinyalert("Location not found please try a different address or zip code. Values defaulted to zipcode: 30606 (Athens, GA)", type="error")
        Loc_values$lat <- 33.950001
        Loc_values$long <-  -83.383331
      } else{
        geoloc <- geo(postalcode=as.character(input$zipcode), lat = "lat", long = "long", verbose = FALSE, method="geocodio")
        if(is.logical(geoloc$lat)){ #is.logical() is used if geocascade doesn't find address, it returns N/A
          shinyalert("Location not found please try a different address or zip code. Values defaulted to zipcode: 30606 (Athens, GA)", type="error")
          Loc_values$lat <- 33.950001
          Loc_values$long <-  -83.383331
        } 
        else{
          shinyalert("Estimating Forecast...")
          geo_test <- TRUE #Set geo_test to true if location is found
          Loc_values$lat <- geoloc$lat[1] #Set lat to value from geo() function
          Loc_values$long <- geoloc$long[1] #Set long to value from geo() function
        }
      }
    }
    
    
    # Eventually, the preset inputs will need to be cleared value=x set to value="". Possibly will need to int(input$y) for numerical cases.
    # Inputs
    api_key <- k$NSRDB
    array_type <- input$array_type
    azimuth <- as.numeric(input$azimuth)
    dataset <- 'nsrdb'
    dc_ac_ratio <- as.numeric(input$dc_ac_ratio)
    demand_percent_change <- as.numeric(input$demand_percent_change)/100
    elec_price_percent_change <- as.numeric(input$elec_price_percent_change)/100
    elec_rate <- as.numeric(input$elec_rate)
    energy_demand <- as.numeric(input$energy_demand)
    installation_perc <- as.numeric(input$installation_cost)
    installation_cost <- (installation_perc/100)*as.numeric(input$capital_cost) 
    subsidies <- as.numeric(input$subsidies)/100
    capital_cost <- (1-subsidies)*as.numeric(input$capital_cost) # Capital cost reduced by subsidy amount
    inv_eff <- as.numeric(input$inv_eff)
    lat <- as.numeric(Loc_values$lat)
    lon <- as.numeric(Loc_values$long)
    losses <- as.numeric(input$losses)
    module_type <- input$module_type
    nameplate_capacity <- as.numeric(input$nameplate_capacity)
    solar_panel_degradation_rate <- as.numeric(input$solar_panel_degradation_rate)/100
    system_life <- as.numeric(input$system_life)
    tilt <- as.numeric(Loc_values$lat) #Typically just the latitude
    
    
    ################################
              # Test Values
    # api_key <- # Need to copy into file. DO NOT PUSH to git with this value shown
    # capital_cost <- 1000
    # demand_percent_change <- 0.01
    # elec_price_percent_change <- 0.018
    # elec_rate <- 0.01
    # energy_demand <- 10500
    # installation_perc <- 20
    # installation_cost <- .2 * capital_cost
    # nameplate_capacity <- 1.5
    # lat <- 33.950001
    # lon <- -83.383331
    # azimuth <- 120
    # tilt <- 45
    # array_type <- 0
    # module_type <- 0
    # losses <- 10
    # dataset <- 'nsrdb'
    # radius <- 0
    # dc_ac_ratio <- 1.2
    # gcr <- 0.4
    # inv_eff <- 96
    # solar_panel_degradation_rate <- .005
    # system_life <- 30
    
    ################################
    
    
    # Data Retrieval
    Solar_Data_Monthly <- jsonlite::fromJSON(paste0('https://developer.nrel.gov/api/pvwatts/v6.json?api_key=',api_key,'&lat=',lat,'&lon=',lon,'&system_capacity=',nameplate_capacity,'&azimuth=',azimuth,'&tilt=',tilt,'&array_type=',array_type,'&module_type=',module_type,'&losses=',losses,'&dataset=',dataset,'&dc_ac_ratio=',dc_ac_ratio,'&inv_eff=',inv_eff)) #'&gcr=',gcr, '&radius=',radius,
    #Solar_Data_Hourly <- jsonlite::fromJSON(paste0('https://developer.nrel.gov/api/pvwatts/v6.json?api_key=',api_key,'&lat=',lat,'&lon=',lon,'&system_capacity=',nameplate_capacity,'&azimuth=',azimuth,'&tilt=',tilt,'&array_type=',array_type,'&module_type=',module_type,'&losses=',losses,'&dataset=',dataset,'&radius=',radius,'&dc_ac_ratio=',dc_ac_ratio,'&gcr=',gcr,'&inv_eff=',inv_eff,'&timeframe=hourly'))
    #Solar_Data_Daily <- data.frame(unname(tapply(c(Solar_Data_Hourly$outputs$ac), (seq_along(c(Solar_Data_Hourly$outputs$ac))-1) %/% 24, sum)))
    
    
    # API Outputs
    station_distance <- round(Solar_Data_Monthly$station_info$distance / 1609.34, 2)
    
    # Allow for degradation over time
    solar_efficiency <- 1
    for (i in 1:(system_life-1)) {
      solar_efficiency[i+1] <- (1-solar_panel_degradation_rate)**i
    }
    
    # Allow for demand increases over time
    e_demand <- energy_demand
    for (i in 1:(system_life-1)) {
      e_demand[i+1] <- energy_demand*(1+demand_percent_change)**i
    }
    energy_demand = e_demand 
    
    # Allow for electricity price increases over time
    e_rate <- elec_rate
    for (i in 1:(system_life-1)) {
      e_rate[i+1] <- elec_rate*(1+elec_price_percent_change)**i
    }
    elec_rate = e_rate 
    
    # Various intermediary calculations for easy reference
    panel_ac_output <- Solar_Data_Monthly$output$ac_annual
    panel_cost_per_year <- (capital_cost + installation_cost) / system_life 
    panel_cost_per_kWh_year <- (capital_cost + installation_cost) / sum(panel_ac_output*solar_efficiency)
    #y_solar_output <- solar_efficiency * Solar_Data_Monthly$output$ac_annual
    
    
    # Optimization calculations (minimize total cost) 
    if(panel_cost_per_kWh_year < mean(elec_rate)) {
      panels <- floor(energy_demand/panel_ac_output)
      buy_energy_Q <- energy_demand - (panels*panel_ac_output)
      ifelse(buy_energy_Q < 0, 0, buy_energy_Q)
      energy_mix <- c(panels,panels*panel_ac_output,buy_energy_Q)
      if(elec_rate*buy_energy_Q > panel_cost_per_year & buy_energy_Q < panel_ac_output){
        panels = panels + ceiling(buy_energy_Q/panel_ac_output)
        buy_energy_Q <- 0
        energy_mix <- c(panels,panels*panel_ac_output,buy_energy_Q)
      } else{
        #blank space
      }} else {
        panels <- 0
        buy_energy_Q <- energy_demand - (panels*panel_ac_output)
        ifelse(buy_energy_Q < 0, 0, buy_energy_Q)
        energy_mix <- c(panels,panels*panel_ac_output,buy_energy_Q)
      }
    
    
    # Table Creation
    years <- c(1:ceiling(system_life))
    #months <- c(1:12) # Used in monthly tables (which aren't used anymore but are still included in the code)
    yearly_solar_output <- solar_efficiency * Solar_Data_Monthly$output$ac_annual*energy_mix[1]
    grid_electricity_cost_col <- round(ifelse(energy_demand - yearly_solar_output < 0,0,energy_demand - yearly_solar_output)*elec_rate,0)
    
    
    # Binary variable to determine whether an additional panel should be added when looking at grid costs over the lifetime of the system (as a result of degradation)
    test_var = ifelse(energy_mix[1] == 0, FALSE, ((energy_mix[1]*(capital_cost + installation_cost))/(sum(yearly_solar_output)) < mean(elec_rate)))
    
    # If an additional panel was added, enter this while loop. Add a panel and recheck if more are needed. Repeat while opt_test == TRUE
    while ((sum(grid_electricity_cost_col) > (capital_cost + installation_cost)) & test_var == TRUE) {
      additional_panels = 1
      energy_mix[1] = energy_mix[1] + additional_panels
      energy_mix[2] = energy_mix[1] * panel_ac_output
      energy_mix[3] = energy_demand - (energy_mix[2])
      ifelse(energy_mix[3] < 0, 0, energy_mix[3])
      
      yearly_solar_output <- solar_efficiency * Solar_Data_Monthly$output$ac_annual*energy_mix[1]
      grid_electricity_cost_col <- round(ifelse(energy_demand - yearly_solar_output < 0,0,energy_demand - yearly_solar_output)*elec_rate,0)
    }
    
    
    # Solar output after degradation
    yearly_solar_output <- solar_efficiency * Solar_Data_Monthly$output$ac_annual*energy_mix[1]
    yearly_levelized_cost <- 0
    for (i in 1:(system_life)) {
      yearly_levelized_cost[i] <- ((energy_mix[1]*(capital_cost + installation_cost))/(yearly_solar_output[i]))/system_life
    }
    
    # Alert user if it's not cost effective to consider solar
    if(energy_mix[1] == 0){
      shinyalert("Not cost effective to consider solar with these panels")
    }
    
    # Columns
    yearly_system_cost_col <- energy_mix[1]*(capital_cost + installation_cost)/system_life 
    yearly_solar_output_col <- round(yearly_solar_output,0)
    energy_purchased_col <- round(ifelse(energy_demand - yearly_solar_output < 0,0,energy_demand - yearly_solar_output),0)
    grid_electricity_cost_col <- round(ifelse(energy_demand - yearly_solar_output < 0,0,energy_demand - yearly_solar_output)*elec_rate,0)
    yearly_levelized_cost_col <- round(yearly_levelized_cost,3)
    yearly_solar_savings_col <- round(energy_demand*elec_rate - ifelse(yearly_solar_output_col > energy_demand, energy_demand*yearly_levelized_cost_col, (yearly_solar_output_col*yearly_levelized_cost_col + grid_electricity_cost_col)),0) 
    e_demand <- round(e_demand, 0)
    e_rate <- round(e_rate, 3)
    
    
    #Create the dataframe
    df <- data.frame(years, e_demand, yearly_solar_output_col, yearly_levelized_cost_col, e_rate, energy_purchased_col, grid_electricity_cost_col, yearly_solar_savings_col) 
    colnames(df) <- c('Year', 'Energy Demand (kWh)', 'Yearly Solar Output (kWh)', 'Annual Levelized Cost ($/kWh)', 'Electricity Cost ($/kWh)', 'Energy Purchased (kWh)', 'Grid Electricity Cost ($)', 'Annual Solar Savings ($)')
    
    
    # These are monthly tables that are commented out but still included in case approach changes
    # #ac_monthly
    # ac_monthly_df <- data.frame(months, Solar_Data_Monthly$output$ac_monthly)
    # colnames(ac_monthly_df) <- c('Month', 'AC System Output (kWh)')
    # #solrad_monthly
    # solrad_monthly_df <- data.frame(months, Solar_Data_Monthly$output$solrad_monthly)
    # colnames(solrad_monthly_df) <- c('Month', 'Solar Radiation (kWh/m2/day)')
    # #dc_monthly
    # dc_monthly_df <- data.frame(months, Solar_Data_Monthly$output$dc_monthly)
    # colnames(dc_monthly_df) <- c('Month', 'DC System Output (kWhdc)')
    # #dc_monthly
    # poa_monthly_df <- data.frame(months, Solar_Data_Monthly$output$poa_monthly)
    # colnames(poa_monthly_df) <- c('Month', 'POA Incidence (kWh/m2)')
    
    
#### MONTHLY TABLE RENDERING ####
    # output$ac_monthly <- renderTable({
    #   ac_monthly_df
    # })
    # output$solrad_monthly <- renderTable({
    #   solrad_monthly_df
    # })
    # output$dc_monthly <- renderTable({
    #   dc_monthly_df
    # })
    # output$poa_monthly <- renderTable({
    #   poa_monthly_df
    # })
    
    # MAIN TABLE ####
    output$main_df <- renderDataTable({
      DT::datatable(
        df[2:8], # Selecting columns to be displayed
        extensions = 'Buttons',
        options = list(
          dom = 'tpB',
          lengthMenu = list(c(0, 15, -1), c('0', '15', 'All')), # Button options
          pageLength = 15,
          buttons = list(
            list(
              extend = "collection",
              text = 'Show All',
              action = DT::JS("function ( e, dt, node, config ) {
                              dt.page.len(-1);
                              dt.ajax.reload();}")
            ),list(
              extend = "collection",
              text = 'Show Less',
              action = DT::JS("function ( e, dt, node, config ) {
                              dt.page.len(15);
                              dt.ajax.reload();}")
              
            )
          ),
          columnDefs = list(list(className = 'dt-right', targets = "_all"))
        )
      )
    })
    
    
    # Render Tiles
    output$ac_annual_tile <- renderValueBox({
      valueBox(value = tags$p("Annual AC System Output: ", style = "font-size: 80%;"), 
               subtitle = tags$p(paste0(round(Solar_Data_Monthly$output$ac_annual, digits = 2), " (kWh)"), style = "font-size: 200%;"))
    })
    output$capacity_factor_tile <- renderValueBox({
      valueBox(value = tags$p("Capacity Factor: ", style = "font-size: 80%;"), 
               subtitle = tags$p(paste0(round(Solar_Data_Monthly$output$capacity_factor, digits = 2)), style = "font-size: 200%;"))
    })
    output$energy_purchase <- renderValueBox({
      valueBox(value = tags$p("Energy Purchased: ", style = "font-size: 80%;"), 
               subtitle = tags$p(paste0(round(sum(df$`Energy Purchased (kWh)`),2)," kWh"), style = "font-size: 200%;"))
    })
    output$levelized_cost <- renderValueBox({
      valueBox(value = tags$p("Lifetime Levelized Cost:", style = "font-size: 80%;"), 
               subtitle = tags$p(paste0('$', round((energy_mix[1]*(capital_cost + installation_cost))/(sum(yearly_solar_output)), digits = 3)), style = "font-size: 200%;"))
    })
    output$panel_purchase <- renderValueBox({
      valueBox(value = tags$p("Optimal Panels to Purchase: ", style = "font-size: 80%;"), 
               subtitle = tags$p(paste0(energy_mix[1]), style = "font-size: 200%;"))
    })
    output$solar_energy_provided <- renderValueBox({
      valueBox(value = tags$p("Solar Energy Provided: ", style = "font-size: 80%;"), 
               subtitle = tags$p(paste0(sum(df$`Yearly Solar Output (kWh)`)," kWh"), style = "font-size: 200%;"))
    })
    output$solar_savings_tile_lifetime <- renderValueBox({
      valueBox(value = tags$p("Lifetime Savings from solar:", style = "font-size: 80%;"),
               subtitle = tags$p(paste0('$', sum(df$`Annual Solar Savings ($)`)), style = "font-size: 200%;"))
    })
    output$solrad_annual_tile <- renderValueBox({
      valueBox(value = tags$p("Annual solar radiation values: ", style = "font-size: 80%;"), 
               subtitle = tags$p(paste0(round(Solar_Data_Monthly$output$solrad_annual, digits = 2), " (kWh/m2/day)"), style = "font-size: 200%;"))
    })
    output$station_distance_tile <- renderValueBox({
      valueBox(value = tags$p("Closest station: ", style = "font-size: 80%;"), 
               subtitle = tags$p(paste0(station_distance, " (mi)"), style = "font-size: 200%;"))
    })
    output$total_cost <- renderValueBox({
      total_cost = round(energy_mix[1]*(capital_cost + installation_cost) + sum(df$`Grid Electricity Cost ($)`),0)
      valueBox(value = tags$p("Total Cost: ", style = "font-size: 80%;"), 
               subtitle = tags$p(paste0('$',total_cost), style = "font-size: 200%;"))
    })
    output$total_grid_cost <- renderValueBox({
      valueBox(value = tags$p("Total Grid Cost: ", style = "font-size: 80%;"), 
               subtitle = tags$p(paste0('$',sum(df$`Grid Electricity Cost ($)`)), style = "font-size: 200%;"))
    })
    output$total_solar_cost <- renderValueBox({
      total_solar_cost = round(energy_mix[1]*panel_cost_per_year,2)
      valueBox(value = tags$p("Average Yearly Optimal System Cost: ", style = "font-size: 80%;"), 
               subtitle = tags$p(paste0('$',total_solar_cost), style = "font-size: 200%;"))
    })
    
    # Plots
    # Create a plot showing degradation & energy bought
    output$degradation_plot <- renderPlot({
      # Visualize the data
      ggplot(df,aes(x = Year)) +
        geom_line(aes(y=`Energy Demand (kWh)`, color = "Energy Demand (kWh)")) + 
        geom_line(aes(y=`Yearly Solar Output (kWh)`, color = "Yearly Solar Output (kWh)")) + 
        labs(title = paste0("Energy Demand and Solar Output over Time "),
             caption = paste0("")) +
        ylab("Energy (kWh)") + xlab("Year") + labs(color = "Legend") + 
        scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
        theme_bw(base_size = 15) + 
        scale_color_manual(values = c("Energy Demand (kWh)" = "black", "Yearly Solar Output (kWh)" = "dark orange"))
    })
    # Create a plot showing degradation & energy bought
    output$annual_levelized_plot <- renderPlot({
      # Visualize the data
      ggplot(df,aes(x = Year)) +
        geom_line(aes(y = `Electricity Cost ($/kWh)`, color = "Electricity Cost ($/kWh)")) +
        geom_line(aes(y = `Annual Levelized Cost ($/kWh)`, color = "Annual Levelized Cost ($/kWh)")) +
        labs(title = paste0("Levelized Cost and Electricity Cost over Time"),
             caption = paste0("")) +
        ylab("Energy Cost ($/kWh)") + xlab("Year") + labs(color = "Legend") +
        scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
        theme_bw(base_size = 15) + 
        scale_color_manual(values = c("Electricity Cost ($/kWh)" = "red", "Annual Levelized Cost ($/kWh)" = "green"))
    })
    
    
  }) 
}

shinyApp(ui, server)