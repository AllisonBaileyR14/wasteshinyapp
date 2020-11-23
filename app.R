#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(tidyverse)
library(shinythemes)
#library(hrbrthemes)
library(viridis)

# Here is the ui that defines our Waste Site. Notice, there are three tabs that will stage our interactive applications. Feel free to change titles and 
# descriptions as you please.


ui <- navbarPage("Waste Navigation Bar!",
                 theme = shinytheme("cyborg"),
                 tabPanel("About",
                          h1("What A Waste"),
                          p("Here is the main part to talk about our app!")),
                 tabPanel("Worldwide Landfills", # Becca this is your panel.  Take a look at my tabPanel
                          h1("Worldwide Landfills"),
                          p("Here is an interactive graph to visualize global landfill locations")), 
                 tabPanel("Waste Products by Country",
                          h1("Waste Products by Country"),
                          p("What a Waste is a global project to aggregate data on solid 
                            waste management from around the world. This database features 
                            the statistics collected through the effort, covering nearly all 
                            countries and over 330 cities. The metrics included cover all steps 
                            from the waste management value chain, including waste generation, 
                            composition, collection, and disposal, as well as information on user 
                            fees and financing, the informal sector, administrative structures, 
                            public communication, and legal information. The information presented 
                            is the best available based on a study of current literature and limited 
                            conversations with waste agencies and authorities. While there may be 
                            variations in the definitions and quality of reporting for individual 
                            data points, general trends should reflect the global reality. 
                            All sources and any estimations are noted."),
                          sidebarPanel("MSW Percentage by Country",
                                       selectInput(inputId = "country",
                                                   label = "Choose country:",
                                                   choices = list(
                                                     "Angola",
                                                     "Albania",
                                                     "Andorra",
                                                     "United Arab Emirates",
                                                     "Argentina",
                                                     "Armenia",
                                                     "American Samoa",
                                                     "Antigua and Barbuda",
                                                     "Australia",
                                                     "Austria",
                                                     "Azerbaijan",
                                                     "Burundi",
                                                     "Belgium",
                                                     "Benin",
                                                     "Burkina Faso",
                                                     "Bangladesh",
                                                     "Bulgaria",
                                                     "Bahrain",
                                                     "Bahamas, The",
                                                     "Bosnia and Herzegovina",
                                                     "Belarus",
                                                     "Belize",
                                                     "Bermuda",
                                                     "Bolivia",
                                                     "Brazil",
                                                     "Barbados",
                                                     "Brunei Darussalam",
                                                     "Bhutan",
                                                     "Botswana",
                                                     "Central African Republic",
                                                     "Canada",
                                                     "Switzerland",
                                                     "Channel Islands",
                                                     "Chile",
                                                     "China",
                                                     "C√¥te d‚ÄôIvoire",
                                                     "Cameroon",
                                                     "Congo, Dem. Rep.",
                                                     "Congo, Rep.",
                                                     "Colombia",
                                                     "Comoros",
                                                     "Cabo Verde",
                                                     "Costa Rica",
                                                     "Cuba",
                                                     "Curacao",
                                                     "Cayman Islands",
                                                     "Cyprus",
                                                     "Czech Republic",
                                                     "Germany",
                                                     "Djibouti",
                                                     "Dominica",
                                                     "Denmark",
                                                     "Dominican Republic",
                                                     "Algeria",
                                                     "Ecuador",
                                                     "Egypt, Arab Rep.",
                                                     "Eritrea",
                                                     "Spain",
                                                     "Estonia",
                                                     "Ethiopia",
                                                     "Finland",
                                                     "Fiji",
                                                     "France",
                                                     "Faeroe Islands",
                                                     "Micronesia, Fed. Sts.",
                                                     "Gabon",
                                                     "United Kingdom",
                                                     "Georgia",
                                                     "Ghana",
                                                     "Gibraltar",
                                                     "Guinea",
                                                     "Gambia, The",
                                                     "Guinea-Bissau",
                                                     "Equatorial Guinea",
                                                     "Greece",
                                                     "Grenada",
                                                     "Greenland",
                                                     "Guatemala",
                                                     "Guam",
                                                     "Guyana",
                                                     "Hong Kong SAR, China",
                                                     "Honduras",
                                                     "Croatia",
                                                     "Haiti",
                                                     "Hungary",
                                                     "Indonesia",
                                                     "Isle of Man",
                                                     "India",
                                                     "Ireland",
                                                     "Iran, Islamic Rep.",
                                                     "Iraq",
                                                     "Iceland",
                                                     "Israel",
                                                     "Italy",
                                                     "Jamaica",
                                                     "Jordan",
                                                     "Japan",
                                                     "Kazakhstan",
                                                     "Kenya",
                                                     "Kyrgyz Republic",
                                                     "Cambodia",
                                                     "Kiribati",
                                                     "St. Kitts and Nevis",
                                                     "Korea, Rep.",
                                                     "Kuwait",
                                                     "Lao PDR",
                                                     "Lebanon",
                                                     "Liberia",
                                                     "Libya",
                                                     "St. Lucia",
                                                     "Liechtenstein",
                                                     "Sri Lanka",
                                                     "Lesotho",
                                                     "Lithuania",
                                                     "Luxembourg",
                                                     "Latvia",
                                                     "Macao SAR, China",
                                                     "St. Martin (French part)",
                                                     "Morocco",
                                                     "Monaco",
                                                     "Moldova",
                                                     "Madagascar",
                                                     "Maldives",
                                                     "Mexico",
                                                     "Marshall Islands",
                                                     "Macedonia, FYR",
                                                     "Mali",
                                                     "Malta",
                                                     "Myanmar",
                                                     "Montenegro",
                                                     "Mongolia",
                                                     "Northern Mariana Islands",
                                                     "Mozambique",
                                                     "Mauritania",
                                                     "Mauritius",
                                                     "Malawi",
                                                     "Malaysia",
                                                     "Namibia",
                                                     "New Caledonia",
                                                     "Niger",
                                                     "Nigeria",
                                                     "Nicaragua",
                                                     "Netherlands",
                                                     "Norway",
                                                     "Nepal",
                                                     "Nauru",
                                                     "New Zealand",
                                                     "Oman",
                                                     "Pakistan",
                                                     "Panama",
                                                     "Peru",
                                                     "Philippines",
                                                     "Palau",
                                                     "Papua New Guinea",
                                                     "Poland",
                                                     "Puerto Rico",
                                                     "Portugal",
                                                     "Paraguay",
                                                     "West Bank and Gaza",
                                                     "French Polynesia",
                                                     "Qatar",
                                                     "Romania",
                                                     "Russian Federation",
                                                     "Rwanda",
                                                     "Saudi Arabia",
                                                     "Sudan",
                                                     "Senegal",
                                                     "Singapore",
                                                     "Solomon Islands",
                                                     "Sierra Leone",
                                                     "El Salvador",
                                                     "San Marino",
                                                     "Somalia",
                                                     "Serbia",
                                                     "South Sudan",
                                                     "S√£o Tom√© and Pr√≠ncipe",
                                                     "Suriname",
                                                     "Slovak Republic",
                                                     "Slovenia",
                                                     "Sweden",
                                                     "Eswatini",
                                                     "Sint Maarten (Dutch part)",
                                                     "Seychelles",
                                                     "Syrian Arab Republic",
                                                     "Turks and Caicos Islands",
                                                     "Chad",
                                                     "Togo",
                                                     "Thailand",
                                                     "Tajikistan",
                                                     "Turkmenistan",
                                                     "Timor-Leste",
                                                     "Tonga",
                                                     "Trinidad and Tobago",
                                                     "Tunisia",
                                                     "Turkey",
                                                     "Tuvalu",
                                                     "Tanzania",
                                                     "Uganda",
                                                     "Ukraine",
                                                     "Uruguay",
                                                     "United States",
                                                     "Uzbekistan",
                                                     "St. Vincent and the Grenadines",
                                                     "Venezuela, RB",
                                                     "British Virgin Islands",
                                                     "Virgin Islands (U.S.)",
                                                     "Vietnam",
                                                     "Vanuatu",
                                                     "Samoa",
                                                     "Kosovo",
                                                     "Yemen, Rep.",
                                                     "South Africa",
                                                     "Zambia",
                                                     "Zimbabwe"
                                                   ))),
                          mainPanel("Country MSW Graph",
                                    plotOutput(outputId = "country_plot"))),
                 tabPanel("Global Waste Index Scores",  # Lizzy, this is your tab.  Take a look at my tabPanel
                          h1("Global Waste Index Scores"),
                          p("Here is an interactive graph to visualize Global Waste Index Scores by country")))

# Allison's section for Tab 2. 

#####################################

# Get country_level_data_msw.csv

country_wide <- read_csv("country_level_data_msw.csv") %>%
  rename(
    "Organic Food" = composition_food_organic_waste_percent,
    "Glass" = composition_glass_percent,
    "Metal" = composition_metal_percent,
    "Paper" = composition_paper_cardboard_percent,
    "Plastic" = composition_plastic_percent,
  ) %>%
  dplyr::select(-composition_other_percent)

# Put the data in long format. 

country_long <- gather(country_wide, msw, percentage, 
                       "Organic Food":"Plastic", 
                       factor_key=TRUE) %>%
  dplyr::select(country_name, msw, percentage) %>%
  filter(!country_name%in%c("Aruba", "Afghanistan"))






# Define server logic required 
server <- function(input, output) { # do not touch this line. Place your reactive graphs in side here underneath mine. 
  
  country_select <- reactive({
    country_long %>%
      filter(country_name == input$country)
    
  })
  
  output$country_plot <- renderPlot({
    ggplot(data = country_select(), aes(x = msw, y = percentage, fill = msw)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      scale_fill_viridis_d()
  })
  # Place your reactive graph info here!!!
}


# Run the application 
shinyApp(ui = ui, server = server)