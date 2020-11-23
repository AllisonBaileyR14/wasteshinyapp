# attach packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(leaflet)
library(spData)
library(janitor)
library(sf)
library(shinythemes)
library(leaflet.extras)
library(RColorBrewer)
library(mapproj)
library(DT)
library(tmap)
library(mapview)
library(tmap)
library(maps)
library(mapdata)
library(rworldmap)
library(ggmap)
library(stringr)
library(readr)


# Adding in a shinytheme called "Superhero"


# Allison's section for Tab 1. 

#####################################

# Get country_level_data_msw.csv and prepare for a graph for each countries waste composition.

country_wide <- read_csv("country_level_data_msw.csv") %>%
  rename(
    "Organic Food" = composition_food_organic_waste_percent,
    "Glass" = composition_glass_percent,
    "Metal" = composition_metal_percent,
    "Paper" = composition_paper_cardboard_percent,
    "Plastic" = composition_plastic_percent) %>%
  dplyr::select(-composition_other_percent)

# Put the data in long format.

country_long <- gather(country_wide, msw, percentage, 
                       "Organic Food":"Plastic", 
                       factor_key=TRUE) %>%
  dplyr::select(country_name, msw, percentage) %>%
  filter(!country_name%in%c("Aruba", "Afghanistan"))

# Prepare a table that shows: country_name, gdp, population_population_number_of_people, waste_collection_coverage_total_percent_of_population, 
# waste_treatment_anaerobic_digestion_percent	waste_treatment_compost_percent	waste_treatment_controlled_landfill_percent	waste_treatment_incineration_percent,
# waste_treatment_landfill_unspecified_percent,
# waste_treatment_open_dump_percent waste_treatment_other_percent	waste_treatment_recycling_percent	,
# waste_treatment_sanitary_landfill_landfill_gas_system_percent	waste_treatment_unaccounted_for_percent	,
# waste_treatment_waterways_marine_percent

country_table <- country_wide %>%
  dplyr::select(country_name, population_population_number_of_people, gdp,
                waste_collection_coverage_total_percent_of_population,
                waste_treatment_anaerobic_digestion_percent,
                waste_treatment_compost_percent,
                waste_treatment_controlled_landfill_percent,
                waste_treatment_incineration_percent,
                waste_treatment_open_dump_percent,
                waste_treatment_recycling_percent,
                waste_treatment_sanitary_landfill_landfill_gas_system_percent) %>%
  rename(
    "Population" = population_population_number_of_people,
    "Country" = country_name,
    "GDP" = gdp,
    "Waste Collection for Population (%)" = waste_collection_coverage_total_percent_of_population,
    "Waste Treatment-Anaerobic Digestion (%)" = waste_treatment_anaerobic_digestion_percent,
    "Waste Treatment-Compost (%)" = waste_treatment_compost_percent,
    "Waste Treatment-Controlled Landfill (%)" = waste_treatment_controlled_landfill_percent,
    "Waste Treatment-Incineration (%)" = waste_treatment_incineration_percent,
    "Waste Treatment-Open Dump (%)" = waste_treatment_open_dump_percent,
    "Waste Treatment-Recycling (%)" = waste_treatment_recycling_percent,
    "Waste Treatment-Sanitary Landfill w/LGS (%)" = waste_treatment_sanitary_landfill_landfill_gas_system_percent)

# gather(waste_collection_coverage_total_percent_of_population,
#      waste_treatment_anaerobic_digestion_percent,
 #      waste_treatment_compost_percent,
  #     waste_treatment_controlled_landfill_percent,
   #    waste_treatment_incineration_percent,
    #   waste_treatment_open_dump_percent,
     #  waste_treatment_recycling_percent,
      # waste_treatment_sanitary_landfill_landfill_gas_system_percent, 
       #factor_key=TRUE)



# Allison's data wrangling complete
##################################################################
# Lizzy's Section for Tab 2

waste_index <- read_csv("global_waste_index_2019.csv") %>% 
  clean_names() %>% 
  dplyr::select(country, rank, waste_generated, 
                recycling, incineration, landfill, open_dump, 
                unaccounted_waste, final_score)

world <- read_sf(dsn = "TM_WORLD_BORDERS_SIMPL-0.3-1", 
                 layer = "TM_WORLD_BORDERS_SIMPL-0.3") %>%
  clean_names()

names(world)[names(world)=="name"] <- "country"

world_shape <- world %>% 
  dplyr::select(country)

wi_world <- merge(waste_index, world_shape, by.x = "country")
wi_sf <- st_as_sf(wi_world)

# Lizzy's data wrangling complete
##################################################################

# Becca's data wrangling

#Read in landfill.csv data

landfill_untidy <- readr::read_csv("landfill.csv")

#Tidy landfill_untidy:
#-clean_names() to convert column headers to lowercase snakecase 
#-use dplyr::select() to isolate columns for landfill id, name, all location info, ownership type, current status, and waste in space.
#-call out Unknown values as NA
#-drop all NA values and related observations

#Final map looks better when ownership type and current landfill status are united, so I created this dataframe to use for the final map
landfill_united <- landfill_untidy %>% 
  clean_names() %>% 
  select(landfill_name:waste_in_place_tons) %>% 
  mutate(ownership = 
           case_when(ownership_type == "Public" ~ "Public",
                     ownership_type == "Private" ~ "Private")) %>%
  select(-ownership_type) %>% 
  drop_na() %>% 
  unite(status_and_ownership, current_landfill_status, ownership, 
        sep = ",", remove = TRUE, na.rm = FALSE)


#Coerce lat/long data in landfill_tidy df into sf object
landfill_geo <- st_as_sf(landfill_united, coords = 
                           c("longitude", "latitude"))

#Same with landfill_united df
landfill_geo_united <- st_as_sf(landfill_united, coords = 
                                  c("longitude", "latitude"))
#Set crs data for each df
st_crs(landfill_geo) <- 4326
st_crs(landfill_geo_united) <- 4326

#Create sf layers by isolating each topic of interest: ownership type, current landfill status, waste in place (tons)

landfill_own <- landfill_geo %>%
  dplyr::select(status_and_ownership)

st_crs(landfill_own)

landfill_status <- landfill_geo %>%
  dplyr::select(status_and_ownership)


landfill_tons <- landfill_geo %>%
  dplyr::select(waste_in_place_tons)


#Test map landfill ownership
tmap_mode("view")
landfill_own_map <- tm_basemap("Esri.WorldTopoMap") +
  tm_shape(landfill_own) +
  tm_dots(col = "ownership",
          title = "Ownership Type")


#Test map landfill status
tmap_mode("view")
landfill_status_map <- tm_basemap("Esri.WorldTopoMap") +
  tm_shape(landfill_status) +
  tm_dots(col = "current_landfill_status",
          title = "Current Landfill Status")


#Test map landfill waste in tons
tmap_mode("view")
landfill_tons_map <- tm_basemap("Esri.WorldTopoMap") +
  tm_shape(landfill_tons) +
  tm_dots(labels = "id",
          col = "blue",
          size = "waste_in_place_tons")


#Final interactive map with all variables that will be featured in Shiny app
tmap_mode("view")
us_landfills_map <- tm_basemap("Esri.WorldTopoMap") +
  tm_shape(landfill_geo_united) +
  tm_dots(size = "waste_in_place_tons",
          col = "status_and_ownership",
          title = "Landfill Status & Ownership Type") +
  tm_legend()

#Create df's for waste % output plot
waste_by_perc <- landfill_united %>%
  select(state, waste_in_place_tons) %>% 
  group_by(state) %>%
  summarise(total_waste_tons = sum(waste_in_place_tons)) %>% 
  arrange(total_waste_tons) 




# Becca's data wrangling complete
##########################################################################


ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Global Waste"),
                    dashboardSidebar(
                      sidebarMenu(id = "menu",
                                  menuItem("Home Page", 
                                           tabName = "home", 
                                           icon = icon("forumbee")),
                                  menuItem("World Report", 
                                           tabName = "report", 
                                           icon = icon("drupal")),
                                  menuItem("Global Waste Products", 
                                           tabName = "waste_prod", 
                                           icon = icon("jedi")),
                                  menuItem("Global Waste Index", 
                                           tabName = "waste_index", 
                                           icon = icon("pastafarianism")),
                                  menuItem("U.S. Landfills Status", 
                                           tabName = "landfill_stat", 
                                           icon = icon("earlybirds")))),
                    dashboardBody(
                      fluidPage(theme = "lux.css"),
                      tabItems(
                        tabItem(
                          tabName = "home",
                          h1("Welcome to the Global Waste App"),
                          br(),
                          hr(),
                          p("See the tons of waste dumped globally per year at the World Counts",
                            a("Global Waste Count",
                              href = "https://www.theworldcounts.com/challenges/planet-earth/state-of-the-planet/world-waste-facts")),
                          hr(),
                          br(),
                          h3("World Bank Assessment of Global Waste Crisis:"),
                          HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/1CSm4GG2VrU" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                          br(),
                          hr(),
                          br(),
                          h3("About this app:"),
                          p("This app will show landfill locations worldwide, 
          waste use as a percentage of population and GDP by country, 
          and sorted percentage of waste by country. SuperFund site 
          data will be used to demonstrate communities that may be 
          affected by harmful contamination as a result of landfill management.
          By visualizing the relationships between countries and waste, the 
          app aims to show how specific countries are contributing to global 
          waste and how this may affect public health.", style = "font-size:15px")),
                        tabItem(
                          tabName = "report",
                          h1("Current State of Waste Affairs"),
                          br(),
                          hr(),
                          br(),
                          HTML('<p><img width="560 height="315" src="trashworld.png"/></p>'),
                          br(),
                          hr(),
                          h3("Global Waste Analysis:"),
                          p("Appropriate solid waste management is essential to human health, 
                          and waste-dumping sites come in many forms. Some communities have 
                          access to managed, engineered landfills, while others practice open 
                          dumping. Regardless of the method, even the most highly-monitored 
                          landfills can contribute to contamination and may be hazardous to 
                          human health. While it is recommended that communities focus on source 
                          reduction as a means of reducing the volume and toxicity of solid waste, 
                          this is often not practiced for a number of reasons.", style = "font-size:15px"),
                          p("In general, lower-income countries rely on open dumping. Three geographical 
                          regions are known to openly dump more than half of their waste: the Middle East 
                          and North Africa, Sub-Saharan Africa, and South Asia. This is exacerbated by the 
                          volume of solid waste entering these areas. Research has shown that 93 percent of
                          global waste is dumped in low-income countries, while only 2 percent is dumped in 
                          high-income countries. Upper-middle-income countries have the highest percentage of 
                          waste in landfills, at 54 percent. This rate decreases to 39 percent in high-income 
                          countries, with 36 percent of this waste diverted to recycling and composting.", style = "font-size:15px"),
                          p("As you explore this app, please take note that regardless of the waste-management 
                          practices implemented within these countries, solid waste will likely remain at these 
                          sites for thousands of years. Consider the sheer volume of waste that has been produced 
                          globally, and that this waste will not simply “go away”. Then ask yourself, is there 
                          one single-use item that I use daily that I can replace with a long-term use alternative?", style = "font-size:15px"),
                          p("Are you interested in learning more about waste reduction? Please start by visiting: ",
                            a("Zero Waste-San Diego,",
                              href = "https://zerowastesandiego.org/"),
                            a("National Geographic,",
                              href = "https://www.nationalgeographic.com/news/2018/05/zero-waste-families-plastic-culture/"),
                            a("and the Ellen MacArthur Foundation",
                              href = "https://www.ellenmacarthurfoundation.org/circular-economy/concept"), style = "font-size:15px")),
                        tabItem(
                          tabName = "waste_index",
                          h1("Global Waste Index"),
                          br(),
                          hr(),
                          br(),
                          fluidRow(
                            mainPanel(title = "Country Index Graph",
                                      p("These data were compiled by the World Bank. The Global Waste Index score weighs how 
                                        environmentally-friendly each of 36 countries waste management practices are. It ranks 
                                        the Organisation for Economic Co-operation and Development (OECD) countries on how they 
                                        manage their per capita waste.", style = "font-size:15px"),
                                      br(),
                                      hr(),
                                      br(),
                                      leafletOutput(outputId = "map"),
                                      p("Choose a country and see where it's ranked against other OECD countries"),
                                      shinydashboard::box(
                                        selectInput(inputId = "country_index",
                                                  label = "Choose Country:",
                                                  choices = c(unique(wi_sf$country)))),
                                      textOutput("results"))),
                          fluidRow(
                            br(),
                            hr(),
                            column(12, div(dataTableOutput("table"))))),      
                      tabItem(
                        tabName = "waste_prod",
                        h1("Global Waste Products and Management"),
                        br(),
                        hr(),
                        br(),
                        br(),
                        fluidRow(
                          infoBox("Global Waste", "2.01 billion tons", "Annual Global Waste", icon = icon("globe-asia"), color = "black"),
                          infoBox("Individual Waste", "10lbs", "Individual Waste per Day", icon = icon("trash"), color = "blue"),
                          infoBox("Waste MGMT.", "33%", "Environmentally Safe Management", icon = icon("fire"), color = "red")),
                        br(),
                        hr(),
                        br(),
                        p("A check box for different waste products (compost, landfill, glass recycling, paper recycling, metal recycling, etc.), 
                        and a select box for the user's country and/or state of choice. The outputs will show an interactive graph of the total 
                        volume of selected waste products by that state/country in comparison with other countries/states that the user could 
                        specify (or none) compared to to the top global producers of those waste products.", style = "font-size:15px"),
                        br(),
                        fluidRow(
                          shinydashboard::box(title = "Country Waste Graph",
                                              selectInput("country", 
                                                          "Choose Country:", 
                                                          choices = c(
                                                            unique(country_long$country_name)))),
                          shinydashboard::box(plotOutput(outputId = "country_plot")),
                          column(12, div(dataTableOutput("country_table"))))),
                      tabItem(
                        tabName = "landfill_stat",
                        h1("U.S. Landfill Exploration"),
                        br(),
                        hr(),
                        br(),
                        br(),
                                   p("Explore this map of the United States to identify landfill locations. 
                                      You can see whether the landfill is currently open or closed, 
                                      and whether it is publicly or privately owned. The size of 
                                      the dots representing each landfill shows the relative amount 
                                      of waste per landfill in tons. To compare quantities of waste 
                                      in landfills across states, choose state(s) by checking the box 
                                      to generate an outcome in the plot.", style = "font-size:15px"),
                        br(),
                        hr(),
                        br(),
                        fluidRow(
                          shinydashboard::box(plotOutput(outputId = "us_plot")),
                          shinydashboard::box(leafletOutput(outputId = "landfill_map"))),
                        fluidRow(
                            shinydashboard::box(title = "U.S. Landfills",
                                                checkboxGroupInput(inputId = "select_state",
                                                                   label = h3("Select State"),
                                                                   inline = TRUE,
                                                                   width = "400px",
                                                                   choices = c(
                                                                     unique(waste_by_perc$state))))))
                      
                      )
                    )
)

                                          




server <- function(input, output) {
  
  # Allison's Graph and Table
  ###################################
  
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
  
  output$country_table <- renderDataTable(country_table)
  
  # default global search value
  if (!exists("default_search")) default_search <- ""
  # default column search values
  if (!exists("default_search_columns")) default_search_columns <- NULL
  
  options = list(  # options
    scrollX = TRUE,
    searchCols = default_search_columns,
    class = "display nowrap compact", # style
    filter = "top",
    search = list(regex = FALSE, caseInsensitive = FALSE, search = default_search)
    # allow user to scroll wide tables horizontally
  )
  
  
  observe({ country_table
    # when it updates, save the search strings so they're not lost
    isolate({
      # update global search and column search strings
      default_search <- input$dataTable_search
      default_search_columns <- c("", input$dataTable_search_columns)
      
      # update the search terms on the proxy table (see below)
      proxy %>% updateSearch(keywords =
                               list(global = default_search, columns = default_search_columns))
    })
  })
  
  proxy <- dataTableProxy('country_table')
  
  # Allison's Graph and Table Complete
  ####################################
  
  
  # Lizzy's Map
  ###################################
  
  output$map = renderTmap({
    tm_shape(wi_sf) +
      tm_basemap("Hydda.Base")+
      tm_polygons("rank")
  })

  
  output$table <- renderDataTable(wi_world)
  
  # default global search value
  if (!exists("default_search")) default_search <- ""
  # default column search values
  if (!exists("default_search_columns")) default_search_columns <- NULL
  
  options = list(  # options
    scrollX = TRUE,
    searchCols = default_search_columns,
    class = "display nowrap compact", # style
    filter = "top",
    search = list(regex = FALSE, caseInsensitive = FALSE, search = default_search)
    # allow user to scroll wide tables horizontally
  )
  
  observe({ wi_world
    # when it updates, save the search strings so they're not lost
    isolate({
      # update global search and column search strings
      default_search <- input$dataTable_search
      default_search_columns <- c("", input$dataTable_search_columns)
      
      # update the search terms on the proxy table (see below)
      proxy %>% updateSearch(keywords =
                               list(global = default_search, columns = default_search_columns))
    })
  })
  
  proxy <- dataTableProxy('table')
  
  country_data <- reactive({
    wi_world %>%
      filter(country %in% input$country_index) %>% 
      pull(rank)
  })
  
  output$results = renderText({
    country_data()
  })
  
  
  # Lizzy's Map Complete
  ####################################
  
  # Becca's Map
  
  us_select <- reactive({
    waste_by_perc %>%
      dplyr::filter(state %in% input$select_state)
    
  })
  
  output$us_plot <- renderPlot({
    ggplot(data = us_select(),aes(x=state, y=total_waste_tons)) +
      geom_col() +
      labs(title = "Total Solid Waste by State (tons)",
           x = "State",
           y = "Total Waste (tons)") +
      theme_minimal() +
      scale_fill_viridis_c()
    
    
  })
  
  output$landfill_map = renderTmap({
    tmap_mode("view") +
      tm_basemap("Esri.WorldTopoMap") +
      tm_shape(landfill_geo_united) +
      tm_dots(size = "waste_in_place_tons",
              col = "status_and_ownership",
              title = "Landfill Status & Ownership Type") +
      tm_legend()
  })
  
}



shinyApp(ui = ui, server = server)



