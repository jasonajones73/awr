require(shinythemes)
require(highcharter)

# Create county character object
counties = c(
  "ALAMANCE",
  "ALEXANDER",
  "ALLEGHANY",
  "ANSON",
  "ASHE",
  "AVERY",
  "BEAUFORT",
  "BERTIE",
  "BLADEN",
  "BRUNSWICK",
  "BUNCOMBE",
  "BURKE",
  "CABARRUS",
  "CALDWELL",
  "CAMDEN",
  "CARTERET",
  "CASWELL",
  "CATAWBA",
  "CHATHAM",
  "CHEROKEE",
  "CHOWAN",
  "CLAY",
  "CLEVELAND",
  "COLUMBUS",
  "CRAVEN",
  "CUMBERLAND",
  "CURRITUCK",
  "DARE",
  "DAVIDSON",
  "DAVIE",
  "DUPLIN",
  "DURHAM",
  "EDGECOMBE",
  "FORSYTH",
  "FRANKLIN",
  "GASTON",
  "GATES",
  "GRAHAM",
  "GRANVILLE",
  "GREENE",
  "GUILFORD",
  "HALIFAX",
  "HARNETT",
  "HAYWOOD",
  "HENDERSON",
  "HERTFORD",
  "HOKE",
  "HYDE",
  "IREDELL",
  "JACKSON",
  "JOHNSTON",
  "JONES",
  "LEE",
  "LENOIR",
  "LINCOLN",
  "MCDOWELL",
  "MACON",
  "MADISON",
  "MARTIN",
  "MECKLENBURG",
  "MITCHELL",
  "MONTGOMERY",
  "MOORE",
  "NASH",
  "NEW HANOVER",
  "NORTHAMPTON",
  "ONSLOW",
  "ORANGE",
  "PAMLICO",
  "PASQUOTANK",
  "PENDER",
  "PERQUIMANS",
  "PERSON",
  "PITT",
  "POLK",
  "RANDOLPH",
  "RICHMOND",
  "ROBESON",
  "ROCKINGHAM",
  "ROWAN",
  "RUTHERFORD",
  "SAMPSON",
  "SCOTLAND",
  "STANLY",
  "STOKES",
  "SURRY",
  "SWAIN",
  "TRANSYLVANIA",
  "TYRRELL",
  "UNION",
  "VANCE",
  "WAKE",
  "WARREN",
  "WASHINGTON",
  "WATAUGA",
  "WAYNE",
  "WILKES",
  "WILSON",
  "YADKIN",
  "YANCEY"
)

# Define User Interface

ui <- navbarPage("Are We Represented?",
  theme = shinytheme("flatly"), collapsible = TRUE,
  tabPanel("Party",
    sidebarLayout(
      sidebarPanel(
        p("Does a county's board of commissioners reflect the demographics of the registered voters in the populations they serve?"),
        br(),
        selectInput("county1",
          label = "Choose a county to display",
          selectize = TRUE,
          choices = counties,
          selected = "ALAMANCE"
        ),
        submitButton("Update County")
      ),
      mainPanel(
          br(),
          br(),
          highchartOutput("cc_party")
        ) # fluidRow
      )
    ),
  tabPanel("Race",
    sidebarLayout(
      sidebarPanel(
        p("Does a county's board of commissioners reflect the demographics of the registered voters in the populations they serve?"),
        br(),
        selectInput("county2",
                    label = "Choose a county to display",
                    selectize = TRUE,
                    choices = counties,
                    selected = "ALAMANCE"
        ),
        submitButton("Update County")
      ),
      mainPanel(
        fluidRow(
          br(),
          br(),
          column(6, plotOutput("cc_race")),
          column(6, plotOutput("voter_race"))
        ) # fluidRow
      )
      )
    ),
  tabPanel("Ethnicity",
           sidebarLayout(
             sidebarPanel(
               p("Does a county's board of commissioners reflect the demographics of the registered voters in the populations they serve?"),
               br(),
               selectInput("county3",
                           label = "Choose a county to display",
                           selectize = TRUE,
                           choices = counties,
                           selected = "ALAMANCE"
               ),
               submitButton("Update County")
             ),
             mainPanel(
               fluidRow(
                 br(),
                 br(),
                 column(6, plotOutput("cc_eth")),
                 column(6, plotOutput("voter_eth"))
               ) # fluidRow
             )
           )
  ),
  tabPanel("Gender",
           sidebarLayout(
             sidebarPanel(
               p("Does a county's board of commissioners reflect the demographics of the registered voters in the populations they serve?"),
               br(),
               selectInput("county4",
                           label = "Choose a county to display",
                           selectize = TRUE,
                           choices = counties,
                           selected = "ALAMANCE"
               ),
               submitButton("Update County")
             ),
             mainPanel(
               fluidRow(
                 br(),
                 br(),
                 column(6, plotOutput("cc_gender")),
                 column(6, plotOutput("voter_gender"))
               ) # fluidRow
             )
           )
          ),
  tabPanel("Data Download",
           sidebarLayout(
             sidebarPanel(
               downloadButton("downloadData", "Download")
             ),
             mainPanel()
           )
        ),
  br(),
  br(),
  p("In North Carolina, county commissioners are elected to provide for the safety and welfare of residents within their counties and to administer social services, public health services and education programs in conjunction with the state. They are responsible for adopting an annual budget, establishing the property tax rate, providing funding for the construction of public school buildings, and enacting local ordinances. Each board appoints a county manager to administer and oversee the activities of county departments."),
  p("Commission boards range in member size from 5 to 9. County Commissioners are generally elected to 4 year terms. In some counties, all commissioners are elected during the same election cycle. In others, the election cycles are staggered among the members."),
  p(HTML(paste0("For information on registering to vote, ", a(href = "https://www.ncsbe.gov/Voters/Registering-to-Vote", "click here to visit the North Carolina State Board of Elections website.")))),
  p("Voter regisration data was sourced from a North Carolina State Board of Elections database last modified June 16, 2018. County Commissioner data was compiled from a  candidate listing spreadsheet received via FOIA to the NC State Board of Elections, the state voter registration database and county government websites.")
)

