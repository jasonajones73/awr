require(tidyverse)
require(scales)
require(markdown)

#Import data
commissioners <- read_csv("data/cc-21june2018.csv") 
voter <- read_csv("data/nc_voter_demographics.csv") 
voter[is.na(voter)] <- 0

###----

#Tidy Commissioners Data
nc_cc_party <- commissioners %>%
  mutate(County = toupper(County)) %>%
  group_by(County, `Party-VR`) %>%
  count(County, `Party-VR`) %>%
  spread(`Party-VR`, n) %>%
  mutate(GRE = 0, LIB = 0) %>%
  select(County, DEM, GRE, LIB, REP, UNA) %>%
  gather(`Party-VR`, n, DEM:UNA, factor_key=TRUE) %>%
  ungroup() %>%
  complete(County, `Party-VR`, fill = list(n = 0))

nc_cc_race <- commissioners %>%
  mutate(County = toupper(County)) %>%
  group_by(County, Race) %>%
  count(County, Race) %>%
  ungroup() %>%
  complete(County, Race, fill = list(n = 0)) %>%
  spread(Race, n) %>%
  mutate(A = 0, O = 0) %>%
  select(County, A, B, I, M, O, U, W) %>%
  gather(Race, n, A:W, factor_key=TRUE) %>%
  arrange(County)

nc_cc_eth <- commissioners %>%
  mutate(County = toupper(County)) %>%
  group_by(County, Ethnicity) %>%
  count(County, Ethnicity) %>%
  spread(Ethnicity, n) %>%
  mutate(HL = 0) %>%
  select(County, HL, NL, UN) %>%
  gather(Ethnicity, n, HL:UN, factor_key=TRUE) %>%
  ungroup() %>%
  complete(County, Ethnicity, fill = list(n = 0))

nc_cc_gender <- commissioners %>%
  mutate(County = toupper(County)) %>%
  group_by(County, Gender) %>%
  count(County, Gender) %>%
  spread(Gender, n)  %>%
  mutate(U = 0) %>%
  gather(Gender, n, F:U, factor_key=TRUE) %>%
  ungroup() %>%
  complete(County, Gender, fill = list(n = 0))

#Palettes
party_palette <- c("DEM" = "#2E86C1", "GRE" = "#17A589", "LIB" = "#D4AC0D", "REP" = "#CB4335", "UNA" = "#884EA0" )

race_palette <- c("A" = "#E74C3C", "B" = "#27AE60", "I" = "#9B59B6", "M" = "#1ABC9C",  "O" = "#F1C40F", "U" = "#C0392B", "W" = "#E67E22")

cc_gender_palette <- c("F" = "#9B59B6", "M" = "#1ABC9C", "U" = "#F4D03F")

voter_gender_palette <- c("Female" = "#9B59B6", "Male" = "#1ABC9C", "UnDisclosedGender" = "#F4D03F")

#Labels
party_labels <- c("DEM" = "Democrats", "REP" = "Republicans", "GRE" = "Green", "LIB" = "Libertarians", "UNA" = "Unaffiliated")

race_labels <- c("A" = "Asian", "B" = "Black", "I" = "Native American", "M" = "Multiracial", "O" = "Other", "U" = "Undisclosed", "W" = "White")

eth_labels <- c("HL" = "Hispanic-Latino", "NL" = "Not Hispanic-Latino", "UN" = "Undisclosed")

cc_gender_labels <- c("F" = "Female", "M" = "Male", "U" = "Undisclosed")

voter_gender_labels <- c("Female" = "Female", "Male" = "Male", "UnDisclosedGender" = "Undisclosed")

# Define Server

server = server <- function(input, output) {
  
  #County Commissioners - Party Affiliation
  output$cc_party <- renderPlot({
    
    county_cc_party <- reactive({
      req(input$county1)
      
      nc_cc_party %>%
        filter(County == input$county1) %>%
        mutate(cc_party_pct = (n/sum(n)))
      
    })
    
    ggplot(county_cc_party(), aes(x = `Party-VR`, y=cc_party_pct, fill = factor(`Party-VR`))) + 
      geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = party_palette) +
      scale_x_discrete(labels= party_labels) +
      scale_y_continuous(labels=percent, limits = c(0,1)) +
      labs(fill="Party", 
           x=NULL, 
           y=NULL, 
           title="County Commissioners") +
      theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust=1, size = 14)) +
      guides(fill=FALSE)
  })
  
  #Voters - Party Affiliation
  output$voter_party <- renderPlot({
    
    county_voter_party <- reactive({
      req(input$county1)
      voter %>%
        filter(county_desc == input$county1) %>%
        select(DEM, REP, GRE, LIB, UNA) %>%
        gather("DEM":"UNA", key = "variable", value = "value") %>%
        mutate(party_pct = (value/sum(value)))
      
    })
    
    ggplot(county_voter_party(), aes(x = variable, y=party_pct, fill = factor(variable))) + 
      geom_bar(width = 1, stat = "identity", position = position_dodge(preserve = "single")) +
      scale_fill_manual(values = party_palette) +
      scale_x_discrete(labels = party_labels) +
      scale_y_continuous(labels=percent, limits = c(0,1)) +
      labs(fill="Party", 
           x=NULL, 
           y=NULL, 
           title="Registered Voters") +
      theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust=1, size = 14)) +
      guides(fill=FALSE)
    
  })
  
  #County Commissioners - Race
  output$cc_race <- renderPlot({
    
    county_cc_race <- reactive({
      req(input$county2)
      
      nc_cc_race %>%
        filter(County == input$county2) %>%
        mutate(cc_race_pct = (n/sum(n)))
      
    })
    
    ggplot(county_cc_race(), aes(x = Race, y=cc_race_pct, fill = factor(Race))) + 
      geom_bar(stat = "identity", position = position_dodge2(preserve = "single"), fill = "#DC7633") +
      #scale_fill_manual(values = race_palette) +
      scale_x_discrete(labels= race_labels) +
      scale_y_continuous(labels=percent, limits = c(0,1)) +
      labs(fill="Race", 
           x=NULL, 
           y=NULL, 
           title="County Commissioners") +
      theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust=1, size = 14)) +
      guides(fill=FALSE)
  })
  
  #Voters - Race
  output$voter_race <- renderPlot({
    
    county_voter_race <- reactive({
      req(input$county2)
      voter %>%
        filter(county_desc == input$county2) %>%
        select(A, B, I, M, O, U, W) %>%
        gather("A":"W", key = "variable", value = "value") %>%
        mutate(race_pct = (value/sum(value)))
      
    })
    
    ggplot(county_voter_race(), aes(x = variable, y=race_pct, fill = factor(variable))) + 
      geom_bar(width = 1, stat = "identity", position = position_dodge(preserve = "single"), fill = "#DC7633") +
      scale_x_discrete(labels= race_labels) +
      scale_y_continuous(labels=percent, limits = c(0,1)) +
      labs(fill="Race", 
           x=NULL, 
           y=NULL, 
           title="Registered Voters") +
      theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust=1, size = 14)) +
      guides(fill=FALSE)
    
  })
  
  #County Commissioners - Ethnicity
  output$cc_eth <- renderPlot({
    
    county_cc_eth <- reactive({
      req(input$county3)
      
      nc_cc_eth %>%
        filter(County == input$county3) %>%
        mutate(cc_eth_pct = (n/sum(n)))
      
    })
    
    ggplot(county_cc_eth(), aes(x = Ethnicity, y=cc_eth_pct, fill = factor(Ethnicity))) + 
      geom_bar(stat = "identity", position = position_dodge2(preserve = "single"), fill = "#DC7633") +
      scale_x_discrete(labels= eth_labels) +
      scale_y_continuous(labels=percent, limits = c(0,1)) +
      labs(fill="Race", 
           x=NULL, 
           y=NULL, 
           title="County Commissioners") +
      theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust=1, size = 14)) +
      guides(fill=FALSE)
  })
  
  #Voters - Ethnicity
  output$voter_eth <- renderPlot({
    
    county_voter_eth <- reactive({
      req(input$county3)
      voter %>%
        filter(county_desc == input$county3) %>%
        select(HL, NL, UN) %>%
        gather("HL":"UN", key = "variable", value = "value") %>%
        mutate(eth_pct = (value/sum(value)))
      
    })
    
    ggplot(county_voter_eth(), aes(x = variable, y=eth_pct, fill = factor(variable))) + 
      geom_bar(width = 1, stat = "identity", position = position_dodge(preserve = "single"), fill = "#DC7633") +
      scale_x_discrete(labels= eth_labels) +
      scale_y_continuous(labels=percent, limits = c(0,1)) +
      labs(fill="Race", 
           x=NULL, 
           y=NULL, 
           title="Registered Voters") +
      theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust=1, size = 14)) +
      guides(fill=FALSE)
    
  })
  
  #County Commissioners - Gender
  output$cc_gender <- renderPlot({
    
    county_cc_gender <- reactive({
      req(input$county4)
      
      nc_cc_gender %>%
        filter(County == input$county4) %>%
        mutate(cc_gender_pct = (n/sum(n)))
      
    })
    
    ggplot(county_cc_gender(), aes(x = Gender, y=cc_gender_pct, fill = factor(Gender))) + 
      geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = cc_gender_palette) +
      scale_x_discrete(labels= cc_gender_labels) +
      scale_y_continuous(labels=percent, limits = c(0,1)) +
      labs(fill="Gender", 
           x=NULL, 
           y=NULL, 
           title="County Commissioners") +
      theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust=1, size = 14)) +
      guides(fill=FALSE)
  })
  
  #Voters - Gender
  output$voter_gender <- renderPlot({
    
    county_voter_gender <- reactive({
      req(input$county4)
      voter %>%
        filter(county_desc == input$county4) %>%
        select(Male, Female, Undisclosed) %>%
        gather("Male":"Undisclosed", key = "variable", value = "value") %>%
        mutate(gender_pct = (value/sum(value)))
      
    })
    
    ggplot(county_voter_gender(), aes(x = variable, y=gender_pct, fill = factor(variable))) + 
      geom_bar(width = 1, stat = "identity", position = position_dodge(preserve = "single")) +
      scale_fill_manual(values = voter_gender_palette) +
      scale_x_discrete(labels= voter_gender_labels) +
      scale_y_continuous(labels=percent, limits = c(0,1)) +
      labs(fill="Gender", 
           x=NULL, 
           y=NULL, 
           title="Registered Voters") +
      theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust=1, size = 14)) +
      guides(fill=FALSE)
    
  })
  
}