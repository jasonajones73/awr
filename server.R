require(highcharter)

source("external.R")

# Define Server

server = server <- function(input, output) {
  
  #County Commissioners - Party Affiliation
  output$cc_party <- renderHighchart({
    
    county_party <- reactive({
      req(input$county1)
      
      party %>%
        filter(County == input$county1) %>%
        group_by(Type) %>%
        mutate(total = sum(n)) %>%
        ungroup() %>%
        mutate(party_pct = n/total) %>%
        rename(party_vr = `Party-VR`) %>%
        mutate(party_pct = round(party_pct*100, digits = 2))
      
    })
    
    hchart(county_party(), "column", hcaes(x = party_vr, y = party_pct, group = Type)) %>%
      hc_yAxis(
        title = list(text = "Party Percent"),
        labels = list(format = "{value}%"), max = 100
      ) %>%
      hc_xAxis(
        title = list(text = "")
      ) %>%
      hc_title(text = "Party Affiliation") %>%
      hc_tooltip(pointFormat = "<b>{point.y}%</b>",
                 headerFormat = "<span style='font-size:10px'>{series.name}</span><br/>") %>%
      hc_add_theme(hc_theme_google())
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