#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(Hmisc)
library(plotly)
library(grid)
hdma <- readRDS("Data.rds")

#options(shiny.launch.browser = TRUE)
#outputOptions(output, "reactStates", suspendWhenHidden=FALSE)
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
    #Filter by state
    output$reactStates <- renderUI({
        #if (input$theTabs == "marketSharebyState"){
         stateList = unique(as.character(hdma$State))
         selectInput("states", "Filter by state", stateList, multiple = TRUE)
        #}
    })
    
    #Filter by Competitors
    output$reactCompetitors <- renderUI({
        z <- hdma[, c('Loan_Amount_000', 'Respondent_Name_TS')] %>% 
            group_by(Respondent_Name_TS) %>%
            summarise(tLoan = sum(Loan_Amount_000)) %>%
            arrange(desc(tLoan))
        CompetitorList = z$Respondent_Name_TS
        #CompetitorList = unique(as.character(hdma$Respondent_Name_TS))
        selectInput("competitors", "Filter by competitor(s)", CompetitorList, multiple = TRUE)
    })
    
    # Text Summary
    #output$textDisplay <- renderText({
    output$textDisplay <- renderUI({
        p1 <-"<h2>App Description: </h2> <br/> <p> This is a simple application to help visualize the home loan data 
            in the 4 states surrounding and including Washington DC i.e.,DC, Maryland (MD), 
            Virginia (VA), Delaware (DE) and West Virginia (WV) </p> <p> 
        This application has two tabs 'Market Share by State' and 'Loan Amount Distribution'</p>"
        p2 <- "<h4>Tab 1: Market Share by State</h4> <br/> By default this tab 
        shows the total market size (from all participants) in the 5 states in scope 
        for this app. You can use the 'Filter by competitor(s)' combo box in the sidebar
        to breadown the market size in each region by competitors. The combo box 
        lists the competitors in descreasing order of total market share in these markets. 
        When data is available the graphs are draws for conventional and conforming Loans<br/>"
        p3 <- "<h4>Tab 2: Loan Amount Distribution</h4>  <br/> This tab displays the 
        distribution of overall loan amount data in the set. The slider 
        'Number of Quantile Groups' in the sidebar changes the number of quantile qroups
        that the data will be broken into, the slider 'Number of bins per quantile group', 
        histogram, and finally, combobox 'Filter by state' filters the data for one or 
        more of the selected states"
        HTML(paste(p1, p2, p3, sep = '<br/>'))
    })
    # Plot marketshare by state and year      
    output$marketShare <- renderPlotly({
        withProgress(message = 'Please wait',
                     detail = 'Drawing Plot...', value = 0,{
        #if (input$theTabs == "marketSharebyState"){
            incProgress(1/4)  
            if(!is.null(input$competitors)){
                y <- hdma[, c('Loan_Amount_000', 'State', 'As_of_Year', 
                              'Respondent_Name_TS', 'Conventional_Conforming_Flag')] %>%
                    filter(Respondent_Name_TS %in% input$competitors)
            } else {
                y <- hdma[, c('Loan_Amount_000','State','As_of_Year','Conventional_Conforming_Flag')]
            }
            incProgress(1/4)
             yG <- group_by(y,State,As_of_Year,Conventional_Conforming_Flag)
             yS <- summarise(yG,tLoans = sum(Loan_Amount_000))
             levels(yS$Conventional_Conforming_Flag)[levels(yS$Conventional_Conforming_Flag)=="Y"] <- "Conventional & Conforming"
             levels(yS$Conventional_Conforming_Flag)[levels(yS$Conventional_Conforming_Flag)=="N"] <- "Not Conventional & Conforming"
             incProgress(1/4)
             p <- ggplot(yS, aes(x=factor(As_of_Year), y = as.character(tLoans), 
                                 group = Conventional_Conforming_Flag)) + 
                 geom_line(aes(linetype=Conventional_Conforming_Flag, 
                               color = Conventional_Conforming_Flag), size=0.5) +
                 geom_point(colour="red", size=2, shape=21, fill="white") +
                 facet_wrap( ~ State, ncol=2, scales = "free_y")+
                 xlab("Year") + ylab("Market Size (thousands of US Dollars)") +
                 theme(legend.position="bottom", legend.title=element_blank(), 
                       axis.ticks = element_blank(),
                       axis.title.y=element_text(size=7,face="bold",angle = 90),
                       axis.title.x=element_text(size=7,face="bold",angle = 00),
                       axis.text=element_text(size=5), #axis.text.y = element_blank()
                       panel.spacing.x =  unit(0,"lines"),
                       panel.spacing.y =  unit(1,"lines")
                       )
                 
             incProgress(1/4)     
            ggplotly(p) 
        })
    })
# Plot the histogram of loan amount distributions      
    output$histPlot <- renderPlotly({
    withProgress(message = 'Please wait',
                   detail = 'Drawing Graph...', value = 0,{
                       
           if(!is.null(input$states)){
               x <- hdma[, c('Loan_Amount_000', 'State')] %>% filter(State %in% input$states)
           } else {
               #x <- data.frame(Loan_Amount_000 = hdma[, c('Loan_Amount_000','State')])
               x <- hdma[, c('Loan_Amount_000','State')]
           } 
        incProgress(1/4)  
        x$Quantile <- with(x, cut2(Loan_Amount_000, g=input$quantile))
        incProgress(1/4)
        nCols<-ifelse(input$quantile<=5,1,2)
        incProgress(1/4)
        q <- ggplot(x, aes(x=Loan_Amount_000)) + 
            geom_histogram(bins = input$bins) +
            facet_wrap( ~ Quantile, ncol=nCols, scales = "free")+
            xlab("Loan Amount (thousands of US Dollars)") + ylab("Count") +
            theme(panel.spacing = unit(2,"lines"),
                  axis.title.y=element_text(size=8,face="bold",angle = 90),
                  axis.title.x=element_text(size=8,face="bold",angle = 00),
                  axis.text=element_text(size=5))
        incProgress(1/4)    
        ggplotly(q)
    })
    
  })
  
  
  
})
