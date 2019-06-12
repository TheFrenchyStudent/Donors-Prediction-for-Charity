if(!require("shinydashboard")) install.packages("shinydashboard"); library("shinydashboard")
if(!require("readr")) install.packages("readr"); library("readr")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("rworldmap")) install.packages("rworldmap"); library("rworldmap")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")

basetable <- read_csv("basetable.csv")


# Defining the choices for selectInput beforehand for clarity
metrics <- c("Bets during the week-end" = "No_bets_weekend", "Bets during the week" = "No_bets_weekdays", 
             "Average spending per month"="avg_spending_per_month", "Total spending"="total_spent")

metrics2 <- c("Language"="Language", "Product"="Application", "Gender"="Gender", "Country"="Country")


ui <- dashboardPage(
  dashboardHeader(title = "Database overview"),
  # Allocating the user input to the sidebar
  dashboardSidebar(
    selectizeInput("xplot1", label="X-axis",
                   choices=metrics2, 
                   selected="Country"),
    
    selectizeInput("yplot1", label="Y-axis",
                   choices=metrics, 
                   selected="avg_spending_per_month"),
    
    sliderInput("sliderplot1", label="Number of observations plotted:", min=1, 
                max=max(sapply(sapply(basetable[, c("Language", "Application","Gender", "Country")], unique), length)), 
                value=10),
    
    selectizeInput("mapvar", label="Variable to map:",
                   choices=metrics, 
                   selected="total_spent")
    
  ),
  dashboardBody(
    fluidRow(
      box(plotOutput("plot1"), width=10)
      
    ),
    
    fluidRow(
      box(plotOutput("plotmap"), width=10)
    ),
    
    fluidRow(
      box(plotOutput("statplot1"), width=6),
      box(plotOutput("statplot2"), width=6),
      box(plotOutput("statplot3"), width=6),
      box(plotOutput("statplot4"), width=6)
    )
  )
)



server <- function(input, output) {

  output$plot1 <- renderPlot({
    # gby <- group_by_at(basetable, input$xplot1) %>% summarize_at(input$yplot1, mean, na.rm = TRUE) %>% arrange_at(input$yplot1, desc)
    # gby <- gby[1:input$sliderplot1,]
    # 
    # barplot(gby[[input$yplot1]], col="blue", ylab=input$yplot1, 
    #         main=paste(names(metrics2)[metrics2 == input$xplot1], "vs.", names(metrics)[metrics == input$yplot1]))
    # text(x=seq(1:input$sliderplot1), y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels=gby[[input$xplot1]], srt=45, adj=1, xpd=T)  
    
    sub <- group_by_at(basetable, input$xplot1) %>% 
      summarize_at(input$yplot1, mean, na.rm = TRUE) %>% 
      arrange_at(input$yplot1, desc)
    
    sub <- sub[1:input$sliderplot1,]
    
    sub[[input$xplot1]] <- factor(sub[[input$xplot1]], levels=sub[[input$xplot1]])
    ggplot(sub, aes_string(x=input$xplot1, y=input$yplot1)) +
      geom_bar(stat="identity", width=.8, fill="blue") +
      xlab(names(metrics2)[metrics2 == input$xplot1]) +
      ylab(names(metrics)[metrics == input$yplot1]) +
      theme_classic() +
      ggtitle(paste(names(metrics2)[metrics2 == input$xplot1], "vs.", names(metrics)[metrics == input$yplot1])) +
      theme(plot.title = element_text(hjust=.5), axis.text.x = element_text(angle = 75, hjust = 1))
  })
  
    
  output$plotmap <- renderPlot({
    gby2 <- group_by_at(basetable, "Country") %>% summarize_at(input$mapvar, mean, na.rm = TRUE) %>% arrange_at(input$mapvar, desc)
    
    mapdf <- joinCountryData2Map(gby2, joinCode='NAME', nameJoinColumn='Country')
    mapParams <- mapPolys(mapdf, nameColumnToPlot=input$mapvar, mapRegion='world',
                          missingCountryCol='dark grey', numCats=10, 
                          addLegend=TRUE, mapTitle=names(metrics)[metrics == input$mapvar],
                          oceanCol='light blue')
  })
  
  
  output$statplot1 <- renderPlot({
    # Gender repartition
    sub_gen <- as_tibble(table(basetable$Gender)) %>% arrange(desc(n))
    sub_gen <- sub_gen[1:10,]
    sub_gen$Var1 <- factor(sub_gen$Var1, levels=sub_gen$Var1)
    ggplot(sub_gen, aes(x=Var1, y=n)) +
      geom_bar(stat="identity", width=.8, fill="purple") +
      xlab("") +
      theme_classic() +
      ggtitle("Gender repartition") +
      theme(plot.title = element_text(hjust=.5), axis.text.x = element_text(angle = 75, hjust = 1))
  })
  
  output$statplot2 <- renderPlot({
    # Country repartition
    sub_country <- as_tibble(table(basetable[["Country"]])) %>% arrange(desc(n))
    sub_country <- sub_country[1:10,]
    sub_country$Var1 <- factor(sub_country$Var1, levels=sub_country$Var1)
    ggplot(sub_country, aes(x=Var1, y=n)) +
      geom_bar(stat="identity", width=.8, fill="green") +
      xlab("") +
      theme_classic() +
      ggtitle("Country repartition") +
      theme(plot.title = element_text(hjust=.5), axis.text.x = element_text(angle = 75, hjust = 1))
  })
  
  output$statplot3 <- renderPlot({
    # Total sales per product
    sub_prod <- group_by_at(basetable, "Product_used_most") %>% 
      summarize_at("total_spent", mean, na.rm = TRUE) %>% 
      arrange_at("total_spent", desc)
    sub_prod <- sub_prod[1:10,]
    sub_prod$Product_used_most <- factor(sub_prod$Product_used_most, levels=sub_prod$Product_used_most)
    ggplot(sub_prod, aes(x=Product_used_most, y=total_spent)) +
      geom_bar(stat="identity", width=.8, fill="orange") +
      xlab("Product") +
      ylab("Total Sales") +
      theme_classic() +
      ggtitle("Product performance") +
      theme(plot.title = element_text(hjust=.5), axis.text.x = element_text(angle = 75, hjust = 1))
  })
  
  output$statplot4 <- renderPlot({
    # Total sales per platform
    sub_prod <- group_by_at(basetable, "Application") %>% 
      summarize_at("total_spent", mean, na.rm = TRUE) %>% 
      arrange_at("total_spent", desc)
    sub_prod <- sub_prod[1:10,]
    sub_prod$Application <- factor(sub_prod$Application, levels=sub_prod$Application)
    ggplot(sub_prod, aes(x=Application, y=total_spent)) +
      geom_bar(stat="identity", width=.8, fill="red") +
      xlab("") +
      ylab("Total Sales") +
      theme_classic() +
      ggtitle("Platform performance") +
      theme(plot.title = element_text(hjust=.5), axis.text.x = element_text(angle = 75, hjust = 1))
  })
  
}

shinyApp(ui, server)
