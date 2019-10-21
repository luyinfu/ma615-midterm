library(shiny)
library(magrittr)
library(ggplot2)              
library(esquisse)             
library(tidyr)
library(dplyr)
data <- readxl::read_xls("/Users/tsuyu/Downloads/Data\ science\ in\ R/project/World\ Bank\ Data/climate_change.xls",
                         sheet = "Data")
pop <- data %>% dplyr::filter(`Series code`=="SP.POP.TOTL")
urbanpop <- data %>% dplyr::filter(`Series code`=="SP.URB.TOTL")

Country_info <- readxl::read_xls("/Users/tsuyu/Downloads/Data\ science\ in\ R/project/World\ Bank\ Data/climate_change.xls", sheet="Country")
region <- Country_info %>% 
    filter(Region!="Aggregates") %>%
    select(`Country code`, `Country name`, Region)

data1 <- data %>% dplyr::filter(`Series code`=="SP.POP.TOTL" |`Series code`=="SP.URB.TOTL")

data2 <- data1 %>% dplyr::filter(`Series code`=="SP.POP.TOTL") %>%
    select(-`Country code`, -`Series code`, -`Series name`, 
           -SCALE, -Decimals, -`2011`) %>% 
    gather(`1990`, `1991`,`1992`, `1993`, `1994`, `1995`, `1996`, 
           `1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, 
           `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`,
           key="year", value="pop")
data3 <- data1 %>% dplyr::filter(`Series code`=="SP.URB.TOTL") %>%
    select(-`Country code`, -`Series code`, -`Series name`, 
           -SCALE, -Decimals, -`2011`) %>% 
    gather(`1990`, `1991`,`1992`, `1993`, `1994`, `1995`, `1996`, 
           `1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, 
           `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`,
           key="year", value="urbanpop")
data4 <- merge(data2, data3, by=c("Country name", "year"))
data4 <- dplyr::left_join(data4, region, by="Country name") %>% 
    mutate(perc=as.numeric(urbanpop)/as.numeric(pop)) %>%
    drop_na()
data5 <- data4 %>% 
  select(`Country name`, Region, year, perc) %>%
    spread(year, perc)
 


ui <- fluidPage(

    # Application title
    titlePanel("World Urban Population Percentage Comparison"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            varSelectInput("year1", "Select a base year(year1)", data5[3:23]),
            varSelectInput("year2", "Select a year to compare(year2)", data5[3:23]),
            soze=2
        ),
        
        mainPanel(
           plotOutput("Plot", click = "plot_click", hover = "plot_hover"),
           verbatimTextOutput("info"),
           width = 10
        )
    )
)


server <- function(input, output, session) {
      
    output$Plot = renderPlot({
        
        ggplot(data5)+
            geom_point(aes(x=!!input$year1, y=!!input$year2, color=Region))+
            labs(title="urban population comparison",
                 x =c("urban population percentage year1", input$year1),
                 y = "urban population percentage year2")+
            scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#F0E442",
                                        "#6600FF", "#CCFF00", "#CC79A7"))
    })
    output$info <- renderText({
        xy_str <- function(e) {
            if(is.null(e)) return("choose a point\n")
            paste0( 
            "urban population percantage is ", "\n",
            "year1: ", round(e$x, 4), "\n",
            "year2: ", round(e$y, 4), "\n")
        }
        
        paste0( xy_str(input$plot_click),xy_str(input$plot_hover)
        )
    })
}

shinyApp(ui = ui, server = server)
