library(shiny)
library(shinydashboard)
library(DT)
library(gridExtra)
library(ggplot2)

css <- "body > div > header > nav {margin-left:25% !important}
        body > div > header > span {width:25% !important}
        #DataTables_Table_0 > thead > tr > th:nth-child(1) { text-align:center;}
        .myClass {
          color:white;
          overflow: hidden;
          padding: 0 15px;
          font-size: 20px;
          margin-left: 25%;
          text-align: left;
          line-height: 50px;
        }
        #last-row {margin-top: 5%}
"
js <- '
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass" style = ""> Surface </span>\');
      })
'

gdp_quarter <- tabPanel("Quarter",
                        tabPanel("quarter_type_panel",fluid = T,
                                 fluidRow(
                                   column(width=4,
                                          selectInput(inputId="quarterly_indicator", label = "indicator",
                                                      choices = c("GDP","Industry","Agriculture",
                                                                  "Construction","Trade","Services"),
                                                      multiple = T, selected = "GDP")
                                   ),
                                   column(width=4,
                                          selectInput(inputId="quarter_type", label = 'Indicator type',
                                                      choices = c("Growth (%)" = 1,
                                                                  "Absolute value (mil AMD)"= 2),
                                                      selected = 1, multiple = T)
                                   ),
                                   column(width=4,
                                          selectInput(inputId = "gdp_quarter_quarter",label="Choose quarter",
                                                      choices=  c("I" = "Q1",
                                                                  "II" = "Q2",
                                                                  "III" = "Q3",
                                                                  "IV" = 'Q4'),
                                                      multiple = T,
                                                      selected ="Q1")
                                   )
                                 ),
                                 fluidRow(
                                   column(width=4,
                                          selectInput(inputId = "gdp_quarter_year",label="Choose year",
                                                      choices= c("2020", "2019","2018","2017","2016","2015",
                                                                 "2014","2013","2012","2011","2010",
                                                                 "2009","2008","2007","2006","2005",
                                                                 "2004","2003","2002", "2001"),multiple = TRUE,selected = "2020"))
                                 )),
                        fluidRow(column(DTOutput("mytable3_quarter"), width = 12, offset=0)),
                        
                        fluidRow(id="last-row",
                                 column(width=4,downloadButton("downloadData1", "Download as EXCEL")),
                                 column(width=4),
                                 column(width=4,h4('Surface DataBase',style="text-align:right"))
                        )
)

gdp_yearly <- tabPanel("Yearly",
                       tabPanel("year_type_panel",fluid = T,
                                fluidRow(
                                  column(width=4,
                                         selectInput(inputId="yearly_indicator", label = "indicator",
                                                     choices = c("GDP","Industry","Agriculture",
                                                                 "Construction","Trade","Services"),
                                                     multiple = T, selected = "GDP")
                                  ),
                                  column(width=4,
                                         selectInput(inputId="yearly_type", label = 'Indicator type',
                                                     choices = c("Growth (%)" = 1,
                                                                 "Absolute value (mil AMD)"= 2),
                                                     selected = 1, multiple = T)),
                                  column(width=4,
                                         selectInput(inputId = "gdp_yearly_year",label="Choose year",
                                                     choices= c("2019","2018","2017","2016","2015",
                                                                "2014","2013","2012","2011","2010",
                                                                "2009","2008","2007","2006","2005",
                                                                "2004","2003","2002", "2001")
                                                     ,multiple = TRUE,selected = "2019"))
                                )
                       ),
                       fluidRow(column(DTOutput("mytable3_yearly"), width = 12, offset=0)),
                       
                       fluidRow(id="last-row",
                                column(width=4,downloadButton("downloadData2", "Download as EXCEL")),
                                column(width=4),
                                column(width=4,h4('Surface DataBase',style="text-align:right"))
                       )
)

GDP <- tabPanel('GDP',
                tabsetPanel(
                  gdp_quarter,
                  gdp_yearly 
                )
)

Export_Monthly <- tabPanel("Monthly",
                           fluidRow(
                             column(width=4,uiOutput('import_export_indicator1')),
                             column(width=4,selectInput(inputId="import_export_indicator_selector_type", label = "indicator type",
                                                        choices = c("Tons" = 0,
                                                                    "Dollars" = 1),
                                                        multiple = T,selected = 1)),
                             column(width=4,selectInput(inputId = "import_export_month",label="Choose month",
                                                        choices=  c(12:1),
                                                        multiple = T,selected = 11))
                           ),
                           fluidRow(
                             column(width=4,offset = 4,
                                    uiOutput('ui_of_selector')
                             )
                           ),
                           fluidRow(column(DTOutput("mytable_export"), width = 12, offset=0)),
                           fluidRow(id="last-row",
                                    column(width=4,downloadButton("downloadData3", "Download as EXCEL")),
                                    column(width=4),
                                    column(width=4,h4('Surface DataBase',style="text-align:right"))
                           ))

Export_Commulative <- tabPanel("Cummulative",
                               fluidRow(
                                 column(width=4,uiOutput('import_export_indicator2')),
                                 column(width=4,selectInput(inputId="import_export_indicator_selector_type2", label = "indicator type",
                                                            choices = c("Tons" = 0,
                                                                        "Dollars" = 1),
                                                            multiple = T,selected = 1))),
                               fluidRow(
                                 column(width=4,offset = 4,
                                        uiOutput('ui_of_selector2')
                                 )),
                               fluidRow(column(DTOutput("mytable_export2"), width = 12, offset=0)),
                               fluidRow(id="last-row",
                                        column(width=4,downloadButton("downloadData4", "Download as EXCEL")),
                                        column(width=4),
                                        column(width=4,h4('Surface DataBase',style="text-align:right"))
                               )
)

Export <- tabPanel('Exprot Import',
         tabsetPanel(
           Export_Monthly,
           Export_Commulative
         )
)

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(title = "Economic Research Center"), 
    dashboardSidebar( disable = T), 
    dashboardBody(
      tags$head(tags$style(HTML(css))),
      tags$script(HTML(js)),
      
      fluidRow(
        tabBox(id ='first',width=12,
               GDP,
               Export
        )
      )
    )
  )
)

