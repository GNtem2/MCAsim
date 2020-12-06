#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(plotly)

library(tidyverse)

#load("computer_long_format.Rda") #12/12/17
#load("computer_long_format020219.Rda") #19/1/18
load("computer_long_format060219.Rda") #

#Exp0 = 0,
###with opposite ICA occlusion
#Exp1 = c(m1Middle, icaTop2), ## Top segment of our split # ICA ##omit M1
#Exp10 = c(m1Middle, pipe.names[c("LeftM1", "LeftACA1")]),
#Exp11 = c(pipe.names["LeftSuperiorM2"]),
#Exp12 = c(pipe.names["LeftInferiorM2"])
#Exp2 = c(m1Middle, icaTop2, pipe.names["LeftM1"]),
#Exp3 = c(m1Middle, icaTop2, pipe.names["LeftACA1"]),
#Exp4 = c(m1Middle, icaTop2, pipe.names[c("LeftM1", "LeftACA1")]),
#Exp5 = c(icaTop2, pipe.names["LeftSuperiorM2"]),
#Exp6 = c(icaTop2, pipe.names["LeftInferiorM2"]),

### without opposite ica occlusion
#Exp7 = c(m1Middle), ## Top segment of our split # ICA
#Exp8 = c(m1Middle, pipe.names["LeftM1"]),
#Exp9 = c(m1Middle, pipe.names["LeftACA1"])


#isolate Exp0
#copy results across
#acomCathTreesRatioLong0<-acomCathTreesRatioLong %>%
 # filter(Experiment=="Exp0") %>%
  #mutate(flowratiostroke=round(flowrationostroke,2))

#remove Exp0
#acomCathTreesRatioLong01<-acomCathTreesRatioLong%>%
 # filter(Experiment!="Exp0")

#combine data
#acomCathTreesRatioLong1<-bind_rows(acomCathTreesRatioLong0,acomCathTreesRatioLong01) %>%
#mutate(flowratiostroke=round(flowratiostroke,2)) %>%
 # rename(flow=flowratiostroke) 

#acomCathTreesRatioLong1 < - rename(acomCathTreesRatioLong, flow=round(flowratiostroke,2))

acomCathTreesRatioLong1<- acomCathTreesRatioLong %>%
  mutate(flowratiostroke=round(flowratiostroke,2)) %>%
  rename(flow=flowratiostroke)

artery<-acomCathTreesRatioLong1 %>% filter (Experiment=="Exp0", diam.inter==0.001,diam.intra==.001,stenosis==0, ArteryName=="ACOM" | ArteryName=="LeftPCOM" |ArteryName=="LeftMCA_PosParietal"|ArteryName=="LeftACA_Paracentral")
write.csv(artery,file="artery.csv")

artery100<-acomCathTreesRatioLong1 %>% filter (Experiment=="Exp0", diam.inter==0.001,diam.intra==.001,stenosis==1, ArteryName=="ACOM" | ArteryName=="LeftPCOM" |ArteryName=="LeftMCA_PosParietal"|ArteryName=="LeftACA_Paracentral")
write.csv(artery100,file="artery100.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Computer Modelling Experiments for ECR - Circle of Willis"),
  wellPanel(
    helpText("The setting for this simulation is contralateral ICA occlusion and ipsilateral MCA occlusion. The investigator simulates the effect of moving a catheter up the left ICA to retrieve the clot in the left MCA. There is progressive narrowing of the left ICA with catheter advancement. The effect on the blood flow is displayed in the interactive table and graph. For the first tab-Catheter causing progressive stenosis, the viewer can control the Circle of Willis configuration, diameter of leptomeningeal  anastomoses (LA) and the experiments. For the tab-Moving site  of occlusion, the viewer can control the Circle of Willis configuration, the diamter of LA and the degree of stenosis.This paper is based on earlier works from our group to estimate the capacity of the leptomeningeal anastomoses. That paper can be accessed here."),
    #tags$h5("This paper is based on earlier works from our group to estimate the capacity of the leptomeningeal anastomoses. That paper can be accessed here")
    tags$a(href ="https://www.frontiersin.org/articles/10.3389/fneur.2014.00176/full","MCA model")
  ),
  #br(),
  column(12,
         tabsetPanel(
           tabPanel("Moving site of occlusion",plotlyOutput("distPlot")),
           tabPanel("Catheter causing progressive ICA stenosis",plotlyOutput("distPlot1")), 
           absolutePanel(top=440,left=80,
                         selectInput("Anat",
                                     "CoW configuration",
                                     c("classical CoW"="typical",
                                       "ACom missing"="Acom",
                                       "Left A1 missing"="LeftA1",
                                       "Left A1 P1 missing"="LeftA1LeftP1",
                                       "Bilateral fetal PCA"="LeftP1RightP1",
                                       "Bilateral fetal PCA and left A1 missing"="LeftA1LeftP1RightP1"
                                     ))),
           absolutePanel(top=440,left=380,
                         selectInput("Exp",
                                     "Experiments",
                                     c("No occlusion"="Exp0",
                                       "Occlude right ICA"="Exp1",
                                      "Occlude left M1 &right ICA"="Exp2",
                                       "Occlude left M1+A1 &right ICA"="Exp3",
                                       #"Occlude left M1+A1 &right ICA"= "Exp4",
                                       ##"Occlude left sup M2 & right ICA"="Exp5",
                                       ##"Occlude left inf M2 & right ICA"="Exp6",
                                       
                                       "Occlude left M1"="Exp7",
                                       #"Occlude left M1"="Exp8",
                                       #"Occlude left M1+A1"="Exp9",
                                       "Occlude left M1+A1"= "Exp10",
                                       "Occlude left sup M2"="Exp11",
                                       "Occlude left inf M2"="Exp12"
                                     ))),
           absolutePanel(top=440,left=680,
                         selectInput("Ste","Stenosis"
                                     ,c("0%"=0,
                                        "25%"=.25,
                                        "50%"=.5,
                                        "75%"=.75,
                                        "90%"=.9,
                                        "99%"=.99,
                                        "100%"=1))),
           absolutePanel(top=440,left=980,
                         selectInput("LA", "LA diameter",
                                     c("0.5 mm"=.0005,
                                       "1 mm"=.001,
                                       "1.5 mm"=.0015))),
           #tabPanel("Moving site of occlusion",plotlyOutput("distPlot")),
          tabPanel("Artery Model",htmlOutput("Art")),
           tabPanel("Data Table",DT::dataTableOutput("table1"))
         )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$table1 <- renderDataTable({datatable(filter(acomCathTreesRatioLong1, CoW == input$Anat, Experiment==input$Exp,diam.inter==input$LA, diam.intra==0.001),
                                              extensions = 'Scroller', 
                                              options = list(
                                                deferRender = TRUE,
                                                scrollY = 200,
                                                scroller = TRUE
                                              )
  )      
  }) 
  
  #anat<- filter(acomCathTreesRatioLong, CoW == input$Anat, diam.inter==0.001, diam.intra==0.001)
  output$distPlot1 <- renderPlotly({
    gg <- ggplot(filter(acomCathTreesRatioLong1, CoW == input$Anat, Experiment==input$Exp,diam.inter==input$LA, diam.intra==0.001), aes(flow, ArteryName,color = ArteryName)) +
      geom_point(aes(size=flow, frame = stenosis)) +xlim(-5,5)
      #scale_x_continuous()
    #+scale_fill_discrete(name=c(0, 0.25, 0.5, 0.75,0.9, 1.0))
    ggplotly(gg)
  })
  #
  output$distPlot <- renderPlotly({
    gg <- ggplot(filter(acomCathTreesRatioLong1, CoW == input$Anat, stenosis==input$Ste,diam.inter==input$LA, diam.intra==0.001), aes(flow, ArteryName,color = ArteryName)) +
      geom_point(aes(size=flow, frame = Experiment)) +xlim(-5,5)
    #scale_x_continuous()
    ggplotly(gg)
  })
  
  getPage<-function() {
    return(includeHTML("artery_3d_plotly.html"))
    #return(includeHTML("Exp1.html"))
  }
  output$Art<-renderUI({getPage()})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

