# load packages
library(shiny)
library(ggplot2)
library(plotly)
library(chorddiag)
library(dplyr)
library(readr)
library(RColorBrewer)
library(grDevices)
library(DT)
library(sunburstR)



shinyServer(function(input, output) {
  
  
  
  ###################################################
  # load table 
  
  table1 <- read.csv("test_for_circos_alldf.csv")
  
  
  #################################################################
  
 
  # creeate table of taxonomic ranks
  
  output$tbl = renderDT(
    datatable(data.frame("Function"=c("Superkingdom","Kingdom","Phylum","Class","Order","Family","Genus","Species")),selection = list(mode="single",
                                                        selected=4)
    ),
    options = list(lengthChange = FALSE)
  )
  
  ##################################################################
  
  # function to create array of colours
  
  getPalette = colorRampPalette(brewer.pal(9, "Set1"))
  
  ##################################################################
  
  output$distPlot <- renderChorddiag({
    
    # level of selected taxonomic rank
    s<- input$tbl_rows_selected
    
    # extract functions and taxonomyform dataset
    chord_table <- data.frame(state=table1[,"Mol_function"],entity=table1[,c("Superkingdom","Kingdom","Phylum","Class","Order","Family","Genus","Species")[s]])#Lowest.Common.Ancestor
    
    # encode NA's as factors
    chord_table$state <- addNA(chord_table$state)
    levels(chord_table$state)[is.na(levels(chord_table$state))]<- "N/A"
    chord_table[is.na(chord_table$state),"state"] <- "N/A"
    
    # remove NA's from analysis
    chord_table2<- chord_table[chord_table$state!=""&chord_table$state!="N/A",]
    
    # ensure functions are factors
    chord_table3 <- data.frame("state" = as.factor(as.character(chord_table2$state)),"entity"=as.factor(as.character(chord_table2$entity)))
    
    # covert dataframe to tibble
    tib <- as_data_frame(chord_table3)
    
    # summarise the tibble
    mat_list<- tib %>% group_by(state,entity)%>%summarise(n=n())
    
    
    # let x be the list of possible functions
    x<- levels(mat_list$state)
    
    # define x outside the function environment
    x<<-x
    
    # let y bethe list of all taxa
    y<- levels(mat_list$entity)
    
    # create zero matrix of the dimensions of the functions and taxa
    m_1 <- matrix(0,nrow=length(x),ncol = length(y),dimnames = list(x,y))
    
    # convert the summary table back to a dataframe
    df<- as.data.frame(mat_list)
    
    # add the size of the links to the zero matrix
    for( i in 1:(length(df$n))){
      m_1[toString(df[i,1]),toString(df[i,2])]<-df[i,3]
    }
    
    #############################################
    
    # create table of functions for sunburst
    
    output$tbl2 = renderDT(
      datatable(data.frame("Function"=x),selection = list(mode="single",
                                                          selected=1)),
      options = list(lengthChange = FALSE)
    )
    
    ############################################
    
    # create the chord diagram (Circos)
    
    chorddiag(m_1,type = "bipartite",
              groupColors = substr(rainbow(nrow(m_1)+ncol(m_1)),0,7),
              groupnamePadding = 20,
              groupnameFontsize = 10,
              # categoryNames = T,
              categorynamePadding = 200,
              ticklabelFontsize = 10,
              tickInterval = 5,
              margin = input$margin)
    
  })
  
  ##################################################################
  
  # create sunburst diagram
  
  output$sunburst <- renderSunburst({
    
    # create variable of the selcted function
    t<- input$tbl2_rows_selected
    
    # Selct the taxonomic data corresponding tho that function
    seq1<-select(table1,Superkingdom:Species) 
    
    # convert the factors to strings
    seq1 <- data.frame(lapply(seq1, as.character),
                       stringsAsFactors=FALSE)
    
    # create placeholder dataframe
    seq2<-data.frame("order"=NULL)
    
    # Remove Nas from the data
    seq1.1 <- seq1[as.numeric(table1$Mol_function)==t& !is.na(table1$Mol_function),]
    
    # update the place holder to contain sequences of type: "...-class-order-family-..."
    for (i in 1:nrow(seq1.1)){
      
      seq2[i,"order"]<- paste(seq1.1[i,],collapse = "-")
    } 
    
    # convert to tibble
    seq2<- as.tbl(seq2)
    
    # summarise the tibble
    seq2 <-seq2 %>% group_by(order) %>% summarize(N=n())
    
    # create sunburst
    sunburst(seq2)
  })
  
})
