GORun <- reactiveValues(go_results = FALSE)

# output$tccSummationUI2 <- renderUI({
#   if(tccRun$tccRunValue){
#     tagList(
#       DT::dataTableOutput("tccSummation2")
#     )} else {
#       helpText("Summary of TCC normalization will be shown after TCC computation.")
#     }
# })
#tcc <- variables$tccObject
#ready_for_go_term<-resultTable()

#output$tccSummation2 <- DT::renderDataTable({variables$result[order(variables$result$rank),]})

# resultTable <- reactive({
#   variables$result
#   print("trying to get to resulttable:")
#   print(head(variables$result,n=23))
#   results_sorted=variables$result[order(variables$result$rank),]
#   print("results sorted by rank:")
#   print(head(results_sorted),n=10)
# 
#   
#   
#   
#   
# })



observeEvent(input$GO_Term, {
  withProgress(message = "calculating GO Term enrichment...",{
  results_sorted=variables$result[order(variables$result$rank),]
  print("could do go term analysis now....")
  sigs_sorted=results_sorted
  head(sigs_sorted)
  # filter for most significant genes 
  
  only_sigs=sigs_sorted[sigs_sorted$rank<input$N_Ranking_genes_to_use,]
  # only need genenames
  print("head of top rank")
  print(head(only_sigs))
  sigs_names=only_sigs$gene_id
  
  go_results=enrichGO(gene = sigs_names, OrgDb = input$OrganismGO,keyType = input$KeyType, ont = input$ontologyType)
  print("go term results table:")
  print(head(as.data.frame(go_results)))
  GORun$s4=go_results
  
  results=as.data.frame(go_results)
  
  GORun$gores=12
 # go_run_value=1
  
  # show the data table and show a lot of plots!!!!!!!
  GORun$go_results=data.frame(go_results)
  
  # todo: add table, add plots
  
  print("go term results are finished!")
  })

})


output$go_res<- renderUI({

      tagList(fluidRow(column(
        12,
        downloadButton("downLoadGOtermtable", "Download GO Term Enrichment results (CSV)")
      )),
        
      (fluidRow(column(   
        12,
         DT::dataTableOutput("goSummation2")
      ))))

})

observeEvent(input$GO_Term, {
  observeEvent(GORun$go_results, {
    print("go_results observed, trying to render the df with go results now...")
    df <-data.frame(GORun$go_results)
    colnames(df) <-c("ID","Description","GeneRatio","BgRatio","pvalue","p.adjust","qvalue","geneID","Count" )
    output$goSummation2 <- DT::renderDataTable({GORun$go_results})

  })
})

observeEvent(input$GO_Term, {
  observeEvent(GORun$go_results, {
    output$go_term_plot<-renderUI({
      tagList(
        fluidRow(
          plotlyOutput("go_plot")
        )
      )
    })
  })
})

observeEvent(input$GO_Term, {
  observeEvent (GORun$go_results, {
    output$go_plot <- renderPlotly({
      dat_order=GORun$go_results[order(as.numeric(GORun$go_results$p.adjust)),]
      dat_head=head(dat_order,n=input$N_Ranking_genes_to_Plot)
      print("top selected hits from go term:")
      print(dat_head)
      xform=list(categoryorder="array",categoryarray= (dat_head$Description))
      plot_ly(data = dat_head, x=dat_head$p.adjust,y=dat_head$Description, type="bar")%>% layout(yaxis =xform,title ="GO Term Enrichment Result Plot",xaxis = list(title="p.value adj."))
    })
  })
})



output$downLoadGOtermtable<- downloadHandler(
  filename = function() {
    paste(
      Sys.Date(),
      input$normMethod,
      input$testMethod,
      input$iteration,
      input$fdr,
      input$floorpdeg,
      "GO_term_enrichment_results.csv",
      sep = "_"
    )
  },
  content = function(file) {
    write.csv(GORun$go_results, file)
  }
)

