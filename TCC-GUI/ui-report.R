fluidPage(

  column(12,
         box(
           title = tagList(icon("play"),"GO Result Plot"),
           width = NULL,
           solidHeader = TRUE,
           status = "info",
           uiOutput("go_term_plot")
         ),

  ),




column(3,
       box(

         title = tagList(icon("cogs"), "GO Term analysis and Plot Parameters"),
         width = NULL,
         solidHeader = TRUE,
         status = "primary",
         tagList(
           selectInput(
             "OrganismGO",
             "choose Organism Name",
             c("Mouse" = "org.Mm.eg.db","Human" = "org.Hs.eg.db"
             )
           ),
           selectInput(
             "ontologyType",
             "choose GO Term Ontology Type",
             c(
               "Biological Process" = "BP",
               "Metabolic Function" = "MF",
               "Cellular Component" = "CC"
             #  "Pool All Ontologies" = "ALL"
               # "WAD" = "wad"
             )
           ),

           
           
           selectInput(
             "KeyType",
             "Choose ID Type of columns",
             c(
               "Gene list (Myc/MYC)" = "SYMBOL",
               "Entrez ID (51529)" = "ENTREZID",
               "RefSeq ID (NM_005130)" = "REFSEQ",
               "Ensembl ID (ENSG00000076242)" = "ENSEMBL"
               #  "Pool All Ontologies" = "ALL"
               # "WAD" = "wad"
             )
           ),
           
           popify(
             sliderTextInput(
               inputId = "N_Ranking_genes_to_use",
               label = "use Top x Ranking Genes for GO Term analysis",
               choices =  c(0:500),
               selected=c(50)
               
             ),
             title = "GeneNumber",
             content = '<p>Filter genes count',
             placement = "top"
           ),
         
           
           
           popify(
             sliderTextInput(
               inputId = "N_Ranking_genes_to_Plot",
               label = "use Top x Ranking GO Terms for GO Term Result Plot",
               choices =  c(1:30),
               selected=c(15)
               
             ),
             title = "TermNumber",
             content = 'Number of GO Terms in Plot',
             placement = "top"
           ),
           
           
           
           
           
           
           
           do.call(actionBttn, c(
             list(
               inputId = "GO_Term",
               label = "GO Term Analyis",
               icon = icon("play")
             ),
             actionBttnParams
           )),
          # DT::dataTableOutput("goSummation2")
         
         
       ))

),






column(12,
# add here the options to choose stuff
    box(
       title = tagList(icon("table"), "GO Result Table"),
        width = NULL,
        solidHeader = TRUE,
         status = "info",
       uiOutput("go_res")
    )
  )
)

