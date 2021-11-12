# server-expression-plot.R

runExp <- reactiveValues(runExpValue = FALSE)
# This function render a selectInput in expression paramters box ----

output$expressionParameters <- renderUI({
  tagList(
    textAreaInput(
      "expressionGeneList",
      "Paste Gene List",
      rows = 5,
      placeholder = "Input gene's name (first column in the dataset), one gene per line."
    ),
    do.call(actionBttn, c(
      list(
        inputId = "runExpression",
        label = "Generate Plot",
        icon = icon("play")
      ),
      actionBttnParams
    ))
  )
})

# Render plotly object of barplot -----
output$geneBarPlotExpression <- renderPlotly({
  if (length(variables$expressionData) > 0) {
    tcc <- variables$tccObject
    data <- variables$expressionData
    isolate({
      p <- list(0)
      xOrder <-
        data.frame(
          "name" = rownames(tcc$group),
          "group" = tcc$group$group
        ) %>% arrange(group)
      xform <- list(
        categoryorder = "array",
        categoryarray = xOrder$name,
        title = ""
      )

      for (i in 1:nrow(data)) {
        p[[i]] <- plot_ly(
          x = colnames(data),
          y = t(data[i, ]),
          color = xOrder[xOrder$name == colnames(data), ]$group,
          type = "bar",
          textposition = "auto",
          text = t(data[i, ]),
          insidetextfont = list(color = "white")
        ) %>%
          layout(
            annotations = list(
              x = 0.5,
              y = 1.1,
              text = row.names(data[i, ]),
              showarrow = F,
              xref = "paper",
              yref = "paper"
            ),
            showlegend = FALSE,
            xaxis = xform
          )
      }
      f <- list(0)
      j <- 1
      for (i in seq(1, nrow(data), 2)) {
        if (i + 1 <= nrow(data)) {
          f[[j]] <- subplot(p[[i]], p[[i + 1]])
        } else {
          f[[j]] <- subplot(p[[i]])
        }
        j <- j + 1
      }
      pp <- subplot(f, nrows = j - 1, margin = 0.05) %>%
        config(
          toImageButtonOptions = list(
            format = "svg",
            filename = "Expression_Plot"
          )
        )
      variables$expressionLevelBar <- pp
      pp
    })
  } else {
    return()
  }
})

# check if norm data is available here






# Render plotly object of boxplot -----
output$geneBoxPlotExpression <- renderPlotly({
  if (length(variables$expressionData) > 0) {
    tcc <- variables$tccObject
    data <- variables$expressionData
    isolate({
      p <- list(0)
      xOrder <-
        data.frame(
          "name" = rownames(tcc$group),
          "group" = tcc$group$group
        ) %>% arrange(group)

      for (i in 1:nrow(data)) {
        subdata <-
          data.frame("name" = rownames(t(data[i, ])), "value" = t(data[i, ])[, 1])
        subdata <- left_join(x = subdata, y = xOrder, by = "name")
        p[[i]] <- plot_ly(
          data = subdata,
          x = ~group,
          y = ~value,
          color = ~group,
          type = "box",
          boxpoints = "all",
          jitter = 0.3,
          pointpos = -1.8
        ) %>%
          layout(
            yaxis = list(rangemode = "tozero"),
            annotations = list(
              x = 0.5,
              y = 1,
              text = row.names(data[i, ]),
              showarrow = F,
              xref = "paper",
              yref = "paper"
            ),
            showlegend = FALSE
          )
      }
      f <- list(0)
      j <- 1
      for (i in seq(1, nrow(data), 2)) {
        if (i + 1 <= nrow(data)) {
          f[[j]] <- subplot(p[[i]], p[[i + 1]])
        } else {
          f[[j]] <- subplot(p[[i]])
        }
        j <- j + 1
      }
      pp <- subplot(f, nrows = j - 1, margin = 0.05) %>%
        config(
          toImageButtonOptions = list(
            format = "svg",
            filename = "Expression_Plot"
          )
        )
      variables$expressionLevelBox <- pp
      pp
    })
  } else {
    return()
  }
})

# Render plotly Bar and Box UI
output$geneBarPlotUI <- renderUI({
  if (runExp$runExpValue & !is.null(variables$expressionData)) {
    geneNum <- nrow(variables$expressionData)
    if (geneNum %/% 2) {
      height <- 300 * geneNum / 2
    } else {
      height <- 300 * (geneNum + 1) / 2
    }
    plotlyOutput("geneBarPlotExpression") %>% withSpinner()
  } else {
    helpText("Input gene list in [Expression Level Parameters] to generate barplot.")
  }
})

output$geneBoxPlotUI <- renderUI({
  if (runExp$runExpValue & !is.null(variables$expressionData)) {
    geneNum <- nrow(variables$expressionData)
    if (geneNum %/% 2) {
      height <- 300 * geneNum / 2
    } else {
      height <- 300 * (geneNum + 1) / 2
    }
    plotlyOutput("geneBoxPlotExpression") %>% withSpinner()
  } else {
    helpText("Input gene list in [Expression Level Parameters] to generate boxplot.")
  }
})

observeEvent(input$runExpression, {
  # Convert data for ploting -----
  runExp$runExpValue <- input$runExpression
  tcc <- variables$tccObject
  data <- data.frame(tcc$count)
  data <- data.frame(variables$norData) # tryout 
  data <-
    data[row.names(data) %in% unlist(strsplit(x = input$expressionGeneList, split = "[\r\n]")), ]

  if (nrow(data) == 0) {
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "Input list error, please make sure you have provided a correct list.",
      type = "error"
    )
    variables$expressionData <- NULL
    return()
  } else {
    variables$expressionData <- data
  }


# check for normed values availability
  print("the normed data:")
  print(head(variables$norData))
  # works
  # now isolate the wanted genes and plot these
  print("only the wanted genes normalized")
  print(variables$norData[rownames(variables$norData)%in% unlist(strsplit(x = input$expressionGeneList, split = "[\r\n]")),])

  print("thats what gets plotet:")
  print(data[row.names(data) %in% unlist(strsplit(x = input$expressionGeneList, split = "[\r\n]")), ])


  # Selected gene row count DataTable ----
  output$geneTable <- DT::renderDataTable({
    df <- data
    # Create 19 breaks and 20 rgb color values ranging from white to blue
    brks <-
      quantile(df %>% select_if(is.numeric),
        probs = seq(.05, .95, .05),
        na.rm = TRUE
      )

    DT::datatable(
      df,
      colnames = c("Gene Name" = 1),
      options = list(
        dom = "t",
        scrollX = TRUE
      )
    ) %>%
      formatStyle(names(df), backgroundColor = styleInterval(brks, head(Blues(40), n = length(brks) + 1)))
  })


  # Selected gene TCC result DataTable -----
  output$geneTableCal <- DT::renderDataTable({
    DT::datatable(
      resultTable()[resultTable()$gene_id %in% row.names(data), ],
      colnames = c(
        "Gene Name",
        "A Value",
        "M Value",
        "P Value",
        "Q Value (FDR)",
        "Rank",
        "estimated DEG"
      ),
      extensions = c("Buttons"),
      options = list(
        dom = "Bt",
        buttons =
          list(
            "copy",
            "print",
            list(
              extend = "collection",
              buttons = c("csv", "excel", "pdf"),
              text = "Download"
            )
          ),
        columnDefs = list(list(
          visible = FALSE, targets = -1
        ))
      )
    ) %>% formatRound(
      columns = c(
        "a.value",
        "m.value",
        "p.value",
        "q.value"
      ),
      digits = 3
    )
  })
  runExp$runExpValue <- input$runExpression
})
