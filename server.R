library(shiny)
library(shinythemes)
library(knitr)
library(rmarkdown)

## Github libraries
# install.packages("devtools")
# library(devtools)
# install_github("trestletech/shinyTable")
library(shinyTable)
# install_github("ppernot/rgumlib")
library(rgumlib)

# Todo
# - limit parsing to strings recognized as formulae/equations
# - Welch-Satterthwaite  


string2Expr = function(string) {
  #   dat <- try(do.call(subset, list(data,parse(text = string))), silent = TRUE)
  dat <- try(parse(text = string),silent = TRUE)
  if(!is(dat, 'try-error')) 
    return(dat)
  else
    return(NULL)
} 

string2Num = function(x) 
  as.numeric(eval(parse(text=eval(string2Expr(x)))))

checkCorrMat = function(x) {
  if(is.null(x))
    FALSE
  else {
    x = matrix(as.numeric(x),nrow=nrow(x))
    any(as.numeric(x) > 1 ) ||
      any(as.numeric(x) < -1) ||
      min(eigen(x, symmetric=TRUE, only.values=TRUE)$values) < 
      sqrt(.Machine$double.eps)
  }
  
}

availablePdfs=c('norm','unif','triangle',
                'arcsine','delta',
                'tnorm','lnorm','stud')

picWidth = '480px'

nrunMax = 1e4 # Max. number of MC runs, for online service


shinyServer(function(input, output, session) {
  
  # Initialize container for uncertain variables definition
  Inputs = reactiveValues(
    fExpr   = NULL,
    names   = NULL,
    nvars   = NULL,
    x.mu    = NULL,
    x.u     = NULL,
    x.df    = NULL,
    x.pdf   = NULL,
    corrMat = NULL,
    invalidMat = FALSE
  )
  
  clearAll = function (model=TRUE) {
    if(model) {
      Inputs$fExpr  <<- NULL
      Inputs$names  <<- NULL      
      Inputs$nvars  <<- NULL      
    }
    Inputs$x.mu   <<- NULL
    Inputs$x.u    <<- NULL
    Inputs$x.df   <<- NULL
    Inputs$x.pdf  <<- NULL
    Inputs$corrMat <<- NULL
    Inputs$invalidMat <<- FALSE
  }
  
  checkInputsSanity = function() {
    listIn = reactiveValuesToList(Inputs)
    # No NULL allowed, except corrMat
    listIn$corrMat=TRUE
    nulVec = unlist(lapply(listIn, is.null))
    noNull = !any(nulVec) 
    return(noNull)
  }
  
  # Update model and raise Variables panel
  observe({
    if (input$updateExpr != 0) {
      isolate({
        Fexpr=string2Expr(input$Fexpr)
        if(!is.null(Fexpr)) {
          Inputs$fExpr  <<- Fexpr
          Inputs$names  <<- all.vars(Inputs$fExpr)
          Inputs$nvars  <<- length(Inputs$names)  
          updateTabsetPanel(session, inputId="mainTabset", 
                            selected="variables")
        }
      })
    }
  })
  
  # Clear all
  observe({
    if (!is.null(input$clearAll) && input$clearAll !=0) {
      updateTextInput(session,"Fexpr",value='')
      clearAll()
    }
  })
  
  # Update variables
  observe({
    if (!is.null(input$updateVars) && input$updateVars != 0) {
      isolate({
        vars = Inputs$names
        Inputs$x.mu <<- sapply(paste0('input$',vars,'.mu' ), 
                               function(x) eval(string2Expr(x)) )
        Inputs$x.u  <<- sapply(paste0('input$',vars,'.u'  ), 
                               function(x) eval(string2Expr(x)) )
        Inputs$x.df <<- sapply(paste0('input$',vars,'.df' ), 
                               function(x) eval(string2Expr(x)) )
        Inputs$x.pdf<<- sapply(paste0('input$',vars,'.pdf'), 
                               function(x) eval(string2Expr(x)) )
      }) 
    }
  })
  
  # Clear Variables
  observe({
    if (is.null(input$clearVars) || input$clearVars ==0) return()
    
    clearAll(model=FALSE)
    isolate({
      nvars = Inputs$nvars
      x.mu  = rep('1',nvars)     
      x.u   = rep('1',nvars)     
      x.df  = rep('Inf',nvars)   
      x.pdf = rep('norm',nvars) 
      names(x.mu)=names(x.u)=names(x.df)=names(x.pdf)=Inputs$names
      for (var in Inputs$names) {
        for (type in c('mu','u','df','pdf')) {
          name=paste0(var,'.',type)
          value=eval(parse(text=paste0('x.',type,'[["',var,'"]]')))
          if(type == 'pdf')
            updateSelectInput(session,name,selected = value)
          else
            updateTextInput(session, name, value=value)
        }
      }      
    })
  })
  
  observe({
    if(is.null(input$modelInFile)) return(NULL)
    #     source(file=input$modelInFile$datapath)
    lines=readLines(con=input$modelInFile$datapath)
    clearAll()
    isolate({
      il = grep('fExpr=',lines)
      if (length(il) == 1) {
        eval(parse(text=lines[il]))
        updateTextInput(session,"Fexpr",value=fExpr)
        Inputs$fExpr  <<- fExpr
        Inputs$names  <<- all.vars(Inputs$fExpr)
        Inputs$nvars  <<- length(Inputs$names)  
      }
      il = grep('x.mu=',lines)
      if (length(il) == 1) {
        eval(parse(text=lines[il]))
        Inputs$x.mu  <<- x.mu
      }
      il = grep('x.u=',lines)
      if (length(il) == 1) {
        eval(parse(text=lines[il]))
        Inputs$x.u  <<- x.u
      }
      il = grep('x.df=',lines)
      if (length(il) == 1) {
        eval(parse(text=lines[il]))
        Inputs$x.df  <<- x.df
      }
      il = grep('x.pdf=',lines)
      if (length(il) == 1) {
        eval(parse(text=lines[il]))
        Inputs$x.pdf <<- x.pdf
      }
      il = grep('corrMat=',lines)
      if (length(il) == 1) {
        eval(parse(text=lines[il]))
        colnames(corrMat)= Inputs$names
        rownames(corrMat)= Inputs$names
        Inputs$corrMat <<- corrMat
        Inputs$invalidMat <<- checkCorrMat(corrMat)
      }
    })    
  })
  
  # Dynamic UI for variables input
  output$variables_ui = renderUI({
    if (input$updateExpr == 0 | 
        is.null(Inputs$fExpr)  ) 
      return(NULL)
    
    # Initialize values and build input fields
    isolate({
      nvars  = Inputs$nvars
      x.mu  = if(is.null(Inputs$x.mu))  rep('1',nvars)    else Inputs$x.mu 
      x.u   = if(is.null(Inputs$x.u))   rep('1',nvars)    else Inputs$x.u  
      x.df  = if(is.null(Inputs$x.df))  rep('Inf',nvars)  else Inputs$x.df 
      x.pdf = if(is.null(Inputs$x.pdf)) rep('norm',nvars) else Inputs$x.pdf
      names(x.mu)=names(x.u)=names(x.df)=names(x.pdf)=Inputs$names
      
      tabInput=list(HTML('<table cellpadding=1 cellspacing=0><tr><th>&nbsp;<th>Mean
                         <th>Std. Unc.<th>DoF<th>Distrib</tr>'))
      for (var in Inputs$names) {
        tabInput=c(tabInput,list(HTML(paste0('<tr><td><b>',var,'&nbsp;</td>'))))
        for (type in c('mu','u','df','pdf')) {
          name  = paste0(var,'.',type)
          value = eval(parse(text=paste0('x.',type,'[["',var,'"]]')))
          if(type == 'pdf')
            locInput = list(HTML('<td>'),
                            selectInput(name, 
                                        label = '', 
                                        availablePdfs, 
                                        selected = value),
                            HTML('</td></tr>'))    
          else
            locInput = list(HTML('<td>'),
                            tags$input(id    = name, 
                                       type  = "text", 
                                       value = value, 
                                       style = 'width: 80px;'),
                            HTML('</td>'))    
          
          tabInput = c(tabInput,locInput)
        }
      }
      tabInput = list(tabInput,HTML('</table>'))
      
      matInput = list(HTML('<table cellpadding=2 border=0>'))
      nvars = Inputs$nvars
      for (i1 in 1:nvars) {
        var1 = Inputs$names[i1]
        matInput=c(matInput,list(HTML('<tr>')))
        for (i2 in 1:nvars) {
          var2 = Inputs$names[i2]
          if ( i2 < i1 ) {
            locInput=list(HTML('<td>&nbsp;</td>'))
            
          } else if (i2 == i1) {
            locInput=list(
              HTML(
                paste0('<td align=center valign=middle width=50><b>',
                       var1,'</b></td>')
              )
            )                        
            
          } else {
            name  = paste0('c_',var1,'_',var2)
            value = if(is.null(Inputs$corrMat)) 0 else Inputs$corrMat[i1,i2]
            locInput = list(
              HTML('<td>'),
              tags$input(id = name, 
                         type = 'number', 
                         value = value, 
                         min = -1, max = 1, 
                         class = 'shiny-bound-input',
                         style = 'width: 50px;'),
              HTML('</td>')
            )                        
          }
          matInput=c(matInput,locInput)
        }
        matInput=c(matInput,list(HTML('</tr>')))
      }
      matInput=list(matInput,HTML('</table>'))
      
    })
    
    verticalLayout(
      tabsetPanel(
        tabPanel(
          title = "Parameters",
          wellPanel(
            tabInput,
            br(),
            fixedRow(
              column(
                6,
                offset = 1,
                actionButton("clearVars" ,
                             "Reset",
                             icon = icon("eraser")),
                actionButton("updateVars",
                             "OK",
                             icon = icon("check"))
              )
            )
          )
        ),
        tabPanel(
          title = "Correlations",
          wellPanel(
            matInput,
            uiOutput("matrixAlert"),
            br(),
            fixedRow(
              column(
                6,
                offset = 1,
                actionButton("clearMat" ,
                             "Reset",
                             icon = icon("eraser")),
                actionButton("updateMat",
                             "OK",
                             icon = icon("check"))
              )
            )
          )
        ),
        type = 'pills'
      ),
      actionButton("propagate",
                   "Propagate",
                   icon = icon("gears")
      )
    )
  })
  
  # Update Correlation Matrix
  observe({
    if (is.null(input$updateMat) || input$updateMat == 0) return()
    isolate({
      if(is.null(Inputs$names)) return()
      nvars= Inputs$nvars
      vars = Inputs$names
      corrMat <- diag(nvars)
      rownames(corrMat) = vars
      colnames(corrMat) = vars
      for (i1 in 1:(nvars-1)) {
        var1 = vars[i1]
        for (i2 in (i1+1):nvars) {
          var2 = vars[i2]
          name  = paste0('input$c_',var1,'_',var2)
          corrMat[i1,i2] =string2Num(name)
          corrMat[i2,i1] = corrMat[i1,i2]
        }
      }
      if (checkCorrMat(corrMat)) {
        Inputs$invalidMat <<- TRUE
        
      } else {
        Inputs$invalidMat <<- FALSE
        Inputs$corrMat <<- corrMat        
      }
    })    
  })
  
  observe({
    if (!is.null(input$clearMat) && input$clearMat != 0) {
      isolate({
        if(is.null(Inputs$names)) return()
        nvars= Inputs$nvars
        vars = Inputs$names
        corrMat <- diag(nvars)
        rownames(corrMat) = vars
        colnames(corrMat) = vars
        Inputs$corrMat <<- corrMat
        Inputs$invalidMat <<- checkCorrMat(corrMat)
        for (i1 in 1:(nvars-1)) {
          var1 = vars[i1]
          for (i2 in (i1+1):nvars) {
            var2 = vars[i2]
            name  = paste0('c_',var1,'_',var2)
            updateNumericInput(session,inputId=name, value=0)
          }
        }        
      })    
    }
  })
  
  output$matrixAlert = renderUI({
    if (Inputs$invalidMat)
      h5('Invalid Correlation Matrix',style="color:red")
    else
      return(NULL)
  })
  
  output$corrMat <- renderTable({
    if(is.null(Inputs$corrMat)) 
      return(NULL)
    if(sum(as.numeric(Inputs$corrMat[upper.tri(Inputs$corrMat, diag = FALSE)]))==0)
      return(NULL)
    matrix(as.numeric(Inputs$corrMat),nrow=Inputs$nvars)
  })
  
  observe({
    if (!is.null(input$propagate) && input$propagate != 0) 
      updateTabsetPanel(session, inputId="mainTabset", 
                        selected="results")
  })
  
  # Core calculation
  do_GUM <- reactive({
    if (!checkInputsSanity()) return(NULL)
    
    isolate({    
      # Get variables data
      fExpr = Inputs$fExpr
      x.mu  = sapply(Inputs$x.mu, string2Num)
      x.u   = sapply(Inputs$x.u,  string2Num)
      x.df  = sapply(Inputs$x.df, string2Num)
      x.pdf = Inputs$x.pdf   
      nvars = Inputs$nvars
      names(x.mu)=names(x.u)=names(x.df)=names(x.pdf)=Inputs$names
      
      if(is.null(Inputs$corrMat)) 
        x.cor=diag(nvars)
      else {
        x.cor=Inputs$corrMat
        #         print(x.cor)
        #         x.cor[lower.tri(x.cor)]=x.cor[upper.tri(x.cor)]
        x.cor=matrix(as.numeric(x.cor),nrow=nvars)           
      }
    })  
    
    # Propagate (reacts to "Propagation method" panel)
    method=input$method
    if( method == 'GUM') {
      gumCV(
        fExpr = fExpr,
        x.mu = x.mu,
        x.u = x.u,
        x.cor = x.cor,
        silent = TRUE
      ) 
      
    } else {
      ndig=as.numeric(input$ndigits)
      adapt = input$sampling == 'adapt'
      if(adapt) {
        if(input$adaptType == 's1')
          gumS1(
            fExpr = fExpr,
            x.mu = x.mu,
            x.u = x.u,
            x.df = x.df,
            x.pdf = x.pdf,
            x.cor = x.cor,
            ndig = ndig,
            adapt = adapt,
            silent = TRUE,
            nrunMax = nrunMax
          )
        else
          gumS2(
            fExpr = fExpr,
            x.mu = x.mu,
            x.u = x.u,
            x.df = x.df,
            x.pdf = x.pdf,
            x.cor = x.cor,
            ndig = ndig,
            silent = TRUE,
            nrunMax = nrunMax
          )
      } else {
        nrun = input$nMCrun
        gumS1(
          fExpr = fExpr,
          x.mu = x.mu,
          x.u = x.u,
          x.df = x.df,
          x.pdf = x.pdf,
          x.cor = x.cor,
          nrun = nrun,
          ndig = ndig,
          adapt = adapt,
          silent = TRUE,
          nrunMax = nrunMax
        )          
      }      
    }          
    
  }) 
  
  # Dynamic UI for Results output
  output$results_ui = renderUI({
    if (is.null(do_GUM())) return(NULL)
    
    if (input$method == 'GUM') {
      tabPanel(
        title="GUM",
        h4("Combination of Variances"),           
        tabsetPanel(
          id='tabGUM',
          selected=input$tabGUM,
          tabPanel(
            title="Summary",
            wellPanel(
              h5("Combined Uncertainty"), 
              textOutput("output_GUM_summary"),      
              h5("Enlarged Uncertainty"), 
              textOutput("output_GUM_EU"),  
              h5("Coverage Interval"),    
              textOutput("output_GUM_CI")
            )
          ),
          tabPanel(
            title="Budget Table",
            wellPanel(
              tableOutput("output_GUM")
            )
          ),
          type='pills'              
        )
      )
    } else {
      tabPanel(
        title="GUM_Supp1",
        h4("Combination of Distributions by Monte-Carlo"),                          
        tabsetPanel(
          id='tabSup1',
          selected=input$tabSup1,
          tabPanel(
            title="Summary",
            wellPanel(
              h5("Sample size"), 
              textOutput("output_GUM_size"),      
              h5("Combined Uncertainty"), 
              textOutput("output_GUM_summary"),      
              h5("Enlarged Uncertainty"), 
              textOutput("output_GUM_EU"),  
              h5("Coverage Interval"),    
              textOutput("output_GUM_CI")  
            )
          ),
          tabPanel(
            title="Convergence",
            wellPanel(
              h5("Cumulated Statistics"),
              plotOutput("output_cumPlot",
                         width=picWidth, 
                         height=picWidth)
            )
          ),
          tabPanel(
            title="Empirical CDF",
            wellPanel(
              h5("Empirical Cumulated Distribution"),
              plotOutput("output_ECIPlot",
                         width=picWidth, 
                         height=picWidth)
            )
          ),
          tabPanel(
            title="I/O Correlations",
            wellPanel(
              h5("Inputs/Ouput(s) Scatterplots and Rank 
                             Correlation Coefficients."),
              plotOutput("output_SAPlot",
                         width=picWidth, 
                         height=picWidth)
            )
          ),
          tabPanel(
            title="Variance Gradients",
            wellPanel(
              h5("Variance Gradients Sensitivity Analysis"),
              tableOutput("output_vgSA"),
              em("cf."),"Campanelli ",em("et al.")," (2013)",
              em("Meas. Sci. Tech. "),strong("24"),":025002"
            )
          ),
          type='pills'
        )
      )
    }  
  })
  output$debug <- renderText({
    print(paste0(reactiveValuesToList(Inputs),collapse=','))
  })
  output$output_Mod <- renderText({
    if (is.null(Inputs$fExpr)) return(NULL)
    paste0('Y = ',Inputs$fExpr)
  })
  output$output_variables <- renderTable({
    if (!checkInputsSanity()) return(NULL)   
    #     print(reactiveValuesToList(Inputs))
    isolate({    
      x.mu = sapply(Inputs$x.mu, string2Num)
      x.u  = sapply(Inputs$x.u,  string2Num)
      x.df = sapply(Inputs$x.df, string2Num)
      x.pdf= sapply(Inputs$x.pdf, function(x) x)  
      names(x.mu)=names(x.u)=names(x.df)=names(x.pdf)=Inputs$names
    })
    data.frame(
      MEAN = sprintf("%.1e",x.mu),
      SD   = sprintf("%.1e",x.u),
      DF   = x.df,
      PDF  = x.pdf,
      row.names = names(x.mu)
      )
  },
  rownames = TRUE,
  align = 'lrrrr'
  )
  
  output$output_GUM <- renderTable({
    if (is.null(do_GUM())) return(NULL)
    do_GUM()$budget
  })
  output$output_GUM_summary <- renderPrint({
    if (is.null(do_GUM())) return(NULL)
    G = do_GUM()
    uncPrint(G$y.mu,G$y.u) 
  })
  output$output_GUM_EU <- renderPrint({
    if (is.null(do_GUM())) return(NULL)
    G=do_GUM()
    F=2
    uncPrint(G$y.mu,G$y.u*F)    
  })
  output$output_GUM_CI <- renderPrint({
    if (is.null(do_GUM())) return(NULL)
    G=do_GUM()
    F=2
    if( isolate(input$method) == 'GUM') 
      CIPrint(G$y.mu,G$y.u,p=0.95,fac=F)   
    else
      CIPrint1(F*G$y.u,G$y.low,G$y.high,p=0.95)   
  })
  output$output_GUM_size <- renderPrint({
    if (is.null(do_GUM())) return(NULL)
    S=do_GUM()
    cat(nrow(S$Y))
  })
  output$output_SAPlot <- renderPlot({
    if (is.null(do_GUM())) return(NULL)
    S=do_GUM()
    SAPlot(cbind(S$X,S$Y))
  })
  output$output_ECIPlot <- renderPlot({
    if (is.null(do_GUM())) return(NULL)
    S=do_GUM()
    ECIPlot(S$Y,p=0.95,cex=1)
  })
  output$output_cumPlot <- renderPlot({
    if (is.null(do_GUM())) return(NULL)
    S=do_GUM()
    cumPlot(S$Y,p=0.95,cex=1)
  })
  output$output_vgSA <- renderTable({
    if (is.null(do_GUM())) return(NULL)
    isolate({    
      # Get variables data
      fExpr= Inputs$fExpr
      x.mu = sapply(Inputs$x.mu, string2Num)
      x.u  = sapply(Inputs$x.u,  string2Num)
      x.df = sapply(Inputs$x.df, string2Num)
      x.pdf= Inputs$x.pdf      
      names(x.mu)=names(x.u)=names(x.df)=names(x.pdf)=Inputs$names
    })
    S=do_GUM()
    vgSA(fExpr, x.mu, x.u, S$X, S$Y, silent=TRUE)$budget
  })
  
  output$saveData <- downloadHandler(
    filename = function() {paste0(input$fileName,'.upsa')},
    content = function(file) {writeLines(text=saveModel(),con=file)},
    contentType='text/plain'
  )
  
  saveModel = function() {
    isolate({    
      # Get variables data
      fExpr= Inputs$fExpr
      x.mu = sapply(Inputs$x.mu, string2Num)
      x.u  = sapply(Inputs$x.u,  string2Num)
      x.df = sapply(Inputs$x.df, string2Num)
      x.pdf= Inputs$x.pdf
      nvars=length(x.mu)
      corrMat = Inputs$corrMat 
      names=Inputs$names
    })
    
    txt = paste0(
      "fExpr='",fExpr,"'\n",
      "x.mu=c(",paste0(x.mu,collapse=","),")\n",
      "names(x.mu)=c('",paste0(names,collapse="','"),"')\n",
      "x.u=c(",paste0(x.u,collapse=","),")\n",
      "names(x.u)=c('",paste0(names,collapse="','"),"')\n",
      "x.df=c(",paste0(x.df,collapse=","),")\n",
      "names(x.df)=c('",paste0(names,collapse="','"),"')\n",
      "x.pdf=c('",paste0(x.pdf,collapse="','"),"')\n",
      "names(x.pdf)=c('",paste0(names,collapse="','"),"')\n")
    if(!is.null(corrMat))
      txt = paste0(txt,
             "corrMat=matrix(c('",
               paste0(corrMat,collapse="','"),
             "'),nrow=",nvars,")\n"
             )
    
    return(txt)
  }
  
  output$report = downloadHandler(
    filename = function() {
      paste(input$reportName, sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },    
    content = function(file) {
      src <- normalizePath('reportTemplate.Rmd')      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'reportTemplate.Rmd')
      out <- rmarkdown::render(
        'reportTemplate.Rmd', 
        switch(input$format,
               PDF = "pdf_document", 
               HTML = "html_document", 
               Word = "word_document")
      )
      file.rename(out, file)
    }
  )
  
})





