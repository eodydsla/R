  ##################################################################
  # 키워드 빈도 & 트렌드 분석 서비스
  ##################################################################
  
  rm(list=ls())
  setwd("~/2020_텍스트_분석_Web_App/ka")
  options(shiny.maxRequestSize=5000*1024^2) 
  options(shiny.plot.res=300)
  
  # Shiny
  library(shiny)
  library(plotly)
  library(NLP4kec)
  library(readr)
  library(KoNLP)
  library(arules)
  library(igraph)
  library(combinat)
  library(DT)
  library(showtext)
  library(extrafont)
  library(readxl)
  library(writexl)
  library(tm)
  library(lubridate)

  
  loadfonts()
  showtext_auto()
  font_add("NanumBarunGothic", "NanumBarunGothic.ttf")
  
  
  # Define UI for data upload app ----
  ui <- fluidPage(
    
    # App title ----
    titlePanel("키워드 빈도 및 트렌드 분석 서비스"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        # Input: Select a file ----
        fileInput('file1', '엑셀 파일 선택(*.xlsx)',  accept = c(".xlsx")),
  
        
        radioButtons(inputId = "morph_type", "형태소 분석기",
                     choices = c(은닢한전= "mecab",
                                     한나눔  = "hannanum",
                                     띄워쓰기 = "space")),
        
        # Input: Select number of rows to display ----
        radioButtons("disp", "Display",
                     choices = c(Head = "head",All = "all"),
                     selected = "head"),
        textInput("s_words", "스탑워드",value = ""),
        actionButton("do", "분석실행"), 
        width=2
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("데이터", DT::dataTableOutput("data")),
                    tabPanel("전체 빈도수", tableOutput("result"),downloadButton("freq_dBtn", "Download")),
                    tabPanel("일별 빈도수", dataTableOutput("result2"),downloadButton("freq_dBtn2", "Download")),
                    tabPanel("일별 트랜드", plotlyOutput("result3"))
                    
        )
      )
    )
  )
  
  
  
  
  s_date <- Sys.Date()
  rn <- sample(1:100000000,1)
  fn <- paste("./result/freq_", s_date,"__",rn,".xlsx",sep="")
  fn2 <- paste("./result/freq_day_", s_date,"__",rn,".xlsx",sep="")
  fn3 <- paste("./result/freq_trends_", s_date,"__",rn,".xlsx",sep="")
  df <- NULL
  
  strsplit_space <- function(x)
  {
    paste(unlist(strsplit(x," ")), sep=" ")
  }
  
  
  # Define server logic to read selected file ----
  server <- function(input, output,session) {
   
    output$data <- DT::renderDataTable({
      req(input$file1)
      file <- input$file1
      
      df <<- read_excel(file$datapath,1)
      
      if(input$disp == "head") {
        return(head(df))
      }
      else {
        return(df)
      }
      
    })
    
    observeEvent(input$do,{
      req(input$file1)
      output$result <- renderText({"Processing..."})
      print(input$chk)
      
      s_words = unlist(strsplit(input$s_words,","))
      
      result_mat <- reactive({                
        baseData <- df[,"text"]
        parsedData <- baseData
        baseData <- as.vector(unlist(baseData))
        baseData <- as.vector(baseData)
        
        morph_type = input$morph_type
        if(morph_type == "mecab"){
          text <- r_extract_noun(baseData, language = "ko")
        } else if(morph_type == "hannanum"){
          text <- extractNoun(baseData)
          text <- unlist(lapply(text,paste,collapse=" "))
        } else{
          text <- Map(strsplit_space,baseData)
          text <- unlist(lapply(text,paste,collapse=" "))
        }
        
        result_mat <- data.frame(text) 
        write_xlsx(result_mat, path = fn2)
        result_mat
      })      
      
      
      day_keyword_freq <- eventReactive(input$do, {
        cnt <- 10
        baseData <- result_mat()
        dateList <- as.vector(df$date)
        dateList <- ymd(as.character(dateList))
        s_words = unlist(strsplit(s_words,","))
        corp <- Corpus(VectorSource(as.vector(unlist(baseData))))
        tdm <- TermDocumentMatrix(corp, control=list(wordLengths=c(4,Inf), stopwords=s_words))
        
        #분석 결과 가져오기
        
        m <- t(as.matrix(tdm))
        m <- as.tibble(m)
        m <- cbind(dateList,m)
        
        result <- aggregate(.~dateList, m ,sum)
        write_xlsx(result, path = fn2)
        
        freq_sum <- apply(subset(result,select=-dateList),2,sum)
        
        idx <- order(-freq_sum)[1:cnt] 
        
        # dateList 컬럼을 제외했기 때문
        idx <- idx + 1
        
        sub_result <- subset(result, select = idx)
        sub_result <- cbind(result$dateList,sub_result)
        colnames(sub_result)[1] = 'date'
        sub_result
      }, ignoreNULL = FALSE)
      
      
      p1 <- eventReactive(input$update, {
        result_df <- day_keyword_freq()
        result_df$date <-as.Date(result_df$date)
        
        p <-plot_ly(
          result_df,
          type = 'scatter',
          mode = 'line+markers',
          line = list(width = 3)
        ) %>%    
          layout(title="키워드별 트렌드",
                 xaxis = list(title = "날짜"),
                 yaxis = list(title="키워드 언급 수"))
        
      col_names = colnames(result_df)
      col_names = col_names[-1]
      
      for (trace in col_names) {
          p <- p %>% add_trace(x = ~date,y = as.formula(paste0("~`", trace, "`")), name = trace)
        }
        p
      }, ignoreNULL = FALSE)
      
      
      output$result <- renderTable({
        baseData <- result_mat()
        corp <- Corpus(VectorSource(as.vector(unlist(baseData))))
        s_words = unlist(strsplit(s_words,","))
        tdm <- TermDocumentMatrix(corp, control=list(wordLengths=c(4,Inf), stopwords=s_words))
        
        #분석 결과 가져오기
        
        m <- as.matrix(tdm) 
        v <- sort(rowSums(m),decreasing=TRUE) # 빈도수를 기준으로 내림차순 정렬
        d <- data.frame(word = names(v),freq=v)  # 데이터 프레임 생성
        return(d)
      })
      
      
    
      output$result2 <- DT::renderDataTable({
        result <- day_keyword_freq()
        return(result)
      },
      rownames = FALSE
      )
      
      
      
      output$result3 <- renderPlotly({
        p1()
      })
      

      output$freq_dBtn <- downloadHandler(
        filename = function(){
          paste("freq_text_all_",rn,".xlsx",sep="")
        },
        content = function(file){
          file.copy(fn, file)
        }
      )
      
      output$freq_dBtn2 <- downloadHandler(
        filename = function(){
          paste("freq_text_day_",rn,".xlsx",sep="")
        },
        content = function(file){
          file.copy(fn2, file)
        }
      )
      
      
    })
    
  }
  
  
  # Create Shiny app ----
  shinyApp(ui, server)
