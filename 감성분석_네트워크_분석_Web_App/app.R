##################################################################
# 감성분석 서비스 
##################################################################

rm(list=ls())
setwd("~/2020_일반과제_기후변화_감성분류기_구축/2020_일반과제_감성분석")
options(shiny.maxRequestSize=5000*1024^2) 
options(shiny.plot.res=300)

# Shiny
library(shiny)
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
library(tidyverse)
library(lubridate)
library(zoo)
library(plotly)
library(vroom)
library('igraph')
library(igraph)
library(network)
library(sna)
library(ggplot2)
library(GGally)
library(wordVectors)
library(tm)

loadfonts()
showtext_auto()
font_add("NanumBarunGothic", "NanumBarunGothic.ttf")
indi_num <- 100
thres <- 0.7
max_kw_num <- 30
min_num <- 20
s_date <- Sys.Date()
rn <- sample(1:100000000,1)
fn <- paste("./result/morphed_", s_date,"__",rn,".csv",sep="")

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("환경 SNS 감성분석 서비스"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select Datasets
      
      radioButtons(inputId = "corpus", "데이터 설정",  choices=c('네이버 뉴스댓글','트위터 데이터')),
      textInput(inputId = "start_date", label = "시작날짜", value = "2019-01-01"),
      textInput(inputId = "end_date", label = "끝날짜", value = "2019-12-31"),
      textInput(inputId = "search_exp", label = "검색식", value = ""),
      
      actionButton("load_data", "데이터 로드"),

      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      textInput(inputId = "thres", label = "Threshold", value = 0.7),
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      actionButton("do", "분석실행"), 
      width=2
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("데이터", DT::dataTableOutput("data")),
                  tabPanel("트렌드 분석", h3("빈도 트렌드 분석"),plotlyOutput("freq_result"),h3("감성 트렌드 분석"),plotlyOutput("senti_result")),
                  tabPanel("키워드 빈도수", tableOutput("morph_result")),
                  tabPanel("키워드 네트워크", plotOutput("nw_result", width = "100%", height= 800))
                  #tabPanel("감성분석", plotOutput("plot", width = "100%", height= 800),downloadButton("dBtn", "Download"))
                  #tabPanel("네트워크 지표", tableOutput("indi"),downloadButton("indi_dBtn", "Download"))                  #tabPanel("메뉴얼",  includeMarkdown("kn_manual.Rmd"))
      )
    )
  )
)



# Define server logic to read selected file ----
server <- function(input, output,session)
{
  df <- eventReactive(input$load_data, {
    if(input$corpus == '네이버 뉴스댓글'){
      df <- vroom("/media/Data/Naver_Comments/result/test.csv",delim=",", col_select = c(date,title,comments,prob))
      df <- unique(df) 
    
      start_date <- input$start_date
      end_date <- input$end_date
      
      start_idx <- which(df$date >= ymd(start_date))
      end_idx <- which(df$date <= ymd(end_date))
      
    
      c_idx <- intersect(start_idx,end_idx)
      print(c_idx)
      if(length(c_idx) > 0){
        df <- df[c_idx,]
        df <- df %>% filter(grepl(input$search_exp,title))
      }
      else{
        df <- NULL
      }
      #print(df)
      df
    }
    else if(input$corpus == '트위터 데이터'){
      df <- vroom("/media/Data/Naver_Comments/result/twitter.csv",delim=",", col_select = c(date,comments,prob))
      df <- unique(df) 
      
      start_date <- input$start_date
      end_date <- input$end_date
      
      start_idx <- which(df$date >= ymd(start_date))
      end_idx <- which(df$date <= ymd(end_date))
      
      
      c_idx <- intersect(start_idx,end_idx)
      print(c_idx)
      
      if(length(c_idx) > 0){
        df <- df[c_idx,]
        df <- df %>% filter(grepl(input$search_exp,comments))
      }
      else{
        df <- NULL
      }
      df
    }
  })
  
z <<- NULL
  
  p1 <- eventReactive(input$do,{
    if(input$corpus == '네이버 뉴스댓글'){
      df <- df()
      df$cnt <- 1
      df$date <- as.Date(as.yearmon(df$date))
      bymonth <- aggregate(cnt~date,data=df,FUN=sum)
      print(head(bymonth))
      df2 <- df[,c('date','title','cnt')]
      df2 <- unique(df2)
      bymonth2 <- aggregate(cnt~date, data=df2,FUN=sum)
      print(head(bymonth2))
      
      bymonth$rcnt <- bymonth$cnt/bymonth2$cnt
      
      print(bymonth)
      
      z <<- bymonth
      
      ay <- list(
        overlaying = "y",
        side = "right",
        title = "second y axis"
      )
      
      
      p <-plot_ly(
        data = bymonth,
        type = 'scatter',
        mode = 'line+markers',
        line = list(width = 3)
      ) %>%    
        layout(title="",
               xaxis = list(title = "날짜",type='date', tickformat='%Y-%m'),
               yaxis = list(title = "기사당 댓글수"),
               yaxis2 = ay)
      
      col_names <- colnames(bymonth)
      col_names <- col_names[-which(col_names == 'date')]
      
      #for(trace in col_names){
      #  p <- p %>% plotly::add_trace(x = ~date, y = as.formula(paste0("~`", trace, "`")), name = trace)
      #}
      #p
      p <- p %>% plotly::add_trace(x= ~date, y = as.formula(paste0("~`", 'cnt', "`")), name = "댓글수")
      p <- p %>% plotly::add_trace(x= ~date, y = as.formula(paste0("~`", 'rcnt', "`")), name = "기사당 댓글수", yaxis = "y2")
      p
    }
    else if(input$corpus == '트위터 데이터'){
      df <- df()
      df$cnt <- 1
      df$date <- as.Date(as.yearmon(df$date))
      bymonth <- aggregate(cnt~date,data=df,FUN=sum)
      print(head(bymonth))
      
      ay <- list(
        overlaying = "y",
        side = "right",
        title = "second y axis"
      )
      
      p <-plot_ly(
        data = bymonth,
        type = 'scatter',
        mode = 'line+markers',
        line = list(width = 3)
      ) %>%    
        layout(title="",
               xaxis = list(title = "날짜",type='date', tickformat='%Y-%m'),
               yaxis = list(title = "트위터 텍스트"),
               yaxis2 = ay)
      
      col_names <- colnames(bymonth)
      col_names <- col_names[-which(col_names == 'date')]

      p <- p %>% plotly::add_trace(x= ~date, y = as.formula(paste0("~`", 'cnt', "`")), name = "트위터 텍스트 수")
      p
    }
  })

  
  
  output$data <- DT::renderDataTable({
    
    if(input$disp == "head") {
      return(head(df()))
    }
    else {
      return(df())
    }
    
  })
  
  p2 <- eventReactive(input$do,{
    df <- df()
    thres <- input$thres
    idx <- which(df$prob > thres)
    df$prob[idx]  = 1
    df$prob[-idx]  = 0
    df$cnt = 1
    
    df$date <- as.Date(as.yearmon(df$date))
    bymonth <- aggregate(prob~date,data=df,FUN=mean,na.rm=FALSE,na.action=NULL)
    cnt <- aggregate(cnt~date,data=df,FUN=sum)$cnt
    print(cnt)
    
    bymonth$prob2 = 1 - bymonth$prob
    
    #idx <- which(cnt < min_text_num)
    
    
    # bymonth$prob1[idx] = 0
    # bymonth$prob2[idx] = 0
    #bymonth$prob3[-idx] = 1
    bymonth <- bymonth %>% filter(cnt > min_num)

    print(bymonth)
    p <- plot_ly(bymonth, x = ~date, y = ~prob2, type = 'bar', name = '긍정/중립')
    p <- p %>% add_trace(y = ~prob, name = '부정')
    #p <- p %>% add_trace(y = ~prob3, name = '샘플부족')
    p <- p %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
  })
  
  output$freq_result <- renderPlotly({
    p1()
  })
  
  
  output$senti_result <- renderPlotly({
    p2()
  })
  
  
  m1 <- eventReactive(input$do,{
    df <- df()
    thres <- input$thres
    idx <- which(df$prob > thres)
    baseData <- df[idx,"comments"]
    parsedData <- baseData
    content <- as.vector(unlist(baseData["comments"]))
    print(content)
    text <- r_extract_noun(content, language = "ko")
    s_words = ''
    
    #분석 결과 가져오기
    
    result_mat <- data.frame(text)
    #write_xlsx(result_mat, path = fn)
    write.csv(result_mat, file = fn, fileEncoding="UTF-8")
    
    corp <- Corpus(VectorSource(as.vector(unlist(result_mat))))
    s_words = unlist(strsplit(s_words,","))
    tdm <- TermDocumentMatrix(corp, control=list(wordLengths=c(4,Inf), stopwords=s_words))
    
    #분석 결과 가져오기
    
    m <- as.matrix(tdm) 
    v <- sort(rowSums(m),decreasing=TRUE) # 빈도수를 기준으로 내림차순 정렬
    d <- data.frame(word = names(v),freq=v)  # 데이터 프레임 생성
    d[1:max_kw_num,]
  })
  
  output$morph_result <- renderTable({
    m1()
  })
  
  output$nw_result <- renderPlot({
    #모델 Training
    print("무야")
    
    m <- m1()
    kw_list <- as.vector(unlist(m[,"word"]))
    
    model = train_word2vec(fn, threads=3, vectors=20, window=5) 
    rnames <- rownames(model@.Data)
    result_mat <- model@.Data
    
    idx <- c()
    for(k in kw_list){
      idx <- c(idx,which(rnames == k))
    }
    
    result_mat <- result_mat[idx,]
  
    g <- 1 - cosineDist(result_mat,result_mat)
   
    thres <- quantile(g,0.7)
    #g <- g[-didx,-didx]
    
    #w_list <- w_list[-didx]
    #colnames(g) <- w_list
    g[g < thres] = 0
    
    
    
    ## Make Network
    net <- network(g, directed=FALSE)
    net %v% "size" <- ifelse(betweenness(net) > quantile(betweenness(net),0.7), "big", "small")
    col = c("small" = "skyblue", "big" = "tomato")
    ggnet2(net, "kamadakawai",label=TRUE,label.size = 6, color = "size", size=round(degree(net) / max(degree(net)) * 15),palette = col,max_size = 20,legend.position = 0,edge.label.alpha = 0)
    
  })
  
}



# Create Shiny app ----
shinyApp(ui, server)