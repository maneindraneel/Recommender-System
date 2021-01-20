ui <- fluidPage(
  theme = shinythemes::shinytheme("sandstone"),
  useShinydashboard(),
  titlePanel("Recommendation System"),
  sidebarLayout(
    sidebarPanel(
      selectInput("user","Select User", choices = 1:20000),
      selectInput("genre1", "Select Genre",choices = genre_list),
      uiOutput("movie1"),
      uiOutput("movie2"),
      uiOutput("movie3"),
      
      actionButton("do", "Show Movies")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("IBCF",
                 tableOutput("ibcf")
        ),
        tabPanel("UBCF",
                 tableOutput("ubcf")),
        tabPanel("Evaluation Metrics",
                 dataTableOutput("Accuracy"),
                 fluidRow(
                   shinydashboard::box(textOutput("acc"), title = "Accuracy on a subset of test data",collapsible = TRUE,
                                       collapsed = TRUE,solidHeader = TRUE))
        )
      )
    )
  )
)

server <- shinyServer(function(input,output,session){
  output$movie1 <- renderUI({
    selectInput("movie_1", "Select movie 1", choices = c(movies_split[movies_split$genres==input$genre1,"title"]))
  })
  output$movie2 <- renderUI({
    selectInput("movie_2", "Select movie 2", choices = c(movies_split[movies_split$genres==input$genre1,"title"]))
  })
  output$movie3 <- renderUI({
    selectInput("movie_3", "Select movie 3", choices = c(movies_split[movies_split$genres==input$genre1,"title"]))
  })
  
  result <- reactive({
    ratingsMovies <- 
      userList %>% inner_join(ratings) %>%
      left_join(movies_split) 
    
    cf_matrix<- ratingsMovies %>%   
      filter(genres == input$genre1) %>% 
      select('userId','title','rating') %>% 
      cast_dtm(userId,title,rating)
    
    ibcf_matrix <- cf_matrix %>% 
      as.matrix() %>% 
      as.data.frame()
    
    x_4<-tibble(a=colnames(ibcf_matrix),
                b=cor(ibcf_matrix,ibcf_matrix[,input$movie_1],use="pairwise.complete.obs"),
                c=cor(ibcf_matrix,ibcf_matrix[,input$movie_2],use="pairwise.complete.obs"),
                d=cor(ibcf_matrix,ibcf_matrix[,input$movie_3],use="pairwise.complete.obs")) %>% 
      filter(!(a %in% c(input$movie_1,input$movie_2,input$movie_3))) 
    x_4 = x_4 %>%as.data.frame() %>% mutate(s = b+c+d)
    x_4 <- x_4[order(-x_4$s),]
    x_4 %>% rename("Movie"=a) %>% select(Movie) %>% head(10)
  })
  ubcf_result <- reactive({
    cor_values<-ubcfMatrix %>%
      cor(ubcfMatrix[,input$user],use="pairwise.complete.obs")
    
    final_matrixUBCF <-tibble(colnames(ubcfMatrix), cor_values) %>%
      rename("userId"=`colnames(ubcfMatrix)`)
    
    ### Select the 4 most similar users
    ubcfSelectUsers <-
      final_matrixUBCF[order(-cor_values),] %>% head(4)
    
    ### Add userId as a column
    tempUsers <- cbind(userId= rownames(ibcf_matrix),ibcf_matrix)
    
    ### Random Error- May fail later
    tempUsers <- tempUsers[,-c(2886)]
    
    ### Join with IBCF
    tempUserSelectIbcf <- tempUsers %>%
      filter(userId %in% ubcfSelectUsers$userId)
    
    ### Add userId as the rownames
    rownames(tempUserSelectIbcf) <- tempUserSelectIbcf$userId
    
    ### Transpose and add movies as a column
    tempTransposeUserIbcf <- tempUserSelectIbcf %>% select(-c(userId)) %>% t()
    
    MoviesSimilarUsers <- cbind(title= rownames(tempTransposeUserIbcf),tempTransposeUserIbcf)
    
    MoviesSimilarUsers <- MoviesSimilarUsers %>% as.data.frame()
    
    ### Select those movies which have not been seen by the selected user and
    ### seen by atleast one of the similar users
    
    MoviesRecommendUbcf <- MoviesSimilarUsers[
      MoviesSimilarUsers[,2]==0 & MoviesSimilarUsers[,3]!=0 | MoviesSimilarUsers[,4]!=0 |
        MoviesSimilarUsers[,5]!=0,]
    
    ### Add their ratings, and take the top 10 movies as per the ratings' sum
    
    MoviesRecommend <- MoviesRecommendUbcf %>%
      mutate(movieScore = as.numeric(levels(MoviesRecommendUbcf[,3])[MoviesRecommendUbcf[,3]]) +
               as.numeric(levels(MoviesRecommendUbcf[,4])[MoviesRecommendUbcf[,4]])+
               as.numeric(levels(MoviesRecommendUbcf[,5])[MoviesRecommendUbcf[,5]])) %>%
      arrange(desc(movieScore)) %>% head(10) %>% rename(`Movie`= title) %>%
      select(`Movie`)
    MoviesRecommend
  })
  output$Accuracy <- DT::renderDataTable(
    DT::datatable(MovieRecoPredRating, options = list(pageLength = 10))
  )
  output$acc<- renderText({
    "Accuracy = 79.23%"
  })
  observeEvent(input$do,{output$ibcf <- renderTable({
    result()
  }, caption = "Here are some movies you might like (IBCF)",caption.placement = getOption("xtable.caption.placement", "top"))
  })
  observeEvent(input$do,{output$ubcf <- renderTable({
    ubcf_result()
  },caption = "Similar users have liked these film (UBCF)",caption.placement = getOption("xtable.caption.placement", "top"))
  })

})

runApp(list(ui=ui,server=server))