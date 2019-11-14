library(dplyr)
library(ggplot2)
library(shiny)
library(tidytext)

library(topicmodels)

library(gridExtra)
library(wordcloud)

# library(ggraph)
# library(igraph)

communitySurveyEncoded <- read.csv("CommunitySurveyEncoded.csv")
communitySurveyEncoded$Wellbeing <- relevel(communitySurveyEncoded$Wellbeing, ref = 'Thriving')
communitySurveyEncoded$Outlook <- relevel(communitySurveyEncoded$Outlook, ref = 'Optimism')
communitySurveyEncoded$PositiveCommunityComments <- as.character(communitySurveyEncoded$PositiveCommunityComments)
communitySurveyEncoded$NegativeCommunityComments <- as.character(communitySurveyEncoded$NegativeCommunityComments)
communitySurveyEncoded$SurveyWDay <- as.numeric(communitySurveyEncoded$SurveyWDay)
genderLevels = c(levels(communitySurveyEncoded$Gender), "All")
raceLevels = c(levels(communitySurveyEncoded$Race), "All")
ageGroupLevels = c(levels(communitySurveyEncoded$AgeGroup), "All")
pos_vars = c("PersonalRating", "PersonalFutureRating", "PhysicalHealth", "MentalHealth",
         "EmotionalHealth", "Purpose", "CommunityWellBeingRating","FutureCommunityRating",
         "Ladder2", "LadderFuture2") #, "PersonalOptimism", "CommunityOptimism"

communityConversations <- read.csv("CommunityConversationsCleaned.csv")
communityConversations$Response <- communityConversations$Response

conditionLevels = c("All", levels(communityConversations$Seven.Vital.Conditions))
questionLevels = c("All", levels(communityConversations$Question.Tag))

bing <- get_sentiments("bing") #no, that's not bing.com as in not Google, but a scientist's name
bing$sentimentNum <- ifelse(bing$sentiment == "positive", 1, -1)

communitySurveyEncodedSummary <- communitySurveyEncoded %>% 
  group_by(Gender, AgeGroup, Race) %>% 
  summarize(
    Responses = n(),
    PersonalRating = round(mean(PersonalRating,na.rm=T), 2),
    PersonalFutureRating = round(mean(PersonalFutureRating,na.rm=T), 2),
    PhysicalHealth = round(mean(PhysicalHealth,na.rm=T), 2),
    MentalHealth = round(mean(MentalHealth,na.rm=T), 2),
    PurposeScore = round(mean(Purpose,na.rm=T), 2),
    EmotionalHealth = round(mean(EmotionalHealth,na.rm=T), 2),
    PercentThriving = round(sum(Thriving,na.rm=T)/n(), 2) * 100,
    PercentStruggling = round(sum(Struggling,na.rm=T)/n(), 2) * 100,
    PercentSuffering = round(sum(Suffering,na.rm=T)/n(), 2) * 100,
    PersonalOptimism = round(mean(PersonalOptimism,na.rm=T), 2) * 100,
    PersonalPessimism = round(mean(PersonalPessimism,na.rm=T), 2) * 100,
    CommunityOptimism = round(mean(CommunityOptimism,na.rm=T), 2) * 100,
    CommunityPessimism = round(mean(CommunityPessimism,na.rm=T), 2) * 100,
    CommunityRating = round(mean(CommunityWellBeingRating,na.rm=T), 2),
    FutureCommunityRating = round(mean(FutureCommunityRating,na.rm=T),2)   ) %>% 
  filter(Responses > 2)

# Define UI for application that draws a histogram
ui <- navbarPage("Imagine Fox Cities",
  tabPanel("Wellbeing by Demographic",
   titlePanel("Average (Mean) Wellbeing by Demographic Group"),
    mainPanel(
       DT::dataTableOutput("DemographicWellfare"),
       downloadButton("downloadDemographicData", "Download Table"),
       HTML(paste(h5("Note: the meaning of ratings may vary by community group a 3 (out of 5) may mean different (better or worse) things to different groups."), #'<br/>',
                  h5("PersonalRating is the Cantril Scale (question asked 0-10 but here scaled from 0-5) used with the FutureRating to generate Thriving/Struggling/Suffering https://news.gallup.com/poll/122453/understanding-gallup-uses-cantril-scale.aspx"),
                  h5("Optimism and Pessimism are % of answers predicting improved or declining ratings for the future. Higher numbers = more optimistic or pessimistic respectively, max score 100 (%). Not shown are those which remain constant."),
                  h5("Code can be viewed and issues reported at https://github.com/winedarksea/ImagineFoxCities")))
    )
  ),
  tabPanel("Wellbeing Graphs",
           titlePanel("Wellbeing by Demographic Group"),
           sidebarLayout(
             sidebarPanel(
               selectInput("GenderGraph", "Gender", genderLevels, selected = "All"),
               selectInput("RaceGraph", "Race", raceLevels, selected = "All"),
               selectInput("AgeGroupGraph", "Age Group", ageGroupLevels, selected = "All"),
               textOutput("nrows"),
               HTML(paste(h3(" "), '<br/>', '<br/>', h3(" "), '<br/>', h3(" "), '<br/>')),
               selectInput("CoorGraphShow", "Show Variable Comparison Density Graph", c("Yes", "No"), selected = "No"),
               conditionalPanel(
                 condition = "input.CoorGraphShow == 'Yes'",
               selectInput("VariableX", "Variable X", pos_vars, selected = "PersonalRating"),
               selectInput("VariableY", "Variable Y", pos_vars, selected = "MentalHealth")
               )
              ),
             mainPanel(
               plotOutput("WellbeingPlot"),
               conditionalPanel(
                 condition = "input.CoorGraphShow == 'Yes'",
                 plotOutput("WellbeingPlot2")
               )
               )
           )
  ),
  tabPanel("Community Comment Explorer",
        sidebarLayout(
        sidebarPanel(
          selectInput("Gender", "Gender", genderLevels, selected = "All"),
          selectInput("Race", "Race", raceLevels, selected = "All"),
          selectInput("AgeGroup", "Age Group", ageGroupLevels, selected = "All"),
          selectInput("Positivity", "Comments: Community Pros/Cons", c("Strengths", "Issues", "All"), selected = "Pros"),
          selectInput("TopicModelShow", "Show LDA Topic Modeling (slow)", c("Yes", "No"), selected = "No"),
          conditionalPanel(
            condition = "input.TopicModelShow == 'Yes'",
            numericInput("topic_num", "Number of Topics (only shows top 4)", 8, min = 4, max = 20, step = 1)
          )
        ),
        mainPanel(
          plotOutput("TestWordCloud", width = "100%"),
          div(downloadButton("downloadCommentWordCloud", "Download Wordcloud: layout changes randomly on download"), align = 'center'),
          conditionalPanel(
            condition = "input.TopicModelShow == 'Yes'",
            plotOutput("TopicModeling") # DT::dataTableOutput plotOutput
          ),
          DT::dataTableOutput("TopWords")
         )
        )
     ),
  tabPanel("Community Conversation Explorer",
           div(selectInput("Question", "Question", questionLevels, selected = "All"), align = "center"),
           div(selectInput("Condition", "Condition", conditionLevels, selected = "All"), align = "center"),
           div(selectInput("ConditionType", "Condition Type",  
              c("Self-Reported" = 'Seven.Vital.Conditions', "Assigned" = 'AssignedCondition', "Assigned -Sub Conditions" = 'Sub.Condition'), selected = "AssignedCondition"),align='center'),
           plotOutput("ConversationWordCloud", width = "100%"),
           div(downloadButton("downloadConversationWordCloud", "Download Wordcloud: layout changes randomly on download"), align = 'center'),
           # DT::dataTableOutput("ConversationTopWords"),
           plotOutput("Sentiment", width = "100%"),
           div(selectInput("ShowTopWordTable", "Show Conversation Top Words", c("Yes", "No"), selected = "No"), align = 'center'),
           conditionalPanel(
             condition = "input.ShowTopWordTable == 'Yes'",
             DT::dataTableOutput("ConversationTopWords")
           ),
           div(selectInput("ShowTable", "Show Conversation Summaries", c("Yes", "No"), selected = "No"), align = 'center'),
           conditionalPanel(
             condition = "input.ShowTable == 'Yes'",
             DT::dataTableOutput("Conversations")
           )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {

   output$DemographicWellfare <- DT::renderDataTable({
     communitySurveyEncodedSummary
   }, options = list(pageLength = 10))
   output$downloadDemographicData <- downloadHandler(
     filename = function() {
       paste("WellbeingDemographicSummary", ".csv", sep = "")
     },
     content = function(file) {
       write.csv(communitySurveyEncodedSummary, file, row.names = FALSE)
     }
   )
   output$downloadCommentWordCloud <- downloadHandler(
     filename = function() {
       paste0("FoxCitiesCommentWordCloud", ".png")
     },
     content = function(file) {
       ggsave(file, plot = CommentWordcloud(), device = "png", dpi = 300)
     }
   )
   output$downloadConversationWordCloud <- downloadHandler(
     filename = function() {
       paste0("FoxCitiesConversationWordCloud", ".png")
     },
     content = function(file) {
       ggsave(file, plot = ConversationWordcloud(), device = "png", dpi = 300)
     }
   )
   localCSE_reactive <- eventReactive({
     input$Race
     input$Gender
     input$AgeGroup
     input$Positivity
   }, {
     localCSE <- communitySurveyEncoded
     if (input$Race != 'All') {
       localCSE <- localCSE %>% filter(Race == input$Race)
     }
     if (input$Gender != 'All') {
       localCSE <- localCSE %>% filter(Gender == input$Gender)
     }
     if (input$AgeGroup != 'All') {
       localCSE <- localCSE %>% filter(AgeGroup == input$AgeGroup)
     }
     if (input$Positivity == 'Strengths') {
       localCSE <- localCSE %>% filter(!is.na(PositiveCommunityComments),
                                       !(PositiveCommunityComments %in% c("", " ", "NA"))) %>%
         mutate(Comments = PositiveCommunityComments) %>%
         select(Respondent.ID, Comments) 
     }
     if (input$Positivity == 'Issues') {
       localCSE <- localCSE %>% filter(!is.na(NegativeCommunityComments),
                                       !(NegativeCommunityComments %in% c("", " ", "NA"))) %>%
         mutate(Comments = NegativeCommunityComments) %>%
         select(Respondent.ID, Comments) 
     }
     if (input$Positivity == 'All') {
       localCSE <- rbind(
            localCSE %>% filter(!is.na(NegativeCommunityComments),!(NegativeCommunityComments %in% c("", " ", "NA"))) %>%
            mutate(Comments = NegativeCommunityComments) %>%
            select(Respondent.ID, Comments),
            localCSE <- localCSE %>% filter(!is.na(PositiveCommunityComments),!(PositiveCommunityComments %in% c("", " ", "NA"))) %>%
            mutate(Comments = PositiveCommunityComments) %>%
            select(Respondent.ID, Comments)     
       )
     }
     localCSE
   })
   output$TopWords <- DT::renderDataTable({
     comments <- localCSE_reactive()
     rbind(
     comments %>% unnest_tokens(word, Comments) %>%
       anti_join(stop_words) %>% count(word, sort = TRUE) %>%
       top_n(n = 20, wt = n) %>% mutate(Ngram = 1),
     comments %>%
       unnest_tokens(word, Comments, token = "ngrams", n = 2) %>%
       tidyr::separate(word, c("word1", "word2"), sep = " ") %>%
       filter(!word1 %in% stop_words$word) %>%
       filter(!word2 %in% stop_words$word) %>%
       tidyr::unite(word, word1, word2, sep = " ") %>%
       count(word, sort = TRUE) %>% filter(word != 'NA NA') %>%
       top_n(n = 20, wt = n) %>% mutate(Ngram = 2),
     comments %>%
       unnest_tokens(word, Comments, token = "ngrams", n = 3) %>%
       tidyr::separate(word, c("word1", "word2", "word3"), sep = " ") %>%
       filter(!word1 %in% stop_words$word) %>%
       filter(!word2 %in% stop_words$word) %>%
       filter(!word3 %in% stop_words$word) %>%
       tidyr::unite(word, word1, word2, word3, sep = " ") %>%
       count(word, sort = TRUE) %>% filter(word != 'NA NA NA') %>%
       top_n(n = 5, wt = n) %>% mutate(Ngram = 3)
     ) %>% filter(n > 1)
   }, options = list(pageLength = 10))
   CommentWordcloud = function() {
     comments <- localCSE_reactive()
     
     bigrams <- comments %>%
       unnest_tokens(word, Comments, token = "ngrams", n = 2) %>%
       tidyr::separate(word, c("word1", "word2"), sep = " ") %>%
       filter(!word1 %in% stop_words$word) %>%
       filter(!word2 %in% stop_words$word) %>%
       tidyr::unite(word, word1, word2, sep = " ") %>%
       count(word, sort = TRUE) %>% filter(word != 'NA NA')
     
     wordcloud(words = bigrams$word, freq = bigrams$n, min.freq = 2, #, scale=c(3.5,1)
               max.words=20, random.order=TRUE, rot.per=0.1, scale=c(3.5,0.25),
               colors=RColorBrewer::brewer.pal(8, "Dark2"), fixed.asp = TRUE)
   }
   output$TestWordCloud <- renderPlot({
     CommentWordcloud()
   })
   output$TopicModeling <- renderPlot({ # DT::renderDataTable renderPlot
     comments <- localCSE_reactive()
     chapters_dtm <- comments %>% unnest_tokens(word, Comments) %>%
       anti_join(stop_words) %>%
       count(Respondent.ID, word, sort = TRUE) %>%
       cast_dtm(Respondent.ID, word, n)
     chapters_lda <- LDA(chapters_dtm, k = input$topic_num, control = list(seed = 1234))
     ap_documents <- tidy(chapters_lda, matrix = "gamma")
     n_topics <- ap_documents %>% group_by(document) %>% top_n(1, gamma) %>%
       group_by(topic) %>% summarize(n = n()) %>% top_n(4, n)
     ap_topics <- tidy(chapters_lda, matrix = "beta")
     top_terms <- ap_topics %>%
       group_by(topic) %>%
       top_n(5, beta) %>%
       ungroup() %>%
       arrange(topic, -beta)
     top_terms[top_terms$topic %in% n_topics$topic,] %>%
       filter(topic %in% n_topics$topic) %>% # head(20) %>%
       #       mutate(term = reorder_within(term, beta, topic)) %>%
       ggplot(aes(term, beta, fill = factor(topic))) +
       geom_col(show.legend = FALSE) +
       facet_wrap(~ topic, scales = "free") +
       coord_flip() # + scale_x_reordered()
   })
   
   localConversation_reactive <- eventReactive({
     input$Condition
     input$Question
     input$ConditionType
   }, {
     localConversation <- communityConversations
     localConversation$ConditionGroup = localConversation[,input$ConditionType]
     
     if ((input$Condition != 'All') & (input$ConditionType != 'Sub.Condition')) {
       localConversation <- localConversation %>% filter(ConditionGroup == input$Condition)
     }
     if ((input$ConditionType == 'Sub.Condition') & (input$Condition != 'All')) {
       localConversation <- localConversation %>% filter(AssignedCondition == input$Condition)
     }
     if (input$Question != 'All') {
       localConversation <- localConversation %>% filter(Question.Tag == input$Question)
     }
     localConversation %>% select(Response, Question.Tag, ConditionGroup, Sub.Condition)
   })
   ConversationWordcloud = function() {
     comments2 <- localConversation_reactive()
     
     bigrams2 <- comments2 %>%
       unnest_tokens(word, Response, token = "ngrams", n = 2) %>%
       tidyr::separate(word, c("word1", "word2"), sep = " ") %>%
       filter(!word1 %in% stop_words$word) %>%
       filter(!word2 %in% stop_words$word) %>%
       tidyr::unite(word, word1, word2, sep = " ") %>%
       count(word, sort = TRUE) %>% filter(word != 'NA NA')
     
     wordcloud(words = bigrams2$word, freq = bigrams2$n, min.freq = 2,
               max.words=20, random.order=TRUE, rot.per=0.1, scale=c(3.5,0.25),
               colors=RColorBrewer::brewer.pal(8, "Dark2"), fixed.asp = TRUE)
   }
   output$ConversationWordCloud <- renderPlot({
     ConversationWordcloud()
   })
   output$ConversationTopWords <- DT::renderDataTable({
     comments3 <- localConversation_reactive()
     comments3$Response <- as.character(comments3$Response)
     rbind(
       comments3 %>% unnest_tokens(word, Response) %>%
         anti_join(stop_words) %>% count(word, sort = TRUE) %>%
         top_n(n = 20, wt = n) %>% mutate(Ngram = 1) %>% filter(n > 1),
       comments3 %>%
         unnest_tokens(word, Response, token = "ngrams", n = 2) %>%
         tidyr::separate(word, c("word1", "word2"), sep = " ") %>%
         filter(!word1 %in% stop_words$word) %>%
         filter(!word2 %in% stop_words$word) %>%
         tidyr::unite(word, word1, word2, sep = " ") %>%
         count(word, sort = TRUE) %>% filter(word != 'NA NA') %>%
         top_n(n = 20, wt = n) %>% mutate(Ngram = 2),
       comments3 %>%
         unnest_tokens(word, Response, token = "ngrams", n = 3) %>%
         tidyr::separate(word, c("word1", "word2", "word3"), sep = " ") %>%
         filter(!word1 %in% stop_words$word) %>%
         filter(!word2 %in% stop_words$word) %>%
         filter(!word3 %in% stop_words$word) %>%
         tidyr::unite(word, word1, word2, word3, sep = " ") %>%
         count(word, sort = TRUE) %>% filter(word != 'NA NA NA') %>%
         top_n(n = 5, wt = n) %>% mutate(Ngram = 3)
     ) 
     })
     output$Sentiment <- renderPlot({
       comments2 <- localConversation_reactive()
       comments2$Response <- as.character(comments2$Response)
       g1 <- comments2 %>% group_by(ConditionGroup) %>% 
         summarise(n = n()) %>%
         mutate(freq = n / sum(n), category = 1) %>%
         ggplot(aes(category, freq, fill = ConditionGroup)) + 
         geom_bar(stat="identity", width = 0.1) +
         coord_flip()  + 
         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) + 
         theme(axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank()) +
         theme(legend.position = "none")
       g2 <- comments2 %>% mutate(Id = row_number()) %>%
          unnest_tokens(word, Response) %>%
          inner_join(bing %>% select(word, sentimentNum)) %>% group_by(ConditionGroup, Id) %>%
         summarize(SentimentScore = sum(sentimentNum)) %>%
         # filter(SentimentScore != 0) %>% arrange(SentimentScore) %>%
         mutate(Positivity = ifelse(SentimentScore > 0, "Positive", ifelse(SentimentScore == 0, "Neutral", "Negative"))) %>%
         mutate(SentimentScore = ifelse(SentimentScore > 2, 2, ifelse(SentimentScore < -2, -2, SentimentScore))) %>%
         ggplot() +
         geom_histogram(aes(SentimentScore, fill = ConditionGroup), binwidth = 0.3, size = 0.3) +
         #geom_bar(aes(Positivity, SentimentScore),stat = 'sum', show.legend = FALSE, width = 0.1, color = "skyblue1") +
         theme_light() +
         theme(legend.position="bottom") +
         xlim(-2.5, 2.5)
       grid.arrange(g1, g2, nrow = 2, ncol=1, heights = c(1, 4))
     })
     output$Conversations <- DT::renderDataTable({
       localConversation_reactive()
     })
     
     localWellbeing_reactive <- eventReactive({
       input$RaceGraph
       input$GenderGraph
       input$AgeGroupGraph
     }, {
       localWellbeing <- communitySurveyEncoded
       if (input$RaceGraph != 'All') {
         localWellbeing <- localWellbeing %>% filter(Race == input$RaceGraph)
       }
       if (input$GenderGraph != 'All') {
         localWellbeing <- localWellbeing %>% filter(Gender == input$GenderGraph)
       }
       if (input$AgeGroupGraph != 'All') {
         localWellbeing <- localWellbeing %>% filter(AgeGroup == input$AgeGroupGraph)
       }
       localWellbeing
     })
     output$nrows <- renderText({
       paste0("Number of Responses: ", as.character(nrow(localWellbeing_reactive())))
     })
     output$WellbeingPlot2 <- renderPlot({
       wellbeingData <- localWellbeing_reactive()
       ggplot(wellbeingData, aes_string(x = input$VariableX,y = input$VariableY) ) +
         stat_density_2d(aes(fill = ..level..), geom = "polygon") +
         scale_fill_continuous(type = "viridis") +
         expand_limits(x = 0.8, y = 0.8) + expand_limits(x = 5.5, y = 5.5)
         # theme_bw()
     })
     output$WellbeingPlot <- renderPlot({
       wellbeingData <- localWellbeing_reactive()
       g1 <- wellbeingData %>% 
         mutate(category = 1) %>% # freq = n / sum(n),
         ggplot(aes(category, fill = Wellbeing)) + # freq,
         geom_bar(width = 0.1) + #stat="identity",
         scale_y_reverse() + 
         coord_flip()  +
         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
         theme(axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank()) +
         theme(legend.position = "bottom") + scale_fill_brewer(palette="PRGn", direction=-1)
       g2 <- wellbeingData %>%
         mutate(category = 1) %>% # freq = n / sum(n),
         ggplot(aes(category, fill = Outlook)) + # freq,
         geom_bar(width = 0.1) + #stat="identity",
         scale_y_reverse() + 
         coord_flip()  +
         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
         theme(axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank()) +
         theme(legend.position = "bottom") + scale_fill_brewer(palette="PRGn", direction=-1)
       grid.arrange(g1, g2, nrow = 2, ncol=1, heights = c(1, 1))
     })

}

# Run the application 
shinyApp(ui = ui, server = server)

