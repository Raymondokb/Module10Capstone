#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
#unigram <- readRDS("unigram.rds")
#enableBookmarking("disable")
bigram <- readRDS("bigram.rds")
trigram <- readRDS("trigram.rds")
fourgram <- readRDS("fourgram.rds")
#fivegram <- readRDS("fivegram.rds")
#sixgram <- readRDS("sixgram.rds")
candidate_list <- data.frame(next_word=c(),prob=c())

#ascii <- rawToChar(as.raw(0:127), multiple=TRUE)
#punctuations <- ascii[grepl('[[:punct:]]', ascii)]
#punct_regex <- paste(punctuations, collapse="+|\\")
punct_regex <- "!+|\\\"+|\\#+|\\$+|\\%+|\\&+|\\'+|\\(+|\\)+|\\*+|\\++|\\,+|\\-+|\\.+|\\/+|\\:+|\\;+|\\<+|\\=+|\\>+|\\?+|\\@+|\\[+|\\\\+|\\]+|\\^+|\\_+|\\`+|\\{+|\\|+|\\}+|\\~"
# Define server logic required to draw a histogram
shinyServer(function(input2, output) {
   
  # output$distPlot <- renderPlot({
  #   
  #   # generate bins based on input$bins from ui.R
  #   x    <- faithful[, 2] 
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #   
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
  #   
  # })
  #run_next_word <- eventReactive(input$go, {
  #output$value <- renderText({ paste(input2$caption, "hi", sep=" ") })
    #user_predict <- function(user_input){
  pred_w <- reactive({
  user_input <- input2$caption
        input <- tolower(user_input)
        input <- trimws(input)
        word_parts <- str_split(input, paste("\\",punct_regex,"+","|\\s+",sep="")) #Overlooks multiple whitesapces
        word_number <- length(word_parts[[1]])

        clean_word <- paste(word_parts[[1]][1:word_number], collapse= " ")
        clean_word <- trimws(clean_word)

        #Run again in case
        user_input <- clean_word
        input <- tolower(user_input)
        input <- trimws(input)
        word_parts <- str_split(input, paste("\\",punct_regex,"+","|\\s+",sep="")) #Overlooks multiple whitesapces
        word_number <- length(word_parts[[1]])

        clean_word <- paste(word_parts[[1]][1:word_number], collapse= " ")
        clean_word <- trimws(clean_word)
        #

        if(clean_word==""){
          word_number <- 0
        }
        if(word_number > 5){
          clean_word <- paste(word_parts[[1]][(word_number-5+1):word_number], collapse= " ")
        }
        input_clean <- as.character(trimws(clean_word))
        word_info <- data.frame(clean_word=clean_word, word_number=word_number)


        if(word_info[1,2] == 0){
          print("No valid input yet")
        } else {
          candidate_list <- data.frame(next_word=c(),prob=c())
          six_prob_left <- 1; five_prob_left <- 1; four_prob_left <- 1;
          tri_prob_left <- 1; bi_prob_left <- 1;
          gamma6 <- 0.5; gamma5 <- 0.5; gamma4 <- 0.5; gamma3 <- 0.5; gamma2 <- 0.5;
          existing_words <- c()
          # if(word_info[1,2]>=5){
          #   input_sixgram <- paste(word_parts[[1]][(word_number-5+1):word_number], collapse= " ")
          #   six <- sixgram[grep(paste("^",input_sixgram,sep=""), sixgram$term),]
          #   six$next_word <- str_split_fixed(six$term," ", 6)[,6]
          #   six$prob <- (six$Freq - gamma6)/sum(six$Freq)
          #   existing_words <- six$next_word #Add to existing_words list
          #   six_prob_left <- 1 - sum(six$prob)
          #   candidate_list <- data.frame(next_word=six$next_word, prob=six$prob )
          # 
          # }
          # if(word_info[1,2]>=4){
          #   input_fivegram <- paste(word_parts[[1]][(word_number-5+2):word_number], collapse= " ")
          #   five <- fivegram[grep(paste("^",input_fivegram,sep=""), fivegram$term),]
          #   five$next_word <- str_split_fixed(five$term," ", 5)[,5]
          #   five <- five[!(five$next_word%in%existing_words),] #Removes from higher order n-gram
          #   five$prob <- (five$Freq - gamma5)/sum(five$Freq) * six_prob_left
          #   existing_words <- c(existing_words, five$next_word) #Add to existing_words list
          #   five_prob_left <- six_prob_left - sum(five$prob)
          #   candidate_list <- rbind(candidate_list, data.frame(next_word=five$next_word, prob=five$prob ))
          # 
          # }
          if(word_info[1,2]>=3){
            input_fourgram <- paste(word_parts[[1]][(word_number-5+3):word_number], collapse= " ")
            four <- fourgram[grep(paste("^",input_fourgram,sep=""), fourgram$term), ]
            four$next_word <- str_split_fixed(four$term," ", 4)[,4]
            four <- four[!(four$next_word%in%existing_words),] #Removes from higher order n-gram
            four$prob <- (four$Freq - gamma4)/sum(four$Freq) * five_prob_left
            existing_words <- c(existing_words, four$next_word)
            four_prob_left <- five_prob_left - sum(four$prob)
            candidate_list <- rbind(candidate_list, data.frame(next_word=four$next_word, prob=four$prob ))

          }
          if(word_info[1,2]>=2){
            input_trigram <- paste(word_parts[[1]][(word_number-5+4):word_number], collapse= " ")
            tri <- trigram[grep(paste("^",input_trigram,sep=""), trigram$term), ]
            tri$next_word <- str_split_fixed(tri$term," ", 3)[,3]
            tri <- tri[!(tri$next_word%in%existing_words), ] #Removes from higher order n-gram
            tri$prob <- (tri$Freq - gamma3)/sum(tri$Freq) * four_prob_left
            existing_words <- c(existing_words, tri$next_word)
            tri_prob_left <- four_prob_left - sum(tri$prob)
            candidate_list <- rbind(candidate_list, data.frame(next_word=tri$next_word, prob=tri$prob ))

          }
          if(word_info[1,2]>=1){
            input_bigram <- paste(word_parts[[1]][(word_number-5+5):word_number], collapse= " ")
            bi <- bigram[grep(paste("^",input_bigram,sep=""), bigram$term),]
            bi$next_word <- str_split_fixed(bi$term," ", 2)[,2]
            bi <- bi[!(bi$next_word%in%existing_words),] #Removes from higher order n-gram
            bi$prob <- (bi$Freq)/sum(bi$Freq) * tri_prob_left
            existing_words <- c(existing_words, bi$next_word)
            bi_prob_left <- tri_prob_left - sum(bi$prob)
            candidate_list <- rbind(candidate_list, data.frame(next_word=bi$next_word, prob=bi$prob ))

          }
          candidate_list <- candidate_list[order(candidate_list$prob,decreasing=TRUE),]
          if(dim(candidate_list)[1]==0){
            print("Invalid input/spelling error/phrase not in corpus")
          } else{
            #cat("Your top word for '", input_clean, "' is >>>",as.character(candidate_list$next_word[1]) ,"<<<")
            candidate_list <- candidate_list
            qq <- as.character(candidate_list$next_word[1])
          }

        }

})
  
  pred_dataframe <-  reactive({
    user_input <- input2$caption
    input <- tolower(user_input)
    input <- trimws(input)
    word_parts <- str_split(input, paste("\\",punct_regex,"+","|\\s+",sep="")) #Overlooks multiple whitesapces
    word_number <- length(word_parts[[1]])
    
    clean_word <- paste(word_parts[[1]][1:word_number], collapse= " ")
    clean_word <- trimws(clean_word)
    
    #Run again in case
    user_input <- clean_word
    input <- tolower(user_input)
    input <- trimws(input)
    word_parts <- str_split(input, paste("\\",punct_regex,"+","|\\s+",sep="")) #Overlooks multiple whitesapces
    word_number <- length(word_parts[[1]])
    
    clean_word <- paste(word_parts[[1]][1:word_number], collapse= " ")
    clean_word <- trimws(clean_word)
    #
    
    if(clean_word==""){
      word_number <- 0
    }
    if(word_number > 5){
      clean_word <- paste(word_parts[[1]][(word_number-5+1):word_number], collapse= " ")
    }
    input_clean <- as.character(trimws(clean_word))
    word_info <- data.frame(clean_word=clean_word, word_number=word_number)
    
    
    if(word_info[1,2] == 0){
      print("No valid input yet")
    } else {
      candidate_list <- data.frame(next_word=c(),prob=c())
      six_prob_left <- 1; five_prob_left <- 1; four_prob_left <- 1;
      tri_prob_left <- 1; bi_prob_left <- 1;
      gamma6 <- 0.5; gamma5 <- 0.5; gamma4 <- 0.5; gamma3 <- 0.5; gamma2 <- 0.5;
      existing_words <- c()
      # if(word_info[1,2]>=5){
      #   input_sixgram <- paste(word_parts[[1]][(word_number-5+1):word_number], collapse= " ")
      #   six <- sixgram[grep(paste("^",input_sixgram,sep=""), sixgram$term),]
      #   six$next_word <- str_split_fixed(six$term," ", 6)[,6]
      #   six$prob <- (six$Freq - gamma6)/sum(six$Freq)
      #   existing_words <- six$next_word #Add to existing_words list
      #   six_prob_left <- 1 - sum(six$prob)
      #   candidate_list <- data.frame(next_word=six$next_word, prob=six$prob )
      #   
      # }
      # if(word_info[1,2]>=4){
      #   input_fivegram <- paste(word_parts[[1]][(word_number-5+2):word_number], collapse= " ")
      #   five <- fivegram[grep(paste("^",input_fivegram,sep=""), fivegram$term),]
      #   five$next_word <- str_split_fixed(five$term," ", 5)[,5]
      #   five <- five[!(five$next_word%in%existing_words),] #Removes from higher order n-gram
      #   five$prob <- (five$Freq - gamma5)/sum(five$Freq) * six_prob_left
      #   existing_words <- c(existing_words, five$next_word) #Add to existing_words list
      #   five_prob_left <- six_prob_left - sum(five$prob)
      #   candidate_list <- rbind(candidate_list, data.frame(next_word=five$next_word, prob=five$prob ))
      #   
      # }
      if(word_info[1,2]>=3){
        input_fourgram <- paste(word_parts[[1]][(word_number-5+3):word_number], collapse= " ")
        four <- fourgram[grep(paste("^",input_fourgram,sep=""), fourgram$term), ]
        four$next_word <- str_split_fixed(four$term," ", 4)[,4]
        four <- four[!(four$next_word%in%existing_words),] #Removes from higher order n-gram
        four$prob <- (four$Freq - gamma4)/sum(four$Freq) * five_prob_left
        existing_words <- c(existing_words, four$next_word)
        four_prob_left <- five_prob_left - sum(four$prob)
        candidate_list <- rbind(candidate_list, data.frame(next_word=four$next_word, prob=four$prob ))
        
      }
      if(word_info[1,2]>=2){
        input_trigram <- paste(word_parts[[1]][(word_number-5+4):word_number], collapse= " ")
        tri <- trigram[grep(paste("^",input_trigram,sep=""), trigram$term), ]
        tri$next_word <- str_split_fixed(tri$term," ", 3)[,3]
        tri <- tri[!(tri$next_word%in%existing_words), ] #Removes from higher order n-gram
        tri$prob <- (tri$Freq - gamma3)/sum(tri$Freq) * four_prob_left
        existing_words <- c(existing_words, tri$next_word)
        tri_prob_left <- four_prob_left - sum(tri$prob)
        candidate_list <- rbind(candidate_list, data.frame(next_word=tri$next_word, prob=tri$prob ))
        
      }
      if(word_info[1,2]>=1){
        input_bigram <- paste(word_parts[[1]][(word_number-5+5):word_number], collapse= " ")
        bi <- bigram[grep(paste("^",input_bigram,sep=""), bigram$term),]
        bi$next_word <- str_split_fixed(bi$term," ", 2)[,2]
        bi <- bi[!(bi$next_word%in%existing_words),] #Removes from higher order n-gram
        bi$prob <- (bi$Freq)/sum(bi$Freq) * tri_prob_left
        existing_words <- c(existing_words, bi$next_word)
        bi_prob_left <- tri_prob_left - sum(bi$prob)
        candidate_list <- rbind(candidate_list, data.frame(next_word=bi$next_word, prob=bi$prob ))
        
      }
      candidate_list <- candidate_list[order(candidate_list$prob,decreasing=TRUE),]
      if(dim(candidate_list)[1]==0){
        print("Invalid input/spelling error/phrase not in corpus")
      } else{
        #cat("Your top word for '", input_clean, "' is >>>",as.character(candidate_list$next_word[1]) ,"<<<")
        candidate_list <- candidate_list
        qq <- candidate_list
      }
      
    }
  })
   #   }
    #user_input <- input$caption
    #output_next <- user_predict(user_input)
    #output$value <- renderText({ output_next })
  #})
  # run_next_word()
  #run_next_word <- eventReactive(input$go, {

    output$value <- renderText({ paste(input2$caption,pred_w(),sep=" ") })
    output$mytable <- renderDataTable({head(pred_dataframe())})
    
    
  #})
})
