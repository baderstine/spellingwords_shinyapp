# Author: Brian Derstine
# Date: 2023-02-20

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(words) 

# TODO:
# calculate and display total points possible
# All Words loading spinner? or just make the code faster
# settings button (Hard Mode: disables "All Words" tab OR removes the "word" column)
# user stats history of games played : 
#   (date of game, starting letters, required letter, # words found, # points, % of possible points)

# extract all valid words from the "words" package
all_words <- words %>%
  mutate(word = toupper(word)) %>%
  filter(word_length > 3) %>%
  mutate(letters = unlist(lapply(FUN = function(X) paste0(sort(unique(X)), collapse=""), 
                                 strsplit(word, "")))) %>%
  filter(nchar(letters) <= 7)

# extract all unique sets of 7 starting letters:
# 7-letter sets used in "septagrams" (words of any length that use all 7 letters)
main_word_letters <- all_words %>%
  filter(nchar(letters) == 7) %>%
  distinct(letters) %>%
  select(letters)

# all the "septagrams"
main_words <- all_words %>%
  filter(letters %in% main_word_letters$letters) %>%
  select(word)


# fixed positions of each letter label in the gameboard plot
position_coords <- data.frame(position = 1:7,
                              x = c(2.5, 3.5,
                                    2, 3, 4,
                                    2.5, 3.5),
                              y = c(3, 3,
                                    2, 2, 2, 
                                    1, 1))

# fixed vertex positions of the "tiles" around each letter in the gameboard plot
# 4 x, y coordinate pairs per tile, 7 tiles 
polygon_coords <- data.frame(position = rep(1:7, each=4),
                             xs = c(2, 3, 3, 2, 
                                    3, 4, 4, 3,
                                    1.5, 2.5, 2.5, 1.5,
                                    2.5, 3.5, 3.5, 2.5,
                                    3.5, 4.5, 4.5, 3.5,
                                    2, 3, 3, 2, 
                                    3, 4, 4, 3),
                             ys = c(2.5, 2.5, 3.5, 3.5,
                                    2.5, 2.5, 3.5, 3.5,
                                    1.5, 1.5, 2.5, 2.5,
                                    1.5, 1.5, 2.5, 2.5,
                                    1.5, 1.5, 2.5, 2.5,
                                    0.5, 0.5, 1.5, 1.5,
                                    0.5, 0.5, 1.5, 1.5))

get_click_position <- function(in_x, in_y) {
  # in_x, in_y : numeric : input$plot_click$x
  
  x1 <- polygon_coords %>%
    filter(in_x >= xs)
  x2 <- polygon_coords %>%
    filter(in_x <= xs)
  y1 <- polygon_coords %>%
    filter(in_y >= ys)
  y2 <- polygon_coords %>%
    filter(in_y <= ys)
  
  x1 %>%
    left_join(x2, by="position", multiple = "all") %>%
    left_join(y1, by="position", multiple = "all") %>%
    left_join(y2, by="position", multiple = "all") %>%
    drop_na() %>%
    distinct(position) %>%
    pull(position)
  
}

get_letters <- function(x = NA) {
  # x : NA or character string : input$manualLetters
  if(is.na(x)){
    main_word <- sample(main_word_letters$letters, 1, replace = F)[[1]]
    letter_pos <- sample(1:7, replace=F)
  } else {
    main_word <- x
    letter_pos <- 1:7
  }
  
  data.frame(letters = strsplit(main_word, split="")[[1]],
             stringsAsFactors = F) %>%
    mutate(position = letter_pos,
           pos_fixed = (position == 4))
}

rotate_letters <- function(in_letters) {
  # in_letters : data.frame : output from get_letters()
  
  new_position <- sample(c(1:3,5:7), replace=F)
  
  out_letters <- in_letters %>% 
    select(letters, position, pos_fixed) %>%
    distinct()
  
  out_letters$position[!out_letters$pos_fixed] = new_position

  out_letters
}

assign_coordinates <- function(x) {
  # x : data.frame : output from rotate_letters()
  x %>%  
    left_join(position_coords, by = "position") %>%
    left_join(polygon_coords, by = "position", multiple = "all")
}

get_word_points <- function(in_word) {
  # in_word : character string : local_vals$current_word
  out_pts <- case_when(
    nchar(in_word) == 4 ~ 1,
    TRUE ~ nchar(in_word))
  out_pts
}

get_all_words_table <- function(x, found_words) {
  # x : data.frame : local_vals$current_letters 
  # found_words : character vector : local_vals$found_words$word
  
  chk_letters <- x$letters
  req_letter <- x %>% 
    filter(pos_fixed) %>% 
    pull(letters)
  
  # this returns all words that use all the letters ("septagrams")
  septagrams <- all_words %>%
    filter(letters %in% paste0(sort(chk_letters), collapse="")) %>%
    pull(word)
  
  # find words that use (1) the required letter, and (2) any subset of only these letters
  all_words %>%
    filter(grepl(req_letter, letters, fixed=T)) %>%
    filter(sapply(letters, 
                  function(x) all(sapply(strsplit(x,"")[[1]], 
                                         function(l) grepl(l, paste0(chk_letters, collapse = ""))))
    )) %>% 
    mutate(septagram = if_else(word %in% septagrams, "✓", ""),
           found = if_else(word %in% found_words, "✓", "")) %>%
    arrange(-word_length, word) %>%
    select(-letters)
}

check_submitted_letters <- function(x) {
  # x : character string : input$manualLetters
  in_letters <- toupper(paste0(unique(strsplit(as.character(x), "")[[1]]), 
                               collapse="")
  )
  
  ## CASE: must be exactly 7 unique letters ----
  if (!grepl(pattern = "[[:upper:]]{7}", in_letters) | nchar(in_letters) != 7) {
    showModal(
      modalDialog(
        title = "Womp Womp!",
        "Invalid Entry, please enter 7 unique characters (letters only)",
        easyClose = TRUE
      )
    )
    return("error")
  } 
  
  ## CASE: must be a valid word that uses all 7 letters ---- 
  if (!paste0(sort(strsplit(in_letters, "")[[1]]), collapse="") %in% all_words$letters) {
    showModal(
      modalDialog(
        title = "Womp Womp!",
        "Invalid Entry, no words using all 7 of these letters exist.",
        easyClose = TRUE
      )
    )
    return("error")
  }
  
  return(in_letters)
}

# UI ---- 
ui <- fluidPage(
  # bug: causes odd selection artifacts so don't use
  # tags$script('
  #   $(document).on("keypress", function (e) {
  #     if(e.keyCode == 13){
  #       Shiny.setInputValue("submitBtn", 1, {priority: "event"});
  #     }
  #     if(e.keyCode == 8 | e.keyCode == 46){
  #       Shiny.setInputValue("deleteBtn", 1, {priority: "event"});
  #     }
  #   });
  # '), 
  
  titlePanel("Spelling Words"),
  
  mainPanel(width=12, 
    tabsetPanel(id = "gameTabs", type = "pills", 
      ## Gameboard Tab ---- 
      tabPanel(title="Game",
               column(width=8, align = "center", 
                      fluidRow(
                        strong("Points: "), 
                        textOutput(outputId = "pointsText", inline = T)
                      ),
                      plotOutput("gameBoard", height=300,
                                 click = "plot_click"),
                      fluidRow(
                        actionButton("deleteBtn",  "Delete",  style="padding: 20px 16px; margin: 8px"),
                        actionButton("shuffleBtn", "Shuffle", style="padding: 20px 16px; margin: 8px"),
                        actionButton("submitBtn",  "Submit",  style="padding: 20px 16px; margin: 8px")
                      ),
                      uiOutput("myWord")
               )
      ),
      ## Found Words Tab ---- 
      tabPanel(title="My Words",
               p(),
               tableOutput(outputId = "foundWordsTable")
      ),
      ## All Words Tab ---- 
      tabPanel(title="All Words",
               DT::dataTableOutput("allWordsTable")
      ),
      ## New Game Tab ---- 
      tabPanel(title = "New Game",
               p(),
               strong("Choose For Me:"),
               p(),
               actionButton(inputId = "newRandomWordBtn", label = "Auto-Select!"),
               p(),
               textInput(inputId = "manualLetters", label = "Choose My Own:"),
               actionButton(inputId = "submitAsIsLettersBtn", label = "Submit"),
               actionButton(inputId = "submitRandLettersBtn", label = "Shuffle Submit")
      )
    )
  )
)

# Server ---- 
server <- function(session, input, output) {
  
  # local session values ----
  local_vals <- reactiveValues(current_word = "",
                               current_letters = rotate_letters(get_letters()),
                               found_words = data.frame(word = NULL, points = NULL),
                               found_points = 0)

  
  # Gameboard UI ---- 
  ## Current Word ----
  output$myWord <- renderUI({
    # oh my word!
    tags$h1(local_vals$current_word)
  })
  
  ## Current Points ---- 
  output$pointsText <- renderText({
    local_vals$found_points <- sum(local_vals$found_words$points)
    local_vals$found_points
    
    # TODO: display X/Y points (%)
  })
  
  ## Game Plot
  output$gameBoard <- renderPlot({
    
    dat_plot <- assign_coordinates(local_vals$current_letters)
    
    ggplot(dat_plot) +
      geom_polygon(aes(xs, ys, group=letters, fill=pos_fixed), linewidth=2, color="white") +
      geom_text(aes(x, y, label=letters), size=12, fontface = "bold", color="white") +
      theme_void() +
      theme(aspect.ratio = 1) +
      scale_fill_manual(values=c("cornflowerblue","brown")) +
      guides(fill = "none")
  
  })
  
  # Gameboard Events ---- 
  ## Plot Click ---- 
  observeEvent(input$plot_click, {
    # record the letter that was clicked
    
    cx = input$plot_click$x
    cy = input$plot_click$y
    
    click_pos <- get_click_position(cx, cy)
    
    if(isTruthy(click_pos)) {
      click_letter <- local_vals$current_letters %>%
        filter(position == click_pos) %>%
        distinct(letters) %>%
        pull(letters)
      
      local_vals$current_word = paste0(local_vals$current_word, click_letter)
    }
  })
  
  ## Submit Word Button ---- 
  observeEvent(input$submitBtn, {
    # TODO: refactor showModal into a function?
    chk_word <- local_vals$current_word
    
    ### CASE: chk_word must be >= 4 characters in length ----
    if(nchar(chk_word) < 4) { 
      showModal(
        modalDialog(title = "Mrrp!",
                    "Not enough letters!",
                    easyClose = TRUE
        )
      )
      local_vals$current_word = ""
      return()
    }
    
    req_letter <- local_vals$current_letters %>%
      filter(pos_fixed) %>%
      distinct(letters)
    
    ## CASE: chk_word must use req_letter ----
    if(!grepl(req_letter, chk_word, fixed=T)) {
      showModal(
        modalDialog(title = "Whoa!",
                    "Missing required letter!",
                    easyClose = TRUE
        )
      )
      local_vals$current_word = ""
      return()
    }
    
    ## CASE: chk_word must not have been found already ----
    if(chk_word %in% local_vals$found_words$word) { 
      showModal(
        modalDialog(title = "Aww Snap!",
                    "Already got that one!",
                    easyClose = TRUE
        )
      )
      local_vals$current_word = ""
      return()
    }
    
    ## CASE: chk_word must be a valid word ----
    if(!chk_word %in% all_words$word) {
      showModal(
        modalDialog(title = "Womp Womp!",
                    paste(chk_word, "is not a valid word!"),
                    easyClose = TRUE
        )
      )
      local_vals$current_word = ""
      return()
    }
    
    ## SPECIAL CASE: chk_word uses all 7 of the letters (congrats!) ----
    if(chk_word %in% main_words$word) {
      showModal(
        modalDialog(title = "Wowzer!",
                    "Septagram!",
                    easyClose = TRUE
        )
      )
    }
    
    local_vals$found_words <- local_vals$found_words %>%
      bind_rows(data.frame(word = chk_word,
                           points = get_word_points(chk_word)))
    
    # reset current word
    local_vals$current_word = ""
  })
  
  ## Delete Letter Button ----
  observeEvent(input$deleteBtn, {
    # print(local_vals$current_word)
    local_vals$current_word = substr(local_vals$current_word, 1, (nchar(local_vals$current_word)-1))
  })
  
  ## Shuffle Letters Button ----
  observeEvent(input$shuffleBtn, {
    local_vals$current_letters <- rotate_letters(local_vals$current_letters)
  })
  
  
  # Found Words UI ---- 
  output$foundWordsTable <- renderTable({
    local_vals$found_words
  })
  

  # All Words UI ---- 
  output$allWordsTable <- DT::renderDataTable({ 
    get_all_words_table(local_vals$current_letters, local_vals$found_words$word) %>%
      tibble::rowid_to_column() 
  }, options = list(paging = F), rownames = F)
  

  # New Game Events ---- 
  ## New Random Word Button ----
  observeEvent(input$newRandomWordBtn, {
    # "randomly" select new set of letters
    local_vals$current_letters <- rotate_letters(get_letters())
    local_vals$current_word <- ""
    local_vals$found_words = data.frame(word = NULL, points = NULL)
    local_vals$found_points = 0
    updateTabsetPanel(session, "gameTabs", selected = "Game")
  })

  
  ## New Manual (As Is) Word Button ---- 
  observeEvent(input$submitAsIsLettersBtn, {
    result <- check_submitted_letters(input$manualLetters)
    
    if(result == "error")
      return()
    
    local_vals$current_letters <- get_letters(result)
    local_vals$current_word <- ""
    local_vals$found_words = data.frame(word = NULL, points = NULL)
    local_vals$found_points = 0
    updateTextInput(inputId = "manualLetters", value="")
    updateTabsetPanel(session, "gameTabs", selected = "Game")
    
  })
  
  ## New Manual (Shuffled) Word Button ----
  observeEvent(input$submitRandLettersBtn, {

    result <- check_submitted_letters(input$manualLetters)
    
    if(result == "error")
      return()
    
    local_vals$current_letters <- get_letters(x = paste0(sample(strsplit(result,"")[[1]], 
                                                                size = 7, 
                                                                replace = F), 
                                                         collapse=""))
    local_vals$current_word <- ""
    local_vals$found_words = data.frame(word = NULL, points = NULL)
    local_vals$found_points = 0
    updateTextInput(inputId = "manualLetters", value="")
    updateTabsetPanel(session, "gameTabs", selected = "Game")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
