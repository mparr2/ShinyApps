library(shiny)


# Define UI
ui <- fluidPage(
  titlePanel("Trolley Problem"),
  sidebarLayout(
    sidebarPanel(
      # Toggle switch input
      checkboxInput("lever_pull", "Do you pull the lever?", value = FALSE),
      # Run function
      actionButton("release_button", "Release the Trolley")
    ), 
    mainPanel(
      img(src = "https://i.redd.it/t2y9hby5foyc1.png", height = "300px", width = "450px"), 
      textOutput("num_press_output"),
      # Spacer
      br(),
      # Score output
      textOutput("score_output"),
      # Spacer
      br(),
      # total
      textOutput("total_output"),
      # Spacer
      br(),
      # Text output
      textOutput("text_output"),
      br(),
      # function output
      textOutput("trolley_output")
    )
  )
)



# Define server logic
server <- function(input, output) {
  
  score <- reactiveVal(0)
  total <- reactiveVal(0)
  num_press <- reactiveVal(0)
  top_box <- reactiveVal(sample(c(3:4), 1))
  bottom_box <- reactiveVal(sample(c(1:6), 1))
  out_text <- reactiveVal("INIT")
  
  # Output number of presses
  output$num_press_output <- renderText({
    paste("You have released", num_press(), "trollies")
  })
  
  # Output total dead
  output$total_output <- renderText({
    paste("You have killed", total(), "people")
  })
  
  # Output score
  output$score_output <- renderText({
    paste("You have saved", score())
  })
  
  # Output text based on toggle switch value
  output$text_output <- renderText({
    if (input$lever_pull) {
      return("You pull the lever. You can only hope to minimise the loss of life through your decision. There are no heroes here.")
    } else {
      return("You do not pull the lever. The decision of inaction is still an action. Widows will be made today.")
    }
  })
  
  observeEvent(input$release_button, {
    
    top_box(sample(c(3:4), 1))
    bottom_box(sample(c(1:6), 1))
      
      if(input$lever_pull){
        if(top_box()>bottom_box()){
          out_text(paste0("Your decision to intervene meant ", top_box(), " people died rather than only ", bottom_box()))
          score(score() + bottom_box() - top_box())
          total(total() + top_box())
        } else if(top_box()==bottom_box()){
          out_text(paste0("While it could perhaps be considered noble to attempt to save lives, it did not matter as ", top_box(), " people died regardless."))
          total(total() + top_box())
        } else {
          out_text(paste0("This is why you're the boss! You only killed ", top_box(), " people rather than ", bottom_box()))
          score(score() + bottom_box() - top_box())
          total(total() + top_box())
        }
      } else {
        if(bottom_box()>top_box()){
          out_text(paste0("By not acting ", bottom_box(), " people died needlessly rather than only ", top_box(), ". Are your hands clean?"))
          score(score() + top_box() - bottom_box())
          total(total() + bottom_box())
        } else if(bottom_box() == top_box()){
          out_text(paste0("Your decision would not have mattered. ", bottom_box(), " people died."))
          total(total() + bottom_box())
        } else {
          out_text(paste0("Amazing! Your decision to do nothing meant only ", bottom_box(), " people died rather than ", top_box(), "!"))
          score(score() + top_box() - bottom_box() )
          total(total() + bottom_box())
        }
      }

    num_press(num_press()+1)
    output$trolley_output <- renderText({
      out_text()
    })
    
  })
}


# Run the application
shinyApp(ui = ui, server = server)
