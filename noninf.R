# Code from shiny::runExample("01_hello')

library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel("Noninf"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(6,
               numericInput("ev_tx", "Event: tegoprazan", value = 49)
        ),
        column(6,
               numericInput("n_tx", "N: tegoprazan", value = 64)
        )
        
      ),
      fluidRow(
        column(6,
               numericInput("ev_control", "Event: lansoprazole", value = 223)
        ),
        column(6,
               numericInput("n_control", "N: lansoprazole", value = 295)
        )
        
      ),
      sliderInput("margin", "Non-inferiority margin(Compare P_trt + margin vs P_control", value = 0.11, min = 0, max = 1, step = 0.01),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tableOutput("res_noninf")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$res_noninf <- renderTable({
    
    diff <- input$ev_tx/input$n_tx - input$ev_control/input$n_control
    #ci <- paste0(round(zz$conf.int * 100, 2), collapse = " ~ ")
    
    # 비율 차이와 그에 대한 95% 신뢰구간 계산
    p <- (input$ev_tx + input$ev_control)/(input$n_tx + input$n_control)
    se <- sqrt(p * (1 - p) * (1/input$n_tx + 1/input$n_control))
    lower <- diff - qnorm(0.975) * se 
    upper <- diff + qnorm(0.975) * se 
    ci <- paste0(round(lower, 2), " ~ ", round(upper, 2))
    pv.sup <- 2 * pnorm(-abs(diff)/se)
    pv.noninf <- 2 * pnorm(-abs(diff + input$margin)/se)
    
    res.p <- data.table::data.table(`Rate difference(Trt - Control, 2-sided 95% CI)` = paste0(round(diff, 2), " (", ci, ")"), 
                                    `p.noninf` = ifelse(pv.noninf < 0.001, "<0.001", round(pv.noninf, 3)), pv.sup = ifelse(pv.sup < 0.001, "<0.001", round(pv.sup, 3)))
    names(res.p)[-1] <- c("P non-inferiority", "P superiority")
    res.p
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)