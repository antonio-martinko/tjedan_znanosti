library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel(h1("24-satna tjelesna aktivnost, sedentarno ponašanje i spavanje")),
  
  fluidRow(
    column(width = 6,
           sidebarPanel(
             h2("Razmisli o jednom svom školskom danu!"),
             tags$img(src = "24h.png",
                      width = 25, height = 25),
             width = 6,
             sliderInput("spavanje",
                         "Koliko dugo spavaš (razmisli o danu kada ideš u školu ujutro)",
                         min = 5,
                         max = 12,
                         value = 8,
                         step = 0.5),
             sliderInput("aktivnost",
                         "Koliko ukupno vremena provedeš u tjelesnim aktivnostima od kojih se umoriš, oznojiš, brže dišeš i srce ti brže kuca?",
                         min = 0,
                         max = 4,
                         value = 1,
                         step = 0.25),
             sliderInput("sedentarno",
                         "Koliko vremena u jednom danu se zabavljaš sa mobitelom, računalom ili gledanjem televizije? (korištenje računala za zadaću se ne broji)",
                         min = 0,
                         max = 8,
                         value = 1,
                         step = 0.25),
             sliderInput("jakost",
                         "Koliko dana radiš vježbe koje jačaju kosti i mišiće (trčanje, skakanje, penjanje, utezi)",
                         min = 0,
                         max = 7,
                         value = 0)
           )
    ),
    column(width = 4),
    
    column(width = 6,
           plotOutput("plot_spavanje"),
           p("Preporučuje se najmanje 60 minuta na dan tjelesnih aktivnosti umjerenog do visokog intenziteta (aktivnosti u kojima se dijete/mlada osoba zadiše i bar malo oznoji)."),
           plotOutput("plot_aktivnost"),
           plotOutput("plot_sedentarno"),
           plotOutput("plot_jakost")
    )
  )
)

server <- function(input, output) {
  
  output$plot_spavanje <- renderPlot({
    ggplot(data.frame(y = input$spavanje), aes(x = "", y = y)) +
      geom_bar(stat = "identity", fill = "darkorchid", alpha = 0.5) +
      geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 8, ymax = 10),
                fill = "red", alpha = 0.5, size = 1.5) +
      xlab("Spavanje") +
      ylim(0, 24) +
      ylab("Trajanje u satima") +
      theme_minimal()
  })
  
  output$plot_aktivnost <- renderPlot({
    ggplot(data.frame(y = input$aktivnost), aes(x = "", y = y)) +
      geom_bar(stat = "identity", fill = "cornflowerblue", alpha = 0.5) +
      geom_hline(yintercept = 1, color = "red", size = 1.5) +
      xlab("Tjelesna aktivnost") +
      ylim(0, 24) +
      ylab("Trajanje u satima") +
      theme_minimal()
  })
  
  output$plot_sedentarno <- renderPlot({
    ggplot(data.frame(y = input$sedentarno), aes(x = "", y = y)) +
      geom_bar(stat = "identity", fill = "darkolivegreen3", alpha = 0.5) +
      geom_hline(yintercept = 2, color = "red", size = 1.5) +
      xlab("Sedentarno ponašanje") +
      ylim(0, 24) +
      ylab("Trajanje u satima") +
      theme_minimal()
  })
  
  output$plot_jakost <- renderPlot({
    ggplot(data.frame(y = input$jakost), aes(x = "", y = y)) +
      geom_bar(stat = "identity", fill = "darkgoldenrod1", alpha = 0.5) +
      geom_hline(yintercept = 3, color = "red", size = 1.5) +
      xlab("Vježbe za jačanje kosti i mišića") +
      ylim(0, 7) +
      ylab("Trajanje u satima") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
