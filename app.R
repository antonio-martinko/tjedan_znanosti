library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel(h2("24-satna tjelesna aktivnost, sedentarno ponašanje i spavanje")),
  theme = bslib::bs_theme(bootswatch = "minty"),
  hr(),
  
  fluidRow(
    column(width = 6,
           sidebarPanel(
             h4("Razmisli o jednom svom školskom danu!"),
             img(src = "clock.png",
                 width = 100, height = 100, align = "center"),
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
             h4("Sad razmisli o jednom uobičajenom tjednu"),
             img(src = "week.png",
                 width = 100, height = 100, align = "center"),
             sliderInput("jakost",
                         "Koliko dana radiš vježbe koje jačaju kosti i mišiće (trčanje, skakanje, penjanje, utezi)",
                         min = 0,
                         max = 7,
                         value = 0)
           )
    ),
    
    column(width = 3,
           p("Preporučuje se 9–11 sati (5-13 god.)  8–10 sati (14-17 god.) neprekinutog spavanja tijekom noći."),
           img(src = "sleeping.png",
               width = 250, height = 250, align = "center"),
           hr(),
           p("Preporučuje se najmanje 60 minuta na dan tjelesnih aktivnosti umjerenog do visokog intenziteta (aktivnosti u kojima se dijete/mlada osoba zadiše i bar malo oznoji)."),
           img(src = "activities.png",
               width = 250, align = "center"),
           hr(),
           p("Preporučuje se da vrijeme pred ekranima (npr., pred TV-om, računalom, tabletom ili mobitelom) bude ograničeno na najdulje 2 sata na dan."),
           img(src = "phone.png",
               width = 250, align = "center"),
           hr(),
           p("Važno je da dijete/mlada osoba svakodnevno provodi vrijeme u aerobnim tjelesnim aktivnostima, a najmanje 3 puta na tjedan u aktivnostima za jačanje mišića i kosti."),
           img(src = "strong.png",
               width = 250, height = 250, align = "center"),
           hr()),
    
    column(width = 3,
           plotOutput("plot_spavanje"),
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
      scale_y_continuous(breaks = seq(0, 24, by = 2), limits = c(0, 24)) +
      ylab("Trajanje u satima") +
      theme_bw()
  })
  
  output$plot_aktivnost <- renderPlot({
    ggplot(data.frame(y = input$aktivnost), aes(x = "", y = y)) +
      geom_bar(stat = "identity", fill = "cornflowerblue", alpha = 0.5) +
      geom_hline(yintercept = 1, color = "red", linewidth = 1.5) +
      xlab("Tjelesna aktivnost") +
      scale_y_continuous(breaks = seq(0, 24, by = 2), limits = c(0, 24)) +
      ylab("Trajanje u satima") +
      theme_bw()
  })
  
  output$plot_sedentarno <- renderPlot({
    ggplot(data.frame(y = input$sedentarno), aes(x = "", y = y)) +
      geom_bar(stat = "identity", fill = "darkolivegreen3", alpha = 0.5) +
      geom_hline(yintercept = 2, color = "red", linewidth = 1.5) +
      xlab("Sedentarno ponašanje") +
      scale_y_continuous(breaks = seq(0, 24, by = 2), limits = c(0, 24)) +
      ylab("Trajanje u satima") +
      theme_bw()
  })
  
  output$plot_jakost <- renderPlot({
    ggplot(data.frame(y = input$jakost), aes(x = "", y = y)) +
      geom_bar(stat = "identity", fill = "darkgoldenrod1", alpha = 0.5) +
      geom_hline(yintercept = 3, color = "red", linewidth = 1.5) +
      xlab("Vježbe za jačanje kosti i mišića") +
      scale_y_continuous(breaks = seq(0, 7, by = 1), limits = c(0, 7)) +
      ylab("Trajanje u satima") +
      theme_bw()
  })
}

shinyApp(ui = ui, server = server)
