library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel(h2("Kako koristiš vrijeme u svom danu?")),
  theme = bslib::bs_theme(bootswatch = "minty"),
  hr(),
  
  sidebarLayout(
    sidebarPanel(width = 8,
      splitLayout(
        # Column 1
        column(
          width = 8,
          h4("Razmisli o jednom svom", br(), "školskom danu!"),
          img(src = "clock.png", width = 95, height = 95),
          br(),
          sliderInput("spavanje", 
                      HTML("Koliko dugo spavaš (razmisli o danu kada ideš u školu ujutro)"),
                      min = 5, max = 12, value = 8, step = 0.5),
          br(), hr(), br(), br(), br(), br(), br(), br(),
          sliderInput("aktivnost",
                      HTML("Koliko ukupno vremena provedeš u tjelesnim aktivnostima od<br>kojih se umoriš, oznojiš, brže dišeš i srce ti brže kuca?"),
                      min = 0, max = 4, value = 1, step = 0.25),
          br(), hr(), br(), br(), br(), br(), br(),
          sliderInput("sedentarno",
                      HTML("Koliko vremena u jednom danu se zabavljaš sa mobitelom,<br>računalom ili gledanjem televizije? (korištenje računala<br>u školi ili za zadaću se ne broji)"),
                      min = 0, max = 8, value = 1, step = 0.25),
          br(), br(), br(), hr(), br(), br(),
          h4("Sad razmisli o jednom uobičajenom", br(),"tjednu!"),
          img(src = "week.png", width = 60, height = 60),
          br(), br(),
          sliderInput("jakost",
                      HTML("Koliko dana radiš vježbe koje jačaju kosti i mišiće (trčanje,<br>skakanje, penjanje, utezi)"),
                      min = 0, max = 7, value = 0)
        ),
        
        # Column 2
        column(
          width = 6,
          p("Trebaš spavati barem 9–11 sati (5-13 god.)", br(),"8–10 sati (14-17 god.) tijekom noći."),
          img(src = "sleeping.png", width = 250, height = 250, align = "center"),
          hr(),
          p("Potrudi se kretati najmanje 1 sat svaki dan, odnosno baviti", br(),"tjelesnim aktivnostima umjerenog do visokog intenziteta (aktivnosti", br(),"u kojima se zadišeš i bar malo oznojiš)."),
          img(src = "activities.png", width = 250, align = "center"),
          hr(),
          p("Ograniči vrijeme koje provodiš pred ekranima (npr., pred TV-om,", br(),"računalom, tabletom ili mobitelom) izvan škole na maksimalno", br(),"2 sata dnevno."),
          img(src = "phone.png", width = 250, align = "center"),
          hr(),
          p("Trči, skači i vježbaj mišiće barem 3 puta tjedno."),
          img(src = "strong.png", width = 200, height = 200, align = "left"),
          img(src = "activities.png", width = 200, align = "right")
          )
      )
    ),
    
    mainPanel(width = 4,
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
      geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 8, ymax = 11),
                fill = "chartreuse3", alpha = 0.5, size = 1.5) +
      xlab("Spavanje") +
      scale_y_continuous(breaks = seq(0, 24, by = 2), limits = c(0, 24)) +
      ylab("Trajanje u satima") +
      theme_bw()
  })
  
  output$plot_aktivnost <- renderPlot({
    ggplot(data.frame(y = input$aktivnost), aes(x = "", y = y)) +
      geom_bar(stat = "identity", fill = "cornflowerblue", alpha = 0.5) +
      geom_hline(yintercept = 1, color = "chartreuse3", linewidth = 1.5) +
      xlab("Tjelesna aktivnost") +
      scale_y_continuous(breaks = seq(0, 8, by = 2), limits = c(0, 8)) +
      ylab("Trajanje u satima") +
      theme_bw()
  })
  
  output$plot_sedentarno <- renderPlot({
    ggplot(data.frame(y = input$sedentarno), aes(x = "", y = y)) +
      geom_bar(stat = "identity", fill = "darkolivegreen3", alpha = 0.5) +
      geom_hline(yintercept = 2, color = "red", linewidth = 1.5) +
      xlab("Sedentarno ponašanje") +
      scale_y_continuous(breaks = seq(0, 8, by = 2), limits = c(0, 8)) +
      ylab("Trajanje u satima") +
      theme_bw()
  })
  
  output$plot_jakost <- renderPlot({
    ggplot(data.frame(y = input$jakost), aes(x = "", y = y)) +
      geom_bar(stat = "identity", fill = "darkgoldenrod1", alpha = 0.5) +
      geom_hline(yintercept = 3, color = "chartreuse3", linewidth = 1.5) +
      xlab("Vježbe za jačanje kosti i mišića") +
      scale_y_continuous(breaks = seq(0, 7, by = 1), limits = c(0, 7)) +
      ylab("Broj dana") +
      theme_bw()
  })
}

shinyApp(ui = ui, server = server)