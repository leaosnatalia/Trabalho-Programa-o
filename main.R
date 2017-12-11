library(shiny)

source("PNAD.R")

ui <- fluidPage(
    img(), theme = "nav.css",
         sidebarLayout(
           sidebarPanel(width = 4,
                        helpText("Selecione abaixo um ano para visualizar os dados sobre a diferença de renda entre homens e mulheres"),
                        selectInput('anoPNAD', 'Temas', c("1995", "2005", "2015"), selected = c("1995")),
                        downloadButton('download', 'Baixar dados')
           ),
           mainPanel(
             plotOutput("plotMapa")
           )
         )
)

server <- function(input, output) {
  allValenciasTopicos <- reactive({
    validate(
      need(input$anoPNAD != 0, "Por favor, selecione um tema na lista ao lado.")
    )
    getLineChartData(input$anoPNAD)
  })
  
  output$plotMapa <- renderPlot({
    dados_mapa <- allValenciasTopicos()
    
    if(length(dados_mapa) == 0){
      return()
    }
    
    ggplot(data = dados_mapa, aes(x = longest, y = latest, group = group)) +
      geom_polygon(aes(fill = difrenda)) +
      scale_fill_viridis(name = "Razão da renda por sexo") +
      labs(title = "Diferencial de Rendimento por Gênero") +
      theme(panel.background = element_rect(fill = "#3E3E3E")
            ,plot.background = element_rect(fill = "#3E3E3E")
            ,legend.background = element_blank()
            ,axis.title = element_blank()
            ,axis.text = element_blank()
            ,axis.ticks = element_blank()
            ,panel.grid = element_blank()
            ,text = element_text(family = "Gill Sans", color = "#DDDDDD")
            ,plot.title = element_text(size = 32)
            ,legend.position = c(.18,.375)
      ) +
      geom_path(data = mapa_est_geral, aes(longest, latest, group=group),
                color = "black", size = 0.1)
    })
}

shinyApp(ui = ui, server = server)