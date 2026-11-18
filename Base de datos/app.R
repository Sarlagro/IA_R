install.packages("shiny")
library(shiny)

archivo_excel <- "Rendimientos_decada.xlsx"

if (!file.exists(archivo_excel)) {
  stop("No se encontró el archivo ", archivo_excel, " en el directorio de la aplicación.")
}

datos <- readxl::read_excel(
  archivo_excel,
  na = c("", "NA", "N/A", "SD")
)

columnas_requeridas <- c("Cultivo", "Campaña", "Sup. Sembrada", "Sup. Cosechada")
columnas_faltantes <- setdiff(columnas_requeridas, names(datos))

if (length(columnas_faltantes) > 0) {
  stop(
    "Faltan columnas requeridas en el Excel: ",
    paste(columnas_faltantes, collapse = ", ")
  )
}

datos <- datos |>
  dplyr::filter(!is.na(.data$Cultivo), !is.na(.data$Campaña))

campanias <- sort(unique(datos$Campaña))
cultivos <- sort(unique(datos$Cultivo))

sumar_o_na <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

ui <- fluidPage(
  titlePanel("Superficie sembrada y cosechada por campaña"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "cultivo",
        "Cultivo",
        choices = cultivos,
        selected = cultivos[1]
      )
    ),
    mainPanel(
      plotOutput("superficies", height = "500px"),
      p(
        "Los valores disponibles se suman por campaña. Si todos faltan, ",
        "el resultado queda vacío y se muestra como un hueco, no como cero."
      )
    )
  )
)

server <- function(input, output, session) {
  datos_grafico <- reactive({
    datos |>
      dplyr::filter(.data$Cultivo == input$cultivo) |>
      dplyr::group_by(.data$Campaña) |>
      dplyr::summarise(
        `Sup. Sembrada` = sumar_o_na(.data$`Sup. Sembrada`),
        `Sup. Cosechada` = sumar_o_na(.data$`Sup. Cosechada`),
        .groups = "drop"
      ) |>
      tidyr::complete(Campaña = campanias) |>
      dplyr::mutate(Campaña = factor(.data$Campaña, levels = campanias)) |>
      tidyr::pivot_longer(
        cols = c("Sup. Sembrada", "Sup. Cosechada"),
        names_to = "Superficie",
        values_to = "Hectareas"
      ) |>
      dplyr::arrange(.data$Superficie, .data$Campaña) |>
      dplyr::group_by(.data$Superficie) |>
      dplyr::mutate(segmento = cumsum(is.na(.data$Hectareas))) |>
      dplyr::ungroup()
  })

  output$superficies <- renderPlot({
    ggplot2::ggplot(
      datos_grafico(),
      ggplot2::aes(
        x = .data$Campaña,
        y = .data$Hectareas,
        color = .data$Superficie,
        group = interaction(.data$Superficie, .data$segmento)
      )
    ) +
      ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +
      ggplot2::geom_point(size = 2, na.rm = TRUE) +
      ggplot2::scale_color_manual(
        values = c("Sup. Sembrada" = "#2878B5", "Sup. Cosechada" = "#E07A25")
      ) +
      ggplot2::labs(
        x = "Campaña",
        y = "Superficie (hectáreas)",
        color = "Superficie",
        title = paste("Cultivo:", input$cultivo)
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui = ui, server = server)
