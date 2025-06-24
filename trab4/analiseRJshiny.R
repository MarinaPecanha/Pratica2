library(shiny)
library(tidyverse)
library(dplyr)
library(sf)
library(geobr)
library(readxl)
library(irr)
library(plotly)
library(shinydashboard)
library(lubridate)

#LENDO OS SHAPES DE MG, DADOS DO CAMS E VON DONKELAR 
cams = read_csv("C:/Users/Marina/OneDrive/Desktop/prat2/trab4/PM2.5_diario_2023.csv")
donvonkelar = read_excel("C:/Users/Marina/OneDrive/Desktop/prat2/trab4/dados_completos_consolidado_donkelar.xlsx")

rj_cams = cams |> 
  mutate(data = ymd(Date),
         mes = month(data)) |> 
  mutate(estado = case_when(str_starts(Cod, "33") ~ "Rio de Janeiro")) |> 
  filter(!is.na(estado)) |> 
  mutate(Cod = as.character(Cod)) |> 
  select(Cod,data,mes,PM2.5)


rj_don = donvonkelar |> 
  filter(SIGLA_UF == 33) |> 
  mutate(CD_MUN = as.character(CD_MUN))

shaperj = read_sf("C:/Users/Marina/OneDrive/Desktop/prat2/trab4/RJ_Municipios_2022.shp")

rj_don_mensal = rj_don |> 
  mutate(mes = case_when(Mes == 1 ~ "Jan",
                         Mes == 2 ~ "Fev",
                         Mes == 3 ~ "Mar",
                         Mes == 4 ~ "Abril",
                         Mes == 5 ~ "Mai",
                         Mes == 6 ~ "Jun",
                         Mes == 7 ~ "Jul",
                         Mes == 8 ~ "Ago",
                         Mes == 9 ~ "Set",
                         Mes == 10 ~ "Out",
                         Mes == 11 ~ "Nov",
                         Mes == 12 ~ "Dez")) |>
  group_by(mes) |> 
  summarise(DON = mean(Media_PM25))


rj_cams_mensal = rj_cams |> 
  mutate(mes = case_when(mes == 1 ~ "Jan",
                         mes == 2 ~ "Fev",
                         mes == 3 ~ "Mar",
                         mes == 4 ~ "Abril",
                         mes == 5 ~ "Mai",
                         mes == 6 ~ "Jun",
                         mes == 7 ~ "Jul",
                         mes == 8 ~ "Ago",
                         mes == 9 ~ "Set",
                         mes == 10 ~ "Out",
                         mes == 11 ~ "Nov",
                         mes == 12 ~ "Dez")) |>
  group_by(mes) |> 
  summarise(CAMS = mean(PM2.5))


rj_mensal = rj_don_mensal |> 
  left_join(rj_cams_mensal,by = "mes")

rj_mensal_longo = rj_mensal |> 
  pivot_longer(cols = c(DON,CAMS),
               names_to = "pm",
               values_to = "valor")

# tabelas pros gráficos de dispersão

rj_cams_munmen = rj_cams |> 
  group_by(Cod,mes) |> 
  summarise(pm2.5_cams = mean(PM2.5))

rj_total = rj_don |> 
  left_join(rj_cams_munmen,join_by("CD_MUN" == "Cod","Mes" == "mes"))

rj_total = rj_total |> 
  mutate(diferenca = Media_PM25 - pm2.5_cams, # bland-altman
         media = (Media_PM25 + pm2.5_cams)/2)


rj_mensal = rj_mensal |>  #blandaltman mensal
  mutate(diferenca = DON - CAMS,
         media = (DON + CAMS)/2)


# tabela pro gráfico boxplot
rj_longo <- rj_total |> 
  pivot_longer(cols = c(Media_PM25,pm2.5_cams), 
               names_to = "pm", 
               values_to = "valor") |> 
  mutate(mes = case_when(Mes == 1 ~ "Jan",
                         Mes == 2 ~ "Fev",
                         Mes == 3 ~ "Mar",
                         Mes == 4 ~ "Abril",
                         Mes == 5 ~ "Mai",
                         Mes == 6 ~ "Jun",
                         Mes == 7 ~ "Jul",
                         Mes == 8 ~ "Ago",
                         Mes == 9 ~ "Set",
                         Mes == 10 ~ "Out",
                         Mes == 11 ~ "Nov",
                         Mes == 12 ~ "Dez")) |> 
  mutate(PM = case_when(pm %in% c("Media_PM25") ~ "DON",
                        pm %in% c("pm2.5_cams") ~ "CAMS"))

# tabela pros mapas

cams_cod = rj_cams |> 
  group_by(Cod) |> 
  summarise(pm2.5_cod_cams = mean(PM2.5))

don_cod = rj_don |> 
  group_by(CD_MUN) |> 
  summarise(pm2.5_cod_don = mean(Media_PM25))

cams_shape = left_join(x = shaperj,
                       y = cams_cod,
                       join_by("CD_MUN" == "Cod"))

don_shape = left_join(x = shaperj,
                      y = don_cod,
                      join_by("CD_MUN" == "CD_MUN"))

# ic
avaliacoes <- rj_total[, c("pm2.5_cams", "Media_PM25")]
iccUF = icc(avaliacoes, model = "twoway", type = "consistency", unit = "single")  # icc
ValorICC1 = round(iccUF$value,2)
IC95_ICC1 = c(round(iccUF$lbound,3), round(iccUF$ubound,3))

media_diff <- mean(rj_total$diferenca, na.rm = TRUE)
sd_diff <- sd(rj_total$diferenca, na.rm = TRUE)
limite_superior <- media_diff + 1.96 * sd_diff
limite_inferior <- media_diff - 1.96 * sd_diff

avaliacoes2 <- rj_mensal[, c("CAMS", "DON")]
iccUF2 = icc(avaliacoes, model = "twoway", type = "consistency", unit = "single")
ValorICC2 = round(iccUF2$value,2)
IC95_ICC2 = c(round(iccUF2$lbound,3), round(iccUF2$ubound,3))

media_diff2 <- mean(rj_mensal$diferenca, na.rm = TRUE)
sd_diff2 <- sd(rj_mensal$diferenca, na.rm = TRUE)

limite_superior2 <- media_diff2 + 1.96 * sd_diff2
limite_inferior2 <- media_diff2 - 1.96 * sd_diff2


# Define UI for application that draws a histogram
ui <- dashboardPage(
  header<-dashboardHeader(title = "PM2.5 no Rio de Janeiro"),
  sidebar<-dashboardSidebar(
    sidebarMenu(
      menuItem("Análise Mensal", icon = icon("bar-chart"),
               menuSubItem("Gráfico de Barras", tabName = "subitem1"),
               menuSubItem("Série", tabName = "subitem2"),
               menuSubItem("Boxplot", tabName = "subitem3")),
      menuItem("Comparando os Satélites", icon = icon("bar-chart"),
               menuSubItem("Comparação 1", tabName = "subitem4"),  # Juntamos os pares
               menuSubItem("Comparação 2", tabName = "subitem5")),  # em abas separadas
      menuItem("Mapas", icon = icon("map"),
               menuSubItem("Comparação de Mapas", tabName = 'subitem6'))
      )
    
    ),
  body<-dashboardBody(
    tabItems(
      tabItem("subitem1",
              plotOutput("barras")),
      tabItem("subitem2", plotlyOutput("serie")),
      tabItem("subitem3", plotlyOutput("boxplot")),
      tabItem("subitem4",
              fluidRow(
                box(width = 6, plotOutput("dispersao1")),
                box(width = 6, plotOutput("blandaltman1"))
              ),
              fluidRow(
                box(width = 12,
                    h4("Análise Comparativa 1"),
                    p("O gráfico à esquerda mostra a relação de dispersão entre as medições dos dois satélites, sendo a dispersão por cada ponto uma observação mensal de um município"),
                    p("O gráfico à direita apresenta a análise Bland-Altman, que avalia a concordância entre as medições, com as linhas tracejadas mostrando os limites de concordância de 95%."),
                    p(strong("Análise de Concordância (ICC):")),
                    p(paste0("ICC(C,1) = ", ValorICC1, " indica uma confiabilidade ", 
                             ifelse(ValorICC1 > 0.6, "excelente", 
                                    ifelse(ValorICC1 > 0.4, "boa", "fraca")), ".")),
                    p(paste0("Intervalo de Confiança 95%: [", IC95_ICC1[1], ", ", IC95_ICC1[2], "]")),
                    style = "margin-top: 20px; border-top: 2px solid #eee; padding-top: 15px;"
                )
              )
      ),
      tabItem("subitem5",
              fluidRow(
                box(width = 6, plotOutput("dispersao2")),
                box(width = 6, plotOutput("blandaltman2"))
              ),
              fluidRow(
                box(width = 12,
                    h4("Análise Comparativa 2"),
                    p("O gráfico de dispersão (esquerda) compara as medições dos dois métodos."),
                    p("O gráfico Bland-Altman (direita) mostra as diferenças entre os métodos."),
                    p(strong("Análise de Concordância (ICC):")),
                    p(paste0("ICC(C,1) = ", ValorICC2, " indica uma confiabilidade ", 
                             ifelse(ValorICC2 > 0.6, "excelente", 
                                    ifelse(ValorICC2 > 0.4, "boa", "fraca")), ".")),
                    p(paste0("Intervalo de Confiança 95%: [", IC95_ICC2[1], ", ", IC95_ICC2[2], "]")),
                    style = "margin-top: 20px; border-top: 2px solid #eee; padding-top: 15px;"
                    )
                )
              ),
      tabItem("subitem6", 
              fluidRow(
                box(title = "Mapa CAMS", width = 6, plotOutput("mapaCAMS")),
                box(title = "Mapa Von-Donkelar", width = 6, plotOutput("mapaVON"))
              ))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$barras <- renderPlot({
      
      ggplot(rj_mensal_longo,aes(x = factor(mes, levels = c("Jan", "Fev", "Mar", "Abril", "Mai", "Jun",
                                                            "Jul", "Ago", "Set", "Out", "Nov", "Dez")), y = valor,fill = pm)) +
        geom_bar(stat= 'identity',position = 'dodge') +
        labs(title = "Médias Mensais",x="Mês",y="PM 2.5μg/m³ Médio",fill = "",
             caption = "Fonte de dados: CAMS² e Vondonkelar³") +
        scale_fill_manual(values = c("pink", "#B25D91")) +
        theme_minimal()
    })
    
    output$serie = renderPlotly({
      
      ggplot(rj_mensal_longo,aes(x = factor(mes, levels = c("Jan", "Fev", "Mar", "Abril", "Mai", "Jun",
                                                            "Jul", "Ago", "Set", "Out", "Nov", "Dez")), y = valor,
                                 color = pm, group = pm)) +
        geom_line(size = 1.2) +
        labs(title = "Médias Mensais",x="Mês",y="PM2.5μg/m³  médio",color = "Satélites", 
             caption = "Fonte de dados: CAMS² e Vondonkelar³") +
        scale_color_manual(values = c("pink","#B25D91")) +
        scale_y_continuous(limits = c(9.5, 20)) +
        theme_minimal()
      
    })
    
    output$boxplot = renderPlotly({
      # boxplot
      ggplot(rj_longo,aes(x = factor(mes, levels = c("Jan", "Fev", "Mar", "Abril", "Mai", "Jun",
                                                     "Jul", "Ago", "Set", "Out", "Nov", "Dez")), y = valor,
                          fill = PM)) +
        geom_boxplot() +
        labs(title = "Médias Mensais",x="Mês",y="PM2.5μg/m³  médio", fill = "Satélites",
             caption = "Fonte de dados: CAMS² e Vondonkelar³") +
        scale_fill_manual(values = c("pink","#B25D91")) +
        theme_minimal()
    })
    
    output$dispersao1 = renderPlot({
      ggplot(rj_total, aes(x = pm2.5_cams, y = Media_PM25)) +
        geom_point() +
        labs(x = "PM2.5μg/m³ CAMS", y = "PM2.5μg/m³ DON", title = "Dispersão entre os PM2.5μg/m³",
             caption = "Fonte de dados: CAMS² e Vondonkelar³") +
        theme_minimal()
    })
    
    output$blandaltman1 = renderPlot({
      
      ggplot(rj_total, aes(x = media, y = diferenca)) +
        geom_point() +
        geom_hline(yintercept = media_diff, linetype = "dotted", color = "pink", size = 1) +
        annotate("text", x = 23, y = media_diff,
                 label = paste0("média: ", round(media_diff,2)),
                 vjust = -0.5, hjust = -0.3, color = "pink",
                 size = 4, fontface = "bold") +
        geom_hline(yintercept = limite_superior, linetype = "dashed", color = "#B25D91", size = 1) +
        geom_hline(yintercept = limite_inferior, linetype = "dashed", color = "#B25D91", size = 1) +
        labs(y = "Diferenças (DON - CAMS)", x = "Médias", title = "Bland-Altman", 
             caption = "Fonte de dados: CAMS² e Vondonkelar³") +
        theme_minimal()
    })
    
    output$dispersao2 = renderPlot({
      ggplot(rj_mensal, aes(x = CAMS, y = DON)) +
        geom_point() +
        labs(x = "PM2.5μg/m³ CAMS", y = "PM2.5μg/m³ DON", title = "Dispersão entre os PM2.5μg/m³ Mensal",
             caption = "Fonte de dados: CAMS¹ e Vondonkelar²") +
        theme_minimal()
    })
    
    output$blandaltman2 = renderPlot({
      
      ggplot(rj_mensal, aes(x = media, y = diferenca)) +
        geom_point() +
        geom_hline(yintercept = media_diff2, linetype = "dotted", color = "pink", size = 1) +
        annotate("text", x = 11, y = media_diff2,
                 label = paste0("média: ", round(media_diff2,2)),
                 vjust = -0.5, hjust = -0.3, color = "pink",
                 size = 4, fontface = "bold") +
        geom_hline(yintercept = limite_superior2, linetype = "dashed", color = "#B25D91", size = 1) +
        geom_hline(yintercept = limite_inferior2, linetype = "dashed", color = "#B25D91", size = 1) +
        labs(y = "Diferenças (DON - CAMS)", x = "Médias", title = "Bland-Altman Mensal", caption = "Fonte de dados: CAMS¹ e Vondonkelar²") +
        theme_minimal()
    })
    
    output$mapaCAMS = renderPlot({
      ggplot(cams_shape) +
        geom_sf(aes(fill=pm2.5_cod_cams)) +
        scale_fill_gradient(name = "CAMS\nPM2.5μg/m³",
                            low = "#ffe6f0", high = "#B25D91",
                            limits = c(7, 24)) +
        labs(caption = 'Fontes: Copernicus Atmosphere Monitoring Service \nDon Vonkelar') +
        theme_minimal()
      
    })
    
    output$mapaVON = renderPlot({
      ggplot(don_shape) +
        geom_sf(aes(fill=pm2.5_cod_don)) +
        scale_fill_gradient(name = "DON\nPM2.5μg/m³",
                            low = "#ffe6f0", high = "#B25D91",
                            limits = c(7, 24)) +
        labs(caption = 'Fontes: Copernicus Atmosphere Monitoring Service \nDon Vonkelar') +
        theme_minimal()
    })
    
    # Cálculo do ICC (coloque no início do server)
    avaliacoes = reactive({
      rj_total[, c("pm2.5_mensal", "Media_PM25")]
    })
    
    icc1 = reactive({
      icc(avaliacoes(), model = "twoway", type = "consistency", unit = "single")
    })
    
    output$icc_text <- renderText({
      icc_val <- round(icc1()$value, 2)
      icc_ci <- paste0("[", round(icc1()$lbound, 3), ", ", round(icc1()$ubound, 3), "]")
      
      paste("ICC(C,1) =", icc_val, "indica uma confiabilidade",
            ifelse(icc_val > 0.6, "excelente", ifelse(icc_val > 0.4, "boa", "fraca")),
            "\nIntervalo de Confiança 95%:", icc_ci,
            "\nO teste F rejeita a hipótese nula (ICC = 0), indicando concordância significativa.")
    })
    
    # Cálculo do ICC2
    avaliacoes2 = reactive({
      rj_mensal[, c("CAMS", "VON")]
    })
    
    icc2 = reactive({
      icc(avaliacoes2(), model = "twoway", type = "consistency", unit = "single")
    })
    
    # Valores para exibição
    output$icc2_text <- renderText({
      icc_val <- round(icc2()$value, 3)
      icc_ci <- paste0("[", round(icc2()$lbound, 3), ", ", round(icc2()$ubound, 3), "]")
      p_val <- round(icc2()$p.value, 3)
      
      paste(
        "ICC(C,1) =", icc_val, "\n",
        "Intervalo de Confiança 95%:", icc_ci, "\n",
        "Valor p =", p_val, ifelse(p_val > 0.05, "(não significativo)", "(significativo)"), "\n",
        "Tamanho amostral: n =", nrow(avaliacoes2())
      )
    })
}




# Run the application 
shinyApp(ui = ui, server = server)
