library(shinydashboard)
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyverse)
library(readxl)
library(dashboardthemes)
library(shinydashboardPlus)

# Set working directory
setwd("C:\\Users\\meina\\Downloads\\FP")


# Load dataset
df <- read_csv("cleaned_stroke_data.csv")


# UI
headerItem <- dashboardHeader(title = "S",
                              titleWidth = 200, disable = FALSE)

sidebarItem <- dashboardSidebar(
  width = 200,
  sidebarMenu(
    menuItem("Home", tabName = "Home", icon = icon("house")),
    menuItem("Pendahuluan", tabName = "Pendahuluan", icon = icon("clipboard")),
    menuItem("Dataset", tabName = "Dataset", icon = icon("sack-dollar")),
    menuItem("Visualisasi", tabName = "Visualisasi", icon = icon("mountain-sun")),
    menuItem("Authors", tabName = "penulis", icon = icon("user"))
  )
)

bodyItem <- dashboardBody(
  tags$style(HTML("
    .box.box-solid.box-primary>.box-header {
      color:#fff;
      background:#c7c6c1;
    }
    .box.box-solid.box-primary {
      border-bottom-color:#fff;
      border-left-color:#fff;
      border-right-color:#fff;
      border-top-color:#fff;
    }
    .box.box-solid.box-success>.box-header {
      color:#fff;
      background:#CA4E4C;
    }
    .box.box-solid.box-success {
      border-bottom-color:#fff;
      border-left-color:#fff;
      border-right-color:#fff;
      border-top-color:#fff;
    }
    .main-header .logo {
      font-family: helvetica, serif, Times, Times New Roman;
      font-weight: bold;
      font-size: 24px;
    }
    .myClass {
      font-size: 20px;
      line-height: 50px;
      text-align: left;
      font-family: helvetica, serif, Times, Times New Roman;
      padding: 0 15px;
      overflow: hidden;
      color: black;
    }
    .carousel-img {
      display: block;
      margin-left: auto;
      margin-right: auto;
      max-height: 700px;
      width: auto;
    }
  ")),
  tags$script(HTML('
    $(document).ready(function() {
      $("header").find("nav").append(\'<span class="myClass"> Dashboard Visualisasi Dataset Prediksi Stroke </span>\');
    })
  ')),
  shinyDashboardThemes("poor_mans_flatly"),
  tabItems(
    tabItem(tabName = "Home",
            carousel(width = 12,
                     id = "mycarousel",
                     carouselItem(
                       tags$img(src = "https://p2ptm.kemkes.go.id/uploads/TmQwU05BQS9YYlJpanB5VnNtRldFUT09/4_Juli_2018_07.png", class = "carousel-img")
                     ),
                     carouselItem(
                       tags$img(src = "https://p2ptm.kemkes.go.id/uploads//TmQwU05BQS9YYlJpanB5VnNtRldFUT09/5_Juli_2018_03.png", class = "carousel-img")
                     ),
                     carouselItem(
                       tags$img(src = "https://p2ptm.kemkes.go.id/uploads//TmQwU05BQS9YYlJpanB5VnNtRldFUT09/19_Juli_06.png", class = "carousel-img")
                     ),
                     carouselItem(
                       tags$img(src = "https://p2ptm.kemkes.go.id/uploads//TmQwU05BQS9YYlJpanB5VnNtRldFUT09/1_November_01.png", class = "carousel-img")
                     ),
                     carouselItem(
                       tags$img(src = "https://p2ptm.kemkes.go.id/uploads//TmQwU05BQS9YYlJpanB5VnNtRldFUT09/1_November_02.png", class = "carousel-img")
                     )
            )),
    tabItem(tabName = "Pendahuluan",
            tabBox(id = "t1", width = 12,
                   fluidRow(
                     box(title = h1(strong("Penyakit Stroke"),
                                    style = "text-align: center"),
                         width = 12,
                         height = 150
                     )),
                   fluidRow(
                     box(solidHeader = TRUE,
                         style = "text-align:justify;",
                         width = 6,
                         p("Stroke merupakan salah satu penyakit yang menyebabkan kematian dan kecacatan terbesar di dunia. Menurut data dari Organisasi Kesehatan Dunia (WHO), stroke menyumbang sekitar 11% dari total kematian global setiap tahun, menjadikannya penyebab kematian kedua setelah penyakit jantung iskemik (World Health Organization, n.d.). Di Indonesia, prevalensi stroke terus meningkat, yang sebagian besar dipicu oleh faktor gaya hidup dan perubahan demografi, seperti peningkatan angka harapan hidup dan urbanisasi (Ministry of Health of the Republic of Indonesia, 2021). Stroke adalah kondisi medis yang memerlukan penanganan cepat dan tepat untuk meminimalkan kerusakan pada otak dan meningkatkan peluang pemulihan pasien."),
                         p("Stroke terjadi ketika suplai darah ke bagian otak terganggu atau berkurang, yang dapat disebabkan oleh penyumbatan (stroke iskemik) atau pecahnya pembuluh darah (stroke hemoragik). Hal ini mengakibatkan otak kekurangan oksigen dan nutrisi, sehingga sel-sel otak mulai mati dalam hitungan menit (World Health Organization, n.d.). Identifikasi dini terhadap individu yang berisiko tinggi mengalami stroke sangat penting untuk pencegahan dan pengelolaan yang lebih baik. Beberapa faktor risiko utama stroke meliputi hipertensi, diabetes, kolesterol tinggi, merokok, obesitas, dan riwayat keluarga stroke (Ministry of Health of the Republic of Indonesia, 2021).")),
                     box(imageOutput("stroke"),
                         style = "text-align:center;",
                         width = 6)
                   )
            )
    ),
    tabItem(tabName = "Dataset",
            tabBox(id = "t2", width = 12,
                   tabPanel("Keterangan", icon=icon("address-card"),
                            fluidRow(
                              box(width=12,
                                  p("Keterangan :"),
                                  p("1) gender: Male, Female or Other"),
                                  p("2) age: age of the patient"),
                                  p("3) hypertension: 0 if the patient doesn't have hypertension, 1 if the patient has hypertension"),
                                  p("4) heart_disease: 0 if the patient doesn't have any heart diseases, 1 if the patient has a heart disease"),
                                  p("5) ever_married: No or Yes"),
                                  p("6) work_type: children, Govt_jov, Never_worked, Private or Self-employed"),
                                  p("7) Residence_type: Rural or Urban"),
                                  p("8) avg_glucose_level: average glucose level in blood"),
                                  p("9) bmi: body mass index"),
                                  p("10) smoking_status: formerly smoked, never smoked, smokes or Unknown*"),
                                  p("11) stroke: 1 if the patient had a stroke or 0 if not"),
                                  p("*Note: Unknown in smoking_status means that the information is unavailable for this patient")
                              )
                            )),
                   tabPanel("Data", icon = icon("address-card"), dataTableOutput("dfb")),
                   tabPanel("Struktur", icon = icon("address-card"), verbatimTextOutput("structure")),
                   tabPanel("Summary", icon = icon("address-card"), verbatimTextOutput("summary"))
            )
    ),
    tabItem(tabName = "Visualisasi",
            tabBox(id = "t3", width = 12,
                   tabPanel("Prediktor", 
                            fluidRow(
                              box(width = 12, 
                                  selectInput("plot_type", "Choose Plot Type:",
                                              choices = c("Gender" = "gender_plot",
                                                          "Age Distribution" = "age_plot",
                                                          "Hypertension" = "hypertension_plot",
                                                          "Heart Disease" = "heart_disease_plot",
                                                          "Ever Married" = "ever_married_plot",
                                                          "Work Type" = "work_type_plot",
                                                          "Residence Type" = "residence_type_plot",
                                                          "Average Glucose Level" = "glucose_level_plot",
                                                          "BMI" = "bmi_plot",
                                                          "Smoking Status" = "smoking_status_plot")),
                                  selectInput("filter", "Stroke or Non-Stroke:",
                                              choices = c("Stroke" = "1",
                                                          "Non-stroke" = "0")),
                                  fluidRow(
                                    box(title = "Visualisasi", status = "primary", solidHeader = FALSE,
                                        collapsible = TRUE, plotOutput("selected_plot"), width = 12)
                                  )
                              )
                            )
                   ),
                   tabPanel("Respon",
                            fluidRow(
                              column(6,
                                     sliderInput("range", "Rentang Usia:",
                                                 min = 1, max = max(df$age),
                                                 value = c(50,70))
                              ),
                              column(6,
                                     selectInput("bmiCategory", "Kategori BMI:",
                                                 choices = c("Underweight", "Normal Weight", "Overweight", "Obesity"),
                                                 selected = "Normal Weight")
                              )
                            ),
                            fluidRow(
                              box(title = "Visualisasi", status = "primary", solidHeader = FALSE,
                                  collapsible = TRUE, plotOutput("respon"), width = 12),
                            )
                   ),
                   tabPanel("Hubungan",
                            fluidRow(
                              box(width = 12, 
                                  selectInput("var1", "Select First Variable:", choices = names(df)),
                                  selectInput("var2", "Select Second Variable:", choices = names(df)),
                                  fluidRow(
                                    box(title = "Visualisasi Heatmap", status = "primary", solidHeader = FALSE,
                                        collapsible = TRUE, plotOutput("heatmap_plot"), width = 12)
                                  )
                              )
                            )
                   )
            )
    ),
    tabItem(tabName = "penulis",
            fluidRow(
              box(width = 6,
                  status = "success",
                  title = "Anggota 1",
                  solidHeader = TRUE,
                  div(imageOutput("meisy"), style = "text-align: center;",
                      style = "margin-bottom:-180px;"),
                  div(strong("Meisy Nathania Yogianty"), style = "text-align: center;"),
                  div(strong("5003211088"), style = "text-align: center;")
              ),
              box(width = 6,
                  status = "success",
                  title = "Anggota 2",
                  solidHeader = TRUE,
                  div(imageOutput("elsya"), style = "text-align: center;",
                      style = "margin-bottom:-180px;"),
                  div(strong("Elsyafia Yasmin Putri Zamil"), style = "text-align: center;"),
                  div(strong("5003211027"), style = "text-align: center;")
              )
            ) 
    )
    
  )
)

ui <- dashboardPage(
  header = headerItem,
  sidebar = sidebarItem,
  body = bodyItem
)


# server
server <- function(input, output, session) {
  output$structure <- renderPrint({
    str(df)
  })
  output$summary <- renderPrint({
    summary(df)
  })
  output$dfb <- renderDataTable({
    df
  }, extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = list('copy', 'pdf', 'excel', 'csv', 'print')))
  
  output$stroke <- renderImage({
    list(src = "www/stroke.jpeg", height = 350, width = 400)
  }, deleteFile = FALSE)
  
  output$elsya <- renderImage({
    list(src = "www/elsya.jpg", deleteFile = FALSE, height = 200, width = 150)
  }, deleteFile = FALSE)
  
  output$meisy <- renderImage({
    list(src = "www/meisy.jpg", deleteFile = FALSE, height = 200, width = 150)
  }, deleteFile = FALSE)
  
  output$selected_plot <- renderPlot({
    plot_choice <- switch(input$plot_type,
                          "gender_plot" = {
                            gender_summary <- df %>%
                              filter(stroke == as.numeric(input$filter)) %>%
                              count(gender) %>%
                              mutate(percent = n / sum(n) * 100,
                                     label = paste0(round(percent), "%"))
                            ggplot(gender_summary, aes(x = "", y = percent, fill = gender)) +
                              geom_bar(width = 1, stat = "identity") +
                              geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
                              coord_polar(theta = "y") +
                              labs(title = "Distribution of Gender", fill = "Gender") +
                              theme_void()
                          },
                          "age_plot" = {
                            ggplot(df %>% filter(stroke == as.numeric(input$filter)), aes(x = age)) +
                              geom_histogram(binwidth = 5, fill = "skyblue", color = "black", aes(y = ..density..)) +
                              geom_density(alpha = 0.5, color = "red") +
                              labs(title = "Distribution of Age", x = "Age", y = "Density")
                          },
                          "hypertension_plot" = {
                            df$hypertension <- factor(df$hypertension, levels = c(0, 1),
                                                      labels = c("No", "Yes"))
                            hypertension_summary <- df %>%
                              filter(stroke == as.numeric(input$filter)) %>%
                              count(hypertension) %>%
                              mutate(percent = n / sum(n) * 100,
                                     label = paste0(round(percent), "%"))
                            ggplot(hypertension_summary, aes(x = "", y = percent, fill = hypertension)) +
                              geom_bar(width = 1, stat = "identity") +
                              geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
                              coord_polar(theta = "y") +
                              labs(title = "Distribution of Hypertension", fill = "Hypertension") +
                              theme_void()
                          },
                          "heart_disease_plot" = {
                            df$heart_disease <- factor(df$heart_disease, levels = c(0, 1),
                                                       labels = c("No", "Yes"))
                            heart_disease_summary <- df %>%
                              filter(stroke == as.numeric(input$filter)) %>%
                              count(heart_disease) %>%
                              mutate(percent = n / sum(n) * 100,
                                     label = paste0(round(percent), "%"))
                            ggplot(heart_disease_summary, aes(x = "", y = percent, fill = heart_disease)) +
                              geom_bar(width = 1, stat = "identity") +
                              geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
                              coord_polar(theta = "y") +
                              labs(title = "Distribution of Heart Disease", fill = "Heart Disease") +
                              theme_void()
                          },
                          "ever_married_plot" = {
                            ever_married_summary <- df %>%
                              filter(stroke == as.numeric(input$filter)) %>%
                              count(ever_married) %>%
                              mutate(percent = n / sum(n) * 100,
                                     label = paste0(round(percent), "%"))
                            ggplot(ever_married_summary, aes(x = "", y = percent, fill = ever_married)) +
                              geom_bar(width = 1, stat = "identity") +
                              geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
                              coord_polar(theta = "y") +
                              labs(title = "Distribution of Ever Married", fill = "Ever Married") +
                              theme_void()
                          },
                          "work_type_plot" = {
                            count_work_type <- df %>%
                              filter(stroke == as.numeric(input$filter)) %>%
                              count(work_type) %>%
                              arrange(desc(n))
                            ggplot(count_work_type, aes(x = reorder(work_type, -n), y = n)) +
                              geom_bar(stat = "identity", fill = "skyblue", color = "black") +
                              labs(title = "Distribution of Work Type", x = "Work Type", y = "Count") +
                              theme(axis.text.x = element_text(angle = 45, hjust = 1))
                          },
                          "residence_type_plot" = {
                            Residence_type_summary <- df %>%
                              filter(stroke == as.numeric(input$filter)) %>%
                              count(Residence_type) %>%
                              mutate(percent = n / sum(n) * 100,
                                     label = paste0(round(percent), "%"))
                            ggplot(Residence_type_summary, aes(x = "", y = percent, fill = Residence_type)) +
                              geom_bar(width = 1, stat = "identity") +
                              geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
                              coord_polar(theta = "y") +
                              labs(title = "Distribution of Residence Type", fill = "Residence Type") +
                              theme_void()
                          },
                          "glucose_level_plot" = {
                            ggplot(df %>% filter(stroke == as.numeric(input$filter)), aes(x = avg_glucose_level)) +
                              geom_histogram(binwidth = 5, fill = "skyblue", color = "black", aes(y = ..density..)) +
                              geom_density(alpha = 0.5, color = "red") +
                              labs(title = "Distribution of Average Glucose Level", x = "Average Glucose Level", y = "Density")
                          },
                          "bmi_plot" = {
                            ggplot(df %>% filter(stroke == as.numeric(input$filter)), aes(x = bmi)) +
                              geom_histogram(binwidth = 5, fill = "skyblue", color = "black", aes(y = ..density..)) +
                              geom_density(alpha = 0.5, color = "red") +
                              labs(title = "Distribution of Body Mass Index", x = "Body Mass Index", y = "Density")
                          },
                          "smoking_status_plot" = {
                            count_smoking_status <- df %>%
                              filter(stroke == as.numeric(input$filter)) %>%
                              count(smoking_status) %>%
                              arrange(desc(n))
                            ggplot(count_smoking_status, aes(x = reorder(smoking_status, -n), y = n)) +
                              geom_bar(stat = "identity", fill = "skyblue", color = "black") +
                              labs(title = "Distribution of Smoking Status", x = "Smoking Status", y = "Count") +
                              theme(axis.text.x = element_text(angle = 45, hjust = 1))
                          }
    )
    
    print(plot_choice)  # Debugging purposes, remove in final app
    
  })
  
  output$respon <- renderPlot({
    df$stroke <- factor(df$stroke, levels = c(0, 1),
                        labels = c("No", "Yes"))
    stroke_summary <- df %>%
      count(stroke) %>%
      mutate(percent = n / sum(n) * 100,
             label = paste0(round(percent), "%"))
    ggplot(stroke_summary, aes(x = "", y = percent, fill = stroke)) +
      geom_bar(width = 1, stat = "identity") +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y") +
      labs(title = "Distribution of Stroke", fill = "Stroke") +
      theme_void()
  })
  
  output$heatmap_plot <- renderPlot({
    df$hypertension <- factor(df$hypertension, levels = c(0, 1),
                              labels = c("No", "Yes"))
    df$heart_disease <- factor(df$heart_disease, levels = c(0, 1),
                               labels = c("No", "Yes"))
    df$stroke <- factor(df$stroke, levels = c(0, 1),
                        labels = c("No", "Yes"))
    req(input$var1, input$var2)  # Ensure variables are selected
    
    if (input$var1 != input$var2) {
      var1 <- sym(input$var1)
      var2 <- sym(input$var2)
      
      heatmap_data <- df %>%
        select(!!var1, !!var2) %>%
        drop_na()
      
      ggplot(heatmap_data, aes_string(x = input$var1, y = input$var2)) +
        geom_bin2d() +
        scale_fill_gradient(low = "white", high = "blue") +
        labs(title = paste("Heatmap of", input$var1, "and", input$var2),
             x = input$var1, y = input$var2)
    } else {
      ggplot() +
        geom_text(aes(0.5, 0.5, label = "Please select two different variables."),
                  size = 8, color = "red") +
        theme_void()
    }
  })
  
  filtered_by_age <- reactive({
    df %>%
      filter(between(age, input$range[1], input$range[2]))
  })
  
  data_filtered <- reactive({
    filtered <- filtered_by_age()
    filtered %>%
      mutate(bmiCategory = case_when(
        bmi < 18.5 ~ "Underweight",
        bmi >= 18.5 & bmi <= 24.9 ~ "Normal Weight",
        bmi >= 25.0 & bmi <= 29.9 ~ "Overweight",
        bmi > 30.0 ~ "Obesity"
      ))
  })
  
  output$respon <- renderPlot({
    df_filtered <- data_filtered()
    df_filtered$stroke <- factor(df_filtered$stroke, levels = c(0, 1),
                                 labels = c("No", "Yes"))
    stroke_summary <- df_filtered %>%
      filter(bmiCategory == input$bmiCategory) %>%
      count(stroke) %>%
      mutate(percent = n / sum(n) * 100,
             label = paste0(round(percent), "%"))
    
    ggplot(stroke_summary, aes(x = "", y = percent, fill = stroke)) +
      geom_bar(width = 1, stat = "identity") +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y") +
      labs(title = paste("Distribusi Stroke untuk Kategori BMI:", input$bmiCategory), fill = "Stroke") +
      theme_void()
  })
}


# shinyapp
shinyApp(ui, server)