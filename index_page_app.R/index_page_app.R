
# Страница показывает содержимое папки с загрузками данных (./data/raw)

library('shiny')


# Переменные ===================================================================

# все сырые исходные данные ....................................................
sRawDataPath <- '../data/raw'

# интерфейс приложения =========================================================
ui <- fluidPage(

    # Заголовок
    titlePanel('Содержимое папки с выгрузками'),

    fluidRow(
        column(width = 4,
               htmlOutput('HeadRegionsList'),
               wellPanel(style = "overflow-y:scroll; max-height: 200px",
                         uiOutput('regionsList'))),
        column(width = 3,
               htmlOutput('HeadPeriodsList'),
               wellPanel(style = "height: 200px",
                         uiOutput('periodsList'))),
        column(width = 5,
               htmlOutput('HeadReadme'),
               wellPanel(style = "height: 200px; background: wheat",
                         htmlOutput('dirPath'),
                         verbatimTextOutput('folderReadme')))
        )
    )


# серверная часть ==============================================================
server <- function(input, output) {

    # статичный список регионов из файлов README в папках
    reg.list <- dir(sRawDataPath)
    reg.list <- sapply(reg.list, function(x){
        readLines(paste0(sRawDataPath, '/', x, '/README.txt'))[1]
    })
    reg.list <- gsub('Регион: ', '', reg.list)
    
    # реагирующий вектор с выбранной папкой
    readme.path <- reactive({
        readme.path <- dir(sRawDataPath)[grep(input$selectedRegion, reg.list)]
        period.flnm <- gsub('с ', 'from', input$selectedPeriod)
        period.flnm <- gsub(' по ', 'to', period.flnm)
        readme.path <- grep(period.flnm, readme.path, value = T)
    })
    
    output$regionsList <- renderUI({
        reg.list.rb <- sort(unique(gsub('Регион: ', '', reg.list)))
        radioButtons('selectedRegion',   # связанная переменная
                     label = NULL, reg.list.rb, 
                     selected = reg.list.rb[1])
    })
    
    output$periodsList <- renderUI({
        folders.list <- dir(sRawDataPath)[grep(input$selectedRegion, reg.list)]

        period.list <- sapply(folders.list, function(x){
            readLines(paste0(sRawDataPath, '/', x, '/README.txt'))[2]
        })

        period.list <- sort(unique(gsub('Период: ', '', period.list)))

        radioButtons('selectedPeriod',   # связанная переменная
                     label = NULL, period.list,
                     selected = period.list[1])
    })
    
    output$folderReadme <- renderText({
        paste0(readLines(paste0(sRawDataPath, '/', readme.path(), 
                                '/README.txt')),
               collapse = '\n')
    })
    
    output$dirPath <- renderText({
        n <- nrow(read.csv2(paste0(sRawDataPath, '/', readme.path(),
                                   '/xlms_names_list.csv')))
        paste0(readme.path(), '</br> xml-файлов: ', n)
    })
    
    output$HeadRegionsList <- renderText({
        paste0('<b>Регионы: </b>')
    })
    
    output$HeadPeriodsList <- renderText({
        paste0('<b>Периоды: </b>')
    })
    
    output$HeadReadme <- renderText({
        paste0('<b>Папка: </b>')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
