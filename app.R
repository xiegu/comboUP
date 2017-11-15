library(dplyr)
library(shiny)
library(DT)
library(shinythemes)
library(scales)

# Load demo data
load('data.RData')

source('util.R', local = TRUE)


#-----------------------------------------

ui <- navbarPage('comboUP', 
                 theme = shinytheme("journal"),
                 tabPanel('Tool',
                          fluidRow(
                            column(width = 2,
                                   wellPanel(
                                     sliderInput('Bed', 'Bedroom', min = 1, max =4 , value = 2),
                                     sliderInput('Living', 'Living room', min =1, max =4, value  = 1),
                                     sliderInput('Budget', 'Budget', min = 20000, max = 100000, step = 5000, value = 20000),
                                     sliderInput('Range', '+/-', min = 0.05, max = 0.2, step = 0.05, value = 0.1),
                                     selectizeInput('Combo', 'Combo set', choices = c('洗衣机' = 'wash', 
                                                                                      '空调' = 'air',
                                                                                      '冰箱' = 'ref', 
                                                                                      '洗碗机' = 'dish', 
                                                                                      '油烟机' = 'hood',
                                                                                      '燃气灶' = 'gas',
                                                                                      '电视' = 'tv'), selected = c('ref', 'air', 'wash', 'hood', 'gas'), multiple = TRUE,options = list(
                                                                                        placeholder = 'Please select at least 2 categories')),
                                     uiOutput('FloorAir'),
                                     checkboxInput('Brand', 'Brand? (Casarte)', FALSE)
                                   )
                                   
                            ),
                            column(width = 3,
                                   wellPanel(style = 'background:#dddddd',
                                             uiOutput('tagPicker')
                                   ),
                                   actionButton("Submit" ,'Submit', class = 'btn-primary', icon = icon('upload'), width = '49%'),
                                   actionButton('Reset', 'Reset', class = 'btn-info', icon = icon('refresh'),width = '49%')
                                   
                            ),
                            column(width = 7,
                                   uiOutput('ComboOutput1'),
                                   br(),
                                   uiOutput('ComboOutput2'),
                                   br()
                                   
                                   
                            )
                            
                            
                          )
                          
                          # hr(),
                          # fluidRow(
                          #     uiOutput('productTable')
                          #   
                          # )
                 )
)


server <- function(input, output, session){
  
  reset <- reactiveValues(data = NULL)
  
  observeEvent(input$Submit, {
    reset$data <- 1
  })
  
  observeEvent(input$Reset, {
    reset$data <- NULL
  })
  
  output$FloorAir <- renderUI({
    if(!'air' %in% input$Combo){
      return()
    }else{
      sliderInput('NoFloorAir', 'No of floor aircon', min = 0, max = input$Living + input$Bed, step = 1, value = input$Living)
    }
  })
  
  
  output$tagPicker <- renderUI({
    ui <- list(
      ref = selectizeInput('RefTag', 'Fridge tags', multiple = TRUE, choices = ref_hot_tag$tag, selected = head(ref_hot_tag$tag,2), options =list(maxItems = 5, placeholder = 'Please select at least 1 tag')),
      air = selectizeInput('AirTag', 'Aircon tags', multiple = TRUE, choices = air_hot_tag$tag, selected = head(air_hot_tag$tag,2), options =list(maxItems = 5, placeholder = 'Please select at least 1 tag')),
      wash= selectizeInput('WashTag', 'Wash tags', multiple = TRUE, choices = wash_hot_tag$tag, selected = head(wash_hot_tag$tag,2), options =list(maxItems = 5, placeholder = 'Please select at least 1 tag')),
      tv = selectizeInput('TVTag', 'TV tags', multiple = TRUE, choices = tv_hot_tag$tag, selected = head(tv_hot_tag$tag,2), options =list(maxItems = 5, placeholder = 'Please select at least 1 tag')),
      hood = selectizeInput('HoodTag', 'Hood tags', multiple = TRUE, choices = hood_hot_tag$tag, selected = head(hood_hot_tag$tag,2), options =list(maxItems = 5, placeholder = 'Please select at least 1 tag')),
      gas = selectizeInput('GasTag', 'Gas tags', multiple = TRUE, choices = gas_hot_tag$tag, selected = head(gas_hot_tag$tag,2), options =list(maxItems = 5, placeholder = 'Please select at least 1 tag')),
      dish = selectizeInput('DishTag', 'Dish tags', multiple = TRUE, choices = dish_hot_tag$tag, selected = head(dish_hot_tag$tag,2), options =list(maxItems = 5, placeholder = 'Please select at least 1 tag'))
      
    )
    tagList(ui[input$Combo]
    )
  })
  
  bed <- eventReactive(input$Submit, {
    input$Bed
  })
  
  living <- eventReactive(input$Submit, {
    input$Living
  })
  
  budget <- eventReactive(input$Submit, {
    input$Budget
  })
  
  range <- eventReactive(input$Submit, {
    input$Range
  })
  
  categories <- eventReactive(input$Submit, {
    input$Combo
  })
  
  brand <- eventReactive(input$Submit,{
    input$Brand
  })
  
  floor <- eventReactive(input$Submit, {
    input$NoFloorAir
  })
  
  reftag <- eventReactive(input$Submit,{
    input$RefTag
  })
  
  airtag <- eventReactive(input$Submit,{
    input$AirTag
  })
  
  washtag <- eventReactive(input$Submit,{
    input$WashTag
  })
  
  tvtag <- eventReactive(input$Submit,{
    input$TVTag
  })
  
  hoodtag <- eventReactive(input$Submit,{
    input$HoodTag
  })
  
  gastag <- eventReactive(input$Submit,{
    input$GasTag
  })
  
  dishtag <- eventReactive(input$Submit,{
    input$DishTag
  })
  
  output$ComboOutput1 <- renderUI({
    
    if(is.null(reset$data)){
      return(NULL)
    }
    
    bed <- bed()
    living <- living()
    floor <- floor()
    budget <- budget()
    range <- range()
    categories <- categories()
    brand <- brand()
    room <- bed + living
    
    tableList <- list(
      ref = tableFilter('ref', 'sale', brand, reftag()),
      air = tableFilter('air', 'sale', brand, airtag(), floor, room),
      wash = tableFilter('wash','sale', brand, washtag()),
      tv = tableFilter('tv','sale', brand, tvtag()),
      hood = tableFilter('hood','sale', brand, hoodtag()),
      gas = tableFilter('gas','sale', brand, gastag()),
      dish = tableFilter('dish','sale', brand, dishtag())
    )
    
    if('air' %in% categories){
      if(floor == 0){
        tableSelector <- tableList[categories]
        tableSelector[['wall']] <- tableSelector[['air']]
        tableSelector[['air']] <- NULL
      }else if(floor == room){
        tableSelector <- tableList[categories]
        tableSelector[['floor']] <- tableSelector[['air']]
        tableSelector[['air']] <- NULL
      }else{
        tableSelector <- tableList[categories]
        tableSelector[['floor']] <- tableSelector[['air']][['floor']]
        tableSelector[['wall']] <- tableSelector[['air']][['wall']]
        tableSelector[['air']] <- NULL
      }
    }else{
      tableSelector <- tableList[categories]
    }
    
    categories <- names(tableSelector)
    li <- list()
    for(i in categories){
      product <- tableSelector[[i]][1,]
      li[[i]]<-  product
    }
    tablei <- do.call(rbind, li)
    tablei <-  mutate(tablei, category = categories, total = ifelse(category=='floor', floor*price, ifelse(category == 'wall', (room - floor)*price, price)))
    
    combo_result1 <- comboModeler(categories, tableSelector, li, tablei, room, floor, budget, range)
    table1 <- combo_result1$table
    l1 <- combo_result1$l
    if(is.null(combo_result1)){
      combo_result2 <- NULL
    }else{
      combo_result2 <- comboModelerNext(categories, tableSelector, l1, room, floor, budget, range)
    }
    table2 <- combo_result2$table
    l2 <- combo_result2$l
    tableSelector2 <- combo_result2$tableSelector
    if(is.null(combo_result2)){
      combo_result3 <- NULL
    }else{
      combo_result3 <- comboModelerNext(categories, tableSelector2, l2, room, floor, budget, range)
    }
    table3 <- combo_result3$table
    ui <- list(combo1 = datatable(table1, caption = tags$caption(style = 'color: black', h1('Hot sales combo-1'), h2(style = 'text-align: right;', paste('Price', prettyNum(sum(table1$total), big.mark = ','), sep = ': '))), options = list(dom = 't')),
             combo2 = datatable(table2, caption = tags$caption(style = 'color: black', h1('Hot sales combo-2'), h2(style = 'text-align: right;', paste('Price', prettyNum(sum(table2$total), big.mark = ','), sep = ': '))), options = list(dom = 't')),
           combo3 = datatable(table3, caption = tags$caption(style = 'color: black', h1('Hot sales combo-3'), h2(style = 'text-align: right;', paste('Price', prettyNum(sum(table3$total), big.mark = ','), sep = ': '))), options = list(dom = 't'))
           )
    #ui <- list(combo1 = datatable(do.call(rbind, tableSelector2)))
    tagList(ui[c('combo1', 'combo2', 'combo3')])
    })
  
  output$ComboOutput2 <- renderUI({
    
    if(is.null(reset$data)){
      return(NULL)
    }
    
    bed <- bed()
    living <- living()
    floor <- floor()
    budget <- budget()
    range <- range()
    categories <- categories()
    brand <- brand()
    room <- bed + living
    
    tableList <- list(
      ref = tableFilter('ref', 'like', brand, reftag()),
      air = tableFilter('air', 'like', brand, airtag(), floor, room),
      wash = tableFilter('wash','like', brand, washtag()),
      tv = tableFilter('tv','like', brand, tvtag()),
      hood = tableFilter('hood','like', brand, hoodtag()),
      gas = tableFilter('gas','like', brand, gastag()),
      dish = tableFilter('dish','likes', brand, dishtag())
    )
    
    if('air' %in% categories){
      if(floor == 0){
        tableSelector <- tableList[categories]
        tableSelector[['wall']] <- tableSelector[['air']]
        tableSelector[['air']] <- NULL
      }else if(floor == room){
        tableSelector <- tableList[categories]
        tableSelector[['floor']] <- tableSelector[['air']]
        tableSelector[['air']] <- NULL
      }else{
        tableSelector <- tableList[categories]
        tableSelector[['floor']] <- tableSelector[['air']][['floor']]
        tableSelector[['wall']] <- tableSelector[['air']][['wall']]
        tableSelector[['air']] <- NULL
      }
    }else{
      tableSelector <- tableList[categories]
    }
    
    categories <- names(tableSelector)
    li <- list()
    for(i in categories){
      product <- tableSelector[[i]][1,]
      li[[i]]<-  product
    }
    tablei <- do.call(rbind, li)
    tablei <-  mutate(tablei, category = categories, total = ifelse(category=='floor', floor*price, ifelse(category == 'wall', (room - floor)*price, price)))
    
    combo_result1 <- comboModeler(categories, tableSelector, li, tablei, room, floor, budget, range)
    table1 <- combo_result1$table
    l1 <- combo_result1$l
    if(is.null(combo_result1)){
      combo_result2 <- NULL
    }else{
      combo_result2 <- comboModelerNext(categories, tableSelector, l1, room, floor, budget, range)
    }
    table2 <- combo_result2$table
    l2 <- combo_result2$l
    tableSelector2 <- combo_result2$tableSelector
    if(is.null(combo_result2)){
      combo_result3 <- NULL
    }else{
      combo_result3 <- comboModelerNext(categories, tableSelector2, l2, room, floor, budget, range)
    }
    table3 <- combo_result3$table
    ui <- list(combo1 = datatable(table1, caption = tags$caption(style = 'color: black', h1('Customer favorites combo-1'), h2(style = 'text-align: right;', paste('Price', prettyNum(sum(table1$total), big.mark = ','), sep = ': '))), options = list(dom = 't')),
               combo2 = datatable(table2, caption = tags$caption(style = 'color: black', h1('Customer favorites combo-2'), h2(style = 'text-align: right;', paste('Price', prettyNum(sum(table2$total), big.mark = ','), sep = ': '))), options = list(dom = 't')),
               combo3 = datatable(table3, caption = tags$caption(style = 'color: black', h1('Customer favorites combo-3'), h2(style = 'text-align: right;', paste('Price', prettyNum(sum(table3$total), big.mark = ','), sep = ': '))), options = list(dom = 't'))
    )
    #ui <- list(combo1 = datatable(do.call(rbind, tableSelector2)))
    tagList(ui[c('combo1', 'combo2', 'combo3')])
  })
  
 
}

shinyApp(ui, server)