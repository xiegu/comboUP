library(dplyr)
library(shiny)
library(DT)
library(shinythemes)

# Load demo data
load('data.RData')




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
                                     selectizeInput('Combo', 'Combo set', choices = categories, selected = c('ref', 'air', 'wash', 'hood', 'gas'), multiple = TRUE,options = list(
                                       placeholder = 'Please select at least 2 categories')),
                                     checkboxInput('Brand', 'Brand? (Leader, Casarte)', FALSE)
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
                                     dataTableOutput('ComboOutput1'),
                                   br(),
                                     dataTableOutput('ComboOutput2')
                                   
                                   
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
  
  multiAndGrep <- function(patterns, x){
    n <- 0
    for(i in patterns){
      y <- grepl(i, x)
      n <- n + y
    }
    if(n > 0 & n == length(patterns)){
      return(1)
    }else(return(0))
  }
  tableFilter <- function(category = 'ref', preference, brand, tag){
    dt <- filter(haier_data, category == category) %>% mutate(match = sapply(name, function(x) multiAndGrep(tag, x))) %>% filter(match == 1) %>% select(-match)
    if(preference == 'sale'){
      dt <- arrange(dt, -comment_count)
    }else if(preference == 'like'){
      dt <- arrange(dt, -score)
    }else{dt <- dt}
    if(isTRUE(brand)){
      dt2 <- filter(dt, grepl(paste(sub_brand, collapse = '|'), name))
    }else{
      dt2 <- dt
    }
    if(nrow(dt2) == 0){
      dt <- dt
    }else{
      dt <- dt2
    }
    dt <- select(dt, name, p)
  }
  
  output$tagPicker <- renderUI({
    ui <- list(
      ref = selectizeInput('RefTag', 'Fridge tags', multiple = TRUE, choices = ref_hot_tag$tag, selected = head(ref_hot_tag$tag,3), options =list(maxItems = 5, placeholder = 'Please select at least 1 tag')),
      air = selectizeInput('AirTag', 'Aircon tags', multiple = TRUE, choices = air_hot_tag$tag, selected = head(air_hot_tag$tag,3), options =list(maxItems = 5, placeholder = 'Please select at least 1 tag')),
      wash= selectizeInput('WashTag', 'Wash tags', multiple = TRUE, choices = wash_hot_tag$tag, selected = head(wash_hot_tag$tag,3), options =list(maxItems = 5, placeholder = 'Please select at least 1 tag')),
      tv = selectizeInput('TVTag', 'TV tags', multiple = TRUE, choices = tv_hot_tag$tag, selected = head(tv_hot_tag$tag,3), options =list(maxItems = 5, placeholder = 'Please select at least 1 tag')),
      hood = selectizeInput('HoodTag', 'Hood tags', multiple = TRUE, choices = hood_hot_tag$tag, selected = head(hood_hot_tag$tag,3), options =list(maxItems = 5, placeholder = 'Please select at least 1 tag')),
      gas = selectizeInput('GasTag', 'Gas tags', multiple = TRUE, choices = gas_hot_tag$tag, selected = head(gas_hot_tag$tag,3), options =list(maxItems = 5, placeholder = 'Please select at least 1 tag')),
      dish = selectizeInput('DishTag', 'Dish tags', multiple = TRUE, choices = dish_hot_tag$tag, selected = head(dish_hot_tag$tag,3), options =list(maxItems = 5, placeholder = 'Please select at least 1 tag'))
      
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
  
  output$ComboOutput1 <- renderDataTable({
    
    if(is.null(reset$data)){
      return(NULL)
    }
    
    budget <- budget()
    range <- range()
    categories <- categories()
    brand <- brand()
    
    tableList <- list(
      ref = tableFilter('ref', 'sale', brand, reftag()),
      air = tableFilter('air', 'sale', brand, airtag()),
      wash = tableFilter('wash','sale', brand, washtag()),
      tv = tableFilter('tv','sale', brand, tvtag()),
      hood = tableFilter('hood','sale', brand, hoodtag()),
      gas = tableFilter('gas','sale', brand, gastag()),
      dish = tableFilter('dish','sale', brand, dishtag())
    )
    budgetChecker <- function(p){
      check <- ifelse(p > budget*(1+range) | p < budget*(1-range), FALSE, TRUE)
      check
    }
    
    tableSelector <- tableList[categories]
    
    air_no <- bed() + living()
    li <- list()
    for(i in categories){
          product <- tableSelector[[i]][1,]
          li[[i]]<-  product
    }
    tablei <- do.call(rbind, li)
    tablei <-  mutate(tablei, category = categories, total = ifelse(category=='air', air_no*p,p))
    budgetMatch <- budgetChecker(sum(tablei$total))
    if(budgetMatch){
      return(tablei)
    }else{
      l1 <- li
      for(i in categories){
        indexi <- which(categories ==i)
        if(indexi>1){
          n <- min(10, nrow(tableSelector[[indexi-1]]))# maximum 10 attempts only
          price2ndMax <- sort(head(tableSelector[[indexi-1]]$p, n), decreasing = TRUE)[2]
          indexP2ndMax <- which(head(tableSelector[[indexi-1]]$p, n) == price2ndMax)[1]
          l1[[indexi-1]] <- tableSelector[[indexi-1]][indexP2ndMax, ]
        }
        n <- min(10, nrow(tableSelector[[indexi]]))# maximum 10 attempts only
        for(j in 2:n){
          product <-tableSelector[[i]][j, ]
          l1[[i]] <- product
          table1 <- do.call(rbind, l1)
          table1 <- mutate(table1, category = categories, total = ifelse(category=='air', air_no*p,p))
          if(budgetChecker(sum(table1$total))){
            break
          }else{next}
        }
        if(budgetChecker(sum(table1$total))){
          break
        }else{next}
      }
      
      if(budgetChecker(sum(table1$total))){
        return(table1)
      }else{
        l2 <- l1
        for(i in categories){
          indexi <- which(categories ==i)
          ts <- arrange(tableSelector[[i]], -p)
          if(indexi>1){
            price2ndMax <- sort(tableSelector[[indexi-1]]$p, decreasing = TRUE)[2]
            indexP2ndMax <- which(tableSelector[[indexi-1]]$p == price2ndMax)[1]
            l2[[indexi-1]] <- tableSelector[[indexi-1]][indexP2ndMax, ]
          }
          n <- which(ts$p==l1[[i]]$p)[1]
          for(j in 1:n){
            product <-ts[j, ]
            l2[[i]] <- product
            table2 <- do.call(rbind, l2)
            table2 <- mutate(table2, category = categories, total = ifelse(category=='air', air_no*p,p))
            if(budgetChecker(sum(table2$total))){
              break
            }else{next}
          }
          if(budgetChecker(sum(table2$total))){
            break
          }else{next}
        }
        if(budgetChecker(sum(table2$total))){
          return(table2)
        }else{return(NULL)}
      }
    }
  }, caption = tags$caption(style = 'color: black', h1('Hot sales combo')), options = list(dom = 't'))
  
  output$ComboOutput2 <- renderDataTable({
    
    if(is.null(reset$data)){
      return(NULL)
    }
    
    budget <- budget()
    range <- range()
    categories <- categories()
    brand <- brand()
    
    tableList <- list(
      ref = tableFilter('ref', 'like', brand,  reftag()),
      air = tableFilter('air', 'like', brand, airtag()),
      wash = tableFilter('wash','like', brand, washtag()),
      tv = tableFilter('tv','like', brand, tvtag()),
      hood = tableFilter('hood','like', brand, hoodtag()),
      gas = tableFilter('gas','like', brand, gastag()),
      dish = tableFilter('dish','like', brand, dishtag())
    )
    budgetChecker <- function(p){
      check <- ifelse(p > budget*(1+range) | p < budget*(1-range), FALSE, TRUE)
      check
    }
    
    tableSelector <- tableList[categories]
    
    air_no <- bed() + living()
    li <- list()
    for(i in categories){
      product <- tableSelector[[i]][1,]
      li[[i]]<-  product
    }
    tablei <- do.call(rbind, li)
    tablei <-  mutate(tablei, category = categories, total = ifelse(category=='air', air_no*p,p))
    if(budgetChecker(sum(tablei$total))){
      return(tablei)
    }else{
      l1 <- li
      for(i in categories){
        indexi <- which(categories ==i)
        if(indexi>1){
          n <- min(10, nrow(tableSelector[[indexi-1]]))# maximum 10 attempts only
          price2ndMax <- sort(head(tableSelector[[indexi-1]]$p, n), decreasing = TRUE)[2]
          indexP2ndMax <- which(head(tableSelector[[indexi-1]]$p, n) == price2ndMax)[1]
          l1[[indexi-1]] <- tableSelector[[indexi-1]][indexP2ndMax, ]
        }
        n <- min(10, nrow(tableSelector[[indexi]]))# maximum 10 attempts only
        for(j in 2:n){
          product <-tableSelector[[i]][j, ]
          l1[[i]] <- product
          table1 <- do.call(rbind, l1)
          table1 <- mutate(table1, category = categories, total = ifelse(category=='air', air_no*p,p))
          if(budgetChecker(sum(table1$total))){
            break
          }else{next}
        }
        if(budgetChecker(sum(table1$total))){
          break
        }else{next}
      }
      
      if(budgetChecker(sum(table1$total))){
        return(table1)
      }else{
        l2 <- l1
        for(i in categories){
          indexi <- which(categories ==i)
          ts <- arrange(tableSelector[[i]], -p)
          if(indexi>1){
            price2ndMax <- sort(tableSelector[[indexi-1]]$p, decreasing = TRUE)[2]
            indexP2ndMax <- which(tableSelector[[indexi-1]]$p == price2ndMax)[1]
            l2[[indexi-1]] <- tableSelector[[indexi-1]][indexP2ndMax, ]
          }
          n <- which(ts$p==l1[[i]]$p)[1]
          for(j in 1:n){
            product <-ts[j, ]
            l2[[i]] <- product
            table2 <- do.call(rbind, l2)
            table2 <- mutate(table2, category = categories, total = ifelse(category=='air', air_no*p,p))
            if(budgetChecker(sum(table2$total))){
              break
            }else{next}
          }
          if(budgetChecker(sum(table2$total))){
            break
          }else{next}
        }
        if(budgetChecker(sum(table2$total))){
          return(table2)
        }else{return(NULL)}
      }
    }
  }, caption = tags$caption(style = 'color: black', h1('Customer favorites combo')), options = list(dom = 't'))
  # output$productTable <- renderUI({
  #   haier_data <- select(haier_data, id, name, comment_count, good_count, score, p)
  #  
  #   
  #   ui <- list(
  #     ref = wellPanel(datatable(tableFilter('ref', input$RefTag),rownames = FALSE)),
  #     air = wellPanel(datatable(tableFilter('air', input$AirTag),rownames = FALSE)),
  #     wash = wellPanel(datatable(tableFilter('wash', input$WashTag),rownames = FALSE)),
  #     tv = wellPanel(datatable(tableFilter('tv', input$TVTag),rownames = FALSE)),
  #     hood = wellPanel(datatable(tableFilter('hood', input$HoodTag),rownames = FALSE)),
  #     gas = wellPanel(datatable(tableFilter('gas', input$GasTag),rownames = FALSE)),
  #     dish = wellPanel(datatable(tableFilter('dish', input$DishTag),rownames = FALSE))
  #     
  #     )
  #   
  #   tagList(ui[input$Combo])
  # })
  
  
    
}

shinyApp(ui, server)