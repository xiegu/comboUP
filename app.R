library(dplyr)
library(shiny)
library(DT)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(scales)
library(markdown)

# Load demo data
load('data.RData')

source('util.R', local = TRUE)


#-----------------------------------------

ui <- navbarPage(
  span(style = 'font-weight:normal;font-size:30px', 'NICE'),
  position = 'fixed-top',
  theme = shinytheme("flatly"),
  windowTitle = 'NICE - Next-generation Interactive Context Engine',
  tabPanel(
    span(style = 'font-weight:normal;font-size:20px','套餐选择'),
    tags$head(includeCSS('www/style.css'),
              includeCSS('www/hover.css')),
    
    fluidRow(
      column(
        width = 3,
        
        wellPanel(style = 'opacity: 0.85',
          sliderInput(
            'Bed',
            '卧室',
            min = 1,
            max = 4 ,
            value = 2
          ),
          sliderInput(
            'Living',
            '客厅',
            min = 1,
            max = 4,
            value  = 1
          ),
          sliderInput(
            'Budget',
            '预算',
            min = 20000,
            max = 100000,
            step = 5000,
            value = 20000
          ),
          sliderInput(
            'Range',
            '预算调整幅度+/-',
            min = 0.05,
            max = 0.2,
            step = 0.05,
            value = 0.1
          ),
          selectizeInput(
            'Combo',
            '家电品类',
            choices = c(
              '洗衣机' = 'wash',
              '空调' = 'air',
              '冰箱' = 'ref',
              '洗碗机' = 'dish',
              '油烟机' = 'hood',
              '燃气灶' = 'gas',
              '电视' = 'tv'
            ),
            selected = c('ref', 'air', 'wash', 'hood', 'gas'),
            multiple = TRUE,
            options = list(placeholder = '请至少选择两个家电品类')
          ),
          uiOutput('FloorAir'),
          checkboxInput('品牌偏好', '卡萨帝', FALSE)
        ),
        actionButton(
          "Submit" ,
          '提交',
          class = 'btn-primary',
          icon = icon('upload'),
          width = '49%'
        ),
        actionButton(
          'Reset',
          '清除',
          class = 'btn-secondary',
          icon = icon('refresh'),
          width = '49%'
        )
        
      ),
      column(
        width = 3,
        wellPanel(style = 'opacity: 0.85',
          sliderInput(
            'People',
            '住户人数',
            min = 1,
            max = 10,
            value = 3
          ),
          checkboxInput('Old', '是否有老人', FALSE),
          radioButtons(
            'Child',
            '是否有小孩',
            choices = list(
              '无' = '无',
              '3岁以下' = '3岁以下',
              '3岁以上' = '3岁以上'
            ),
            FALSE
          ),
          textInput('Address', '地址', value = '', placeholder = '请填入住宅地址'),
          checkboxInput('PM', '根据地址是否选择自清洁/净化pm2.5空调？', FALSE),
          textInput('Phone', '联系电话', value = '', placeholder = '请填入联系电话')
        ),
        wellPanel(style = 'opacity:0.85',
                  uiOutput('tagPicker'))
        
      ),
      column(
        width = 6,
        # You can open the modal server-side, you have to put this in the ui :
        tags$script("Shiny.addCustomMessageHandler('launch-modal', function(d) {$('#' + d).modal().focus();})"),
        tags$script("Shiny.addCustomMessageHandler('remove-modal', function(d) {$('#' + d).modal('hide');})"),
        
        # Code for creating a modal
        tags$div(
          id = "pb-modal",
          class="modal fade", tabindex="-1", `data-backdrop`="static", `data-keyboard`="false",
          tags$div(
            class="modal-dialog",
            tags$div(
              class = "modal-content",
              tags$div(class="modal-header", tags$h4(class="modal-title", "推荐模型计算中")),
              tags$div(
                class="modal-body",
                shinyWidgets::progressBar(id = "pb", value = 0, display_pct = TRUE)
              ),
              tags$div(class="modal-footer", tags$button(type="button", class="btn btn-default", `data-dismiss`="modal", "取消"))
            )
          )
        ),
        useShinyjs(),
        actionButton('Logic', '套餐选择逻辑', class = 'btn-info'),
        wellPanel(style = 'opacity:0.6;background:black;color:white',
                  id = 'LogicPanel',
                  p('根据住房户型、用户预算和所需家电品类，初步筛选目标家电产品'),
                  p('根据用户输入基本信息，客制化精确筛选目标家电产品：'),
                  p('地址：', verbatimTextOutput('AddressOut')),
                  p('住户成员构成：', verbatimTextOutput('Member')),
                  p('联系电话：', verbatimTextOutput('PhoneOut'))
        ),
        actionButton('bought', '购买记录', class = 'btn-success'),
        hr(),
        tabsetPanel(
          tabPanel(
            h2('热卖产品'),
            br(),
            actionButton('sale1', '套餐一', class = 'hvr-fade-1'),
            actionButton('sale2', '套餐二', class = 'hvr-fade-2'),
            actionButton('sale3', '套餐三', class = 'hvr-fade-3'),
            
            uiOutput('ComboOutput1')
          ),
          tabPanel(
            h2('用户最爱'),
            br(),
            actionButton('favor1', '套餐一', class = 'hvr-fade-1'),
            actionButton('favor2', '套餐二', class = 'hvr-fade-2'),
            actionButton('favor3', '套餐三', class = 'hvr-fade-3'),
            uiOutput('ComboOutput2')
          )
        )
        
        
      )
      
      
      
      
      # hr(),
      # fluidRow(
      #     uiOutput('productTable')
      #
      # )
    )
  ),
  tabPanel(style = 'color:white',
    span(style = 'font-weight:normal;font-size:20px','关于我们'),
           tags$head(includeCSS('www/style.css')),
           includeMarkdown('README.md'))
)


server <- function(input, output, session) {
  
  observeEvent(input$Logic == FALSE, {
    # Change the following line for more examples
    toggle("LogicPanel")
  })
  
  output$AddressOut <- renderText({
    paste(input$Address, ' 河北石家庄最近6个月平均污染指数(pm2.5): ', '严重', sep = '')
  })
  
  output$Member <- renderText({
    paste(
      '家中',
      ifelse(input$Old, '有', '无'),
      '老人，',
      ifelse(input$Child == '无', '无', '有'),
      input$Child,
      '小孩',
      sep = '',
      '，提高安全系数比重'
    )
  })
  
  output$PhoneOut <- renderText({
    paste('用户', input$Phone, '购买过海尔家电产品', sep = '', '，根据购买记录匹配推荐产品优先级')
  })
  reset <- reactiveValues(data = NULL)
  
  observeEvent(input$Submit, {
    reset$data <- 1
  })
  
  observeEvent(input$Reset, {
    reset$data <- NULL
  })
  
  output$FloorAir <- renderUI({
    if (!'air' %in% input$Combo) {
      return()
    } else{
      sliderInput(
        'NoFloorAir',
        '立式空调',
        min = 0,
        max = input$Living + input$Bed,
        step = 1,
        value = input$Living
      )
    }
  })
  
  
  output$tagPicker <- renderUI({
    ui <- list(
      ref = selectizeInput(
        'RefTag',
        '冰箱标签',
        multiple = TRUE,
        choices = ref_hot_tag$tag,
        selected = head(ref_hot_tag$tag, 2),
        options = list(
          create = TRUE,
          maxItems = 5,
          placeholder = 'Please select at least 1 tag'
        )
      ),
      air = selectizeInput(
        'AirTag',
        '空调标签',
        multiple = TRUE,
        choices = air_hot_tag$tag,
        selected = head(air_hot_tag$tag, 2),
        options = list(
          create = TRUE,
          maxItems = 5,
          placeholder = 'Please select at least 1 tag'
        )
      ),
      wash = selectizeInput(
        'WashTag',
        '洗衣机标签',
        multiple = TRUE,
        choices = wash_hot_tag$tag,
        selected = head(wash_hot_tag$tag, 2),
        options = list(
          create = TRUE,
          maxItems = 5,
          placeholder = 'Please select at least 1 tag'
        )
      ),
      tv = selectizeInput(
        'TVTag',
        '电视标签',
        multiple = TRUE,
        choices = tv_hot_tag$tag,
        selected = head(tv_hot_tag$tag, 2),
        options = list(
          create = TRUE,
          maxItems = 5,
          placeholder = 'Please select at least 1 tag'
        )
      ),
      hood = selectizeInput(
        'HoodTag',
        '油烟机标签',
        multiple = TRUE,
        choices = hood_hot_tag$tag,
        selected = head(hood_hot_tag$tag, 2),
        options = list(
          create = TRUE,
          maxItems = 5,
          placeholder = 'Please select at least 1 tag'
        )
      ),
      gas = selectizeInput(
        'GasTag',
        '燃气灶标签',
        multiple = TRUE,
        choices = gas_hot_tag$tag,
        selected = head(gas_hot_tag$tag, 2),
        options = list(
          create = TRUE,
          maxItems = 5,
          placeholder = 'Please select at least 1 tag'
        )
      ),
      dish = selectizeInput(
        'DishTag',
        '洗碗机标签',
        multiple = TRUE,
        choices = dish_hot_tag$tag,
        selected = head(dish_hot_tag$tag, 2),
        options = list(
          create = TRUE,
          maxItems = 5,
          placeholder = 'Please select at least 1 tag'
        )
      )
      
    )
    tagList(ui[input$Combo])
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
  
  brand <- eventReactive(input$Submit, {
    input$Brand
  })
  
  floor <- eventReactive(input$Submit, {
    input$NoFloorAir
  })
  
  reftag <- eventReactive(input$Submit, {
    input$RefTag
  })
  
  airtag <- eventReactive(input$Submit, {
    input$AirTag
  })
  
  washtag <- eventReactive(input$Submit, {
    input$WashTag
  })
  
  tvtag <- eventReactive(input$Submit, {
    input$TVTag
  })
  
  hoodtag <- eventReactive(input$Submit, {
    input$HoodTag
  })
  
  gastag <- eventReactive(input$Submit, {
    input$GasTag
  })
  
  dishtag <- eventReactive(input$Submit, {
    input$DishTag
  })
  
  w <- reactiveValues(ui = 'Initial status')
  
  saletable <- reactive({
    if (is.null(reset$data)) {
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
      wash = tableFilter('wash', 'sale', brand, washtag()),
      tv = tableFilter('tv', 'sale', brand, tvtag()),
      hood = tableFilter('hood', 'sale', brand, hoodtag()),
      gas = tableFilter('gas', 'sale', brand, gastag()),
      dish = tableFilter('dish', 'sale', brand, dishtag())
    )
    
    if ('air' %in% categories) {
      if (floor == 0) {
        tableSelector <- tableList[categories]
        tableSelector[['wall']] <- tableSelector[['air']]
        tableSelector[['air']] <- NULL
      } else if (floor == room) {
        tableSelector <- tableList[categories]
        tableSelector[['floor']] <- tableSelector[['air']]
        tableSelector[['air']] <- NULL
      } else{
        tableSelector <- tableList[categories]
        tableSelector[['floor']] <-
          tableSelector[['air']][['floor']]
        tableSelector[['wall']] <- tableSelector[['air']][['wall']]
        tableSelector[['air']] <- NULL
      }
    } else{
      tableSelector <- tableList[categories]
    }
    
    categories <- names(tableSelector)
    li <- list()
    for (i in categories) {
      product <- tableSelector[[i]][1, ]
      li[[i]] <-  product
    }
    tablei <- do.call(rbind, li)
    tablei <-
      mutate(
        tablei,
        category = categories,
        total = ifelse(
          category == 'floor',
          floor * price,
          ifelse(category == 'wall', (room - floor) * price, price)
        )
      )
    
    combo_result1 <-
      comboModeler(categories,
                   tableSelector,
                   li,
                   tablei,
                   room,
                   floor,
                   budget,
                   range)
    table1 <- combo_result1$table
    l1 <- combo_result1$l
    if (is.null(combo_result1)) {
      combo_result2 <- NULL
    } else{
      combo_result2 <-
        comboModelerNext(categories, tableSelector, l1, room, floor, budget, range)
    }
    table2 <- combo_result2$table
    l2 <- combo_result2$l
    tableSelector2 <- combo_result2$tableSelector
    if (is.null(combo_result2)) {
      combo_result3 <- NULL
    } else{
      combo_result3 <-
        comboModelerNext(categories, tableSelector2, l2, room, floor, budget, range)
    }
    table3 <- combo_result3$table
    ui <- list(combo1 = table1,
               combo2 = table2,
               combo3 = table3)
    return(ui)
  })
  
  ww <- reactiveValues(tableList = NULL)
  
  v <- reactiveValues(ui = 'Initial status')
  
  
  favtable <- reactive({
    if (is.null(reset$data)) {
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
      wash = tableFilter('wash', 'like', brand, washtag()),
      tv = tableFilter('tv', 'like', brand, tvtag()),
      hood = tableFilter('hood', 'like', brand, hoodtag()),
      gas = tableFilter('gas', 'like', brand, gastag()),
      dish = tableFilter('dish', 'like', brand, dishtag())
    )
    
    if ('air' %in% categories) {
      if (floor == 0) {
        tableSelector <- tableList[categories]
        tableSelector[['wall']] <- tableSelector[['air']]
        tableSelector[['air']] <- NULL
      } else if (floor == room) {
        tableSelector <- tableList[categories]
        tableSelector[['floor']] <- tableSelector[['air']]
        tableSelector[['air']] <- NULL
      } else{
        tableSelector <- tableList[categories]
        tableSelector[['floor']] <-
          tableSelector[['air']][['floor']]
        tableSelector[['wall']] <- tableSelector[['air']][['wall']]
        tableSelector[['air']] <- NULL
      }
    } else{
      tableSelector <- tableList[categories]
    }
    
    categories <- names(tableSelector)
    li <- list()
    for (i in categories) {
      product <- tableSelector[[i]][1, ]
      li[[i]] <-  product
    }
    tablei <- do.call(rbind, li)
    tablei <-
      mutate(
        tablei,
        category = categories,
        total = ifelse(
          category == 'floor',
          floor * price,
          ifelse(category == 'wall', (room - floor) * price, price)
        )
      )
    
    combo_result1 <-
      comboModeler(categories,
                   tableSelector,
                   li,
                   tablei,
                   room,
                   floor,
                   budget,
                   range)
    table1 <- combo_result1$table
    l1 <- combo_result1$l
    if (is.null(combo_result1)) {
      combo_result2 <- NULL
    } else{
      combo_result2 <-
        comboModelerNext(categories, tableSelector, l1, room, floor, budget, range)
    }
    table2 <- combo_result2$table
    l2 <- combo_result2$l
    tableSelector2 <- combo_result2$tableSelector
    if (is.null(combo_result2)) {
      combo_result3 <- NULL
    } else{
      combo_result3 <-
        comboModelerNext(categories, tableSelector2, l2, room, floor, budget, range)
    }
    table3 <- combo_result3$table
    ui <- list(combo1 = table1,
               combo2 = table2,
               combo3 = table3)
    return(ui)
  })
  
  vv <- reactiveValues(tableList = NULL)
  
  
  observeEvent(input$Submit, {
    updateProgressBar(session = session, id = "pb", value = 0) # reinitialize to 0 if you run the calculation several times
    session$sendCustomMessage(type = 'launch-modal', "pb-modal") # launch the modal
    ww$tableList <- saletable()
    Sys.sleep(0.5)
    
    updateProgressBar(session = session, id = "pb", value = 50)
    
    vv$tableList <- favtable()
    Sys.sleep(0.5)
    
    updateProgressBar(session = session, id = 'pb', value = 100)
    
    Sys.sleep(0.5)
    session$sendCustomMessage(type = 'remove-modal', "pb-modal")
  })
  
  observeEvent(input$sale1, {
    w$ui <- ww$tableList[['combo1']]
  })
  
  observeEvent(input$sale2, {
    w$ui <- ww$tableList[['combo2']]
  })
  
  observeEvent(input$sale3, {
    w$ui <- ww$tableList[['combo3']]
  })
  
  output$ComboOutput1 <- renderUI({
    if (is.null(reset$data)) {
      return(NULL)
    }
    if (is.null(w$ui)) {
      return(h3(style = 'coloe:white', '没有符合条件的组合，请重新搜索。'))
    } else if (w$ui == 'Initial status') {
      return(NULL)
    } else{
      tagList(
        wellPanel(style = 'opacity: 0.6;background:black;color:white',
        datatable(
          w$ui,
          width = '800px',
          rownames = FALSE,
          colnames = c('产品名称', '价格', '匹配度', '品类', '总价'),
          caption = tags$caption(style = 'color: black', h2(
            style = 'text-align: right;color:gold', paste('Price', prettyNum(sum(w$ui$total), big.mark = ','), sep = ': ')
          )),
          options = list(dom = 't')
        ) %>%
          formatStyle('name', fontWeight = 'bold') %>%
          formatPercentage('match', 1) %>%
          formatStyle('total', color = 'gold') %>%
          formatStyle(colnames(w$ui), color = '#fff', backgroundColor = '#2d2d2d')
      )
      )
    }
  })
  
  observeEvent(input$favor1, {
    v$ui <- vv$tableList[['combo1']]
  })
  
  observeEvent(input$favor2, {
    v$ui <- vv$tableList[['combo2']]
  })
  
  observeEvent(input$favor3, {
    v$ui <- vv$tableList[['combo3']]
  })
  
  output$ComboOutput2 <- renderUI({
    if (is.null(reset$data)) {
      return(NULL)
    }
    if (is.null(v$ui)) {
      return(h3(style = 'color:white', '没有符合条件的组合，请重新搜索。'))
    } else if (v$ui == 'Initial status') {
      return(NULL)
    } else{
      tagList(
        wellPanel(style = 'opacity: 0.6;background:black;color:white',
        datatable(
          v$ui,
          width = '800px',
          rownames = FALSE,
          colnames = c('产品名称', '价格', '匹配度', '品类', '总价'),
          caption = tags$caption(style = 'color: black', h2(
            style = 'text-align: right;color:gold', paste('Price', prettyNum(sum(v$ui$total), big.mark = ','), sep = ': ')
          )),
          options = list(dom = 't')
        ) %>%
          formatStyle('name', fontWeight = 'bold') %>%
          formatPercentage('match', 1) %>%
        formatStyle('total', color = 'gold') %>%
          formatStyle(colnames(v$ui), color = '#fff', backgroundColor = '#2d2d2d')
      )
      )
    }
  })
  
  
}

shinyApp(ui, server)