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
  span(style = 'font-weight:normal;font-size:25px', 'NICE'),
  position = 'fixed-top',
  theme = shinytheme("flatly"),
  windowTitle = 'NICE - Next-generation Interactive Context Engine',
  tabPanel(
    span(style = 'font-weight:normal;font-size:20px','套餐选择'),
    tags$head(includeCSS('www/style.css'),
              includeCSS('www/hover.css')),
    
    fluidRow(column(offset = 8,
                    width =4,
                    tags$img(src = 'logo.jpg')
                    )
             ),
    fluidRow(
        useShinyjs(),
          column(1,
                  actionButton(
                    "Submit" ,
                    span(style = 'font-size:20px','提交'),
                    class ='hvr-fade-submit',
                    icon = icon('upload fa-2x'),
                    width = '100%',
                    style = 'height:80px'
                  ),
            actionButton(
              'Reset',
              span(style = 'font-size:20px', '返回'),
              icon = icon('refresh fa-2x'),
              class = 'hvr-fade-submit',
              width = '100%',
              style = 'height:80px'
            )
        
      ),
      column(width = 10,
             fluidRow(
               column(10, 
             wellPanel(style = 'opacity: 1;color:white;background:#065AA2',
                       id = 'ConfigPanel',
                       fluidRow(
                         column(3,
                                sliderInput(
                                  'Bed',
                                  '卧室',
                                  min = 1,
                                  max = 4 ,
                                  value = 2
                                )
                                
                         ),
                         column(3,
                                sliderInput(
                                  'Living',
                                  '客厅',
                                  min = 1,
                                  max = 4,
                                  value  = 1
                                )
                         ),
                         column(3,
                                sliderInput(
                                  'Budget',
                                  '预算',
                                  min = 20000,
                                  max = 100000,
                                  step = 5000,
                                  value = 30000
                                )
                         ),
                         column(3,
                                sliderInput(
                                  'Range',
                                  '预算调整幅度+/-',
                                  min = 0.05,
                                  max = 0.2,
                                  step = 0.05,
                                  value = 0.1
                                )
                         )
                       ),
                       hr(),
                       fluidRow(
                         column(4,
                                div(style = 'padding: 0px 0px 5px;',strong('家电品类'), actionBttn(inputId='catWiki', size = 'xs', color = 'primary', style = 'fill', icon('question-circle-o', 'fa-lg'))),
                                selectizeInput(
                                  'Combo',
                                  NULL,
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
                                )
                         ),
                         column(2,
                                uiOutput('FloorAir')
                         ),
                         column(3,
                                div(style = 'padding: 0px 0px 5px;',strong('品牌偏好')),
                                checkboxInput('BrandPref', '卡萨帝', FALSE)
                                )
                       ),
                       hr(),
                       fluidRow(
                         column(2,
                                sliderInput(
                                  'People',
                                  '住户人数',
                                  min = 1,
                                  max = 10,
                                  value = 3,
                                  step = 1
                                )
                         ),
                         column(2,
                                selectizeInput('Old', '老人', choices = list('无' = '无',
                                                                              '有' = '有'),
                                               selected = NULL, options = list(placeholder = '家中有无老人', onInitialize = I('function() { this.setValue(""); }'))
                                )
                         ),
                         column(2,
                                selectizeInput(
                                  'Child',
                                  '小孩',
                                  choices = list(
                                    '无' = '无',
                                    '有，3岁以下' = '3岁以下',
                                    '有，3岁以上' = '3岁以上'
                                  ),
                                  selected = NULL, options = list(placeholder = '家中有无小孩', onInitialize = I('function() { this.setValue(""); }'))
                                  
                                )
                         )
                         ),
                       fluidRow(
                         column(2,
                                textInput('Phone', '联系电话', value = '', placeholder = '请填入联系电话')
                         ),
                         column(2,
                                selectizeInput('UserProvince', '用户地址', choices = unique(filter(china_pcr, province != 'province')$province) , selected = NULL, options = list(placeholder = '省份', onInitialize = I('function() { this.setValue(""); }')))
                         ),
                         column(2,
                                div(style = 'padding: 0px 0px 26px;'),
                                uiOutput('UserCity')
                                #selectizeInput('UserCity', NULL, choices = unique(china_pcr$city), selected = NULL, options = list(placeholder = '城市', onInitialize = I('function() { this.setValue(""); }')))
                         ),
                         column(3,
                                div(style = 'padding: 0px 0px 26px;'),
                                textInput('Address', NULL, value = '', placeholder = '请填入具体地址')
                                #checkboxInput('PM', '根据地址是否选择自清洁/净化pm2.5空调？', FALSE)
                         )
                       )
                       #####
                       
             )
             ),
             column(2,
                    wellPanel(style = 'opacity:1;color:white;background:#065AA2',
                              id= 'TagPanel',
                      uiOutput('tagPicker')
                    )
                    )
             ),
             fluidRow(
               column(
                 width = 5,
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
                hidden(actionButton('Logic', '套餐选择逻辑', class = 'btn-info')),
                hidden(wellPanel(style = 'opacity:0.6;background:black;color:white',
                           id = 'LogicPanel',
                           p('根据住房户型、用户预算和所需家电品类，初步筛选目标家电产品'),
                           p('根据用户输入基本信息，客制化精确筛选目标家电产品：'),
                           uiOutput('AddressOut'),
                           uiOutput('Member'),
                           uiOutput('PhoneOut')
                 )),
                 hidden(actionButton('Bought', '购买记录', class = 'btn-success'))
               ),
               column(width = 7,
                      hidden(div(id = 'ResultPanel', 
                      tabsetPanel(
                        tabPanel(id = 'SalePanel',
                          h2('热卖产品'),
                          br(),
                          actionButton('sale1', '套餐一', class = 'hvr-fade-1'),
                          actionButton('sale2', '套餐二', class = 'hvr-fade-2'),
                          actionButton('sale3', '套餐三', class = 'hvr-fade-3'),
                          
                          uiOutput('ComboOutput1')
                        ),
                        tabPanel(id = 'FavorPanel',
                          h2('用户最爱'),
                          br(),
                          actionButton('favor1', '套餐一', class = 'hvr-fade-1'),
                          actionButton('favor2', '套餐二', class = 'hvr-fade-2'),
                          actionButton('favor3', '套餐三', class = 'hvr-fade-3'),
                          uiOutput('ComboOutput2')
                        )
                      )
                      )
                      )
                      
                      
               )
             )
      )
      
    )
    
   
      
      
  ),
  tabPanel(style = 'color:white',
    span(style = 'font-weight:normal;font-size:20px','关于我们'),
           tags$head(includeCSS('www/style.css')),
           includeMarkdown('README.md'))
)


server <- function(input, output, session) {
  
  # observe({
  #   hide('ResultPanel')
  #   hide('Logic')
  #   hide('Bought')
  # })
  # observe({
  #   hide('SalePanel')
  #   hide('sale1')
  #   hide('sale2')
  #   hide('sale3')
  # })
  # observe({
  #   hide('FavorPanel')
  #   hide('favor1')
  #   hide('favor2')
  #   hide('favor3')
  # })
  
  observeEvent(input$Submit, {
    hide("ConfigPanel", anim = TRUE, time = 0.2)
    hide("TagPanel", anim = TRUE, time = 0.2)
    show('ResultPanel', anim = TRUE, time = 0.2)
    # show('SalePanel', anim = TRUE, time = 0.2)
    # show('FavorPanel', anim = TRUE, time = 0.2)
    # show('sale1', anim = TRUE, time = 0.2)
    # show('sale2', anim = TRUE, time = 0.2)
    # show('sale3', anim = TRUE, time = 0.2)
    # show('favor1', anim = TRUE, time = 0.2)
    # show('favor2', anim = TRUE, time = 0.2)
    # show('favor3', anim = TRUE, time = 0.2)
    show('Logic', anim = TRUE, time = 0.2)
    show('Bought', anim = TRUE, time = 0.2)
    show('LogicPanel', anim = TRUE, time = 0.2)
  })
  
 

  observeEvent(input$Reset, {
    show("ConfigPanel", anim = TRUE, time = 0.2)
    show("TagPanel", anim = TRUE, time = 0.2)
    hide('ResultPanel', anim = TRUE, time = 0.2)
    # hide('SalePanel', anim = TRUE, time = 0.2)
    # hide('FavorPanel', anim = TRUE, time = 0.2)
    # hide('sale1', anim = TRUE, time = 0.2)
    # hide('sale2', anim = TRUE, time = 0.2)
    # hide('sale3', anim = TRUE, time = 0.2)
    # hide('favor1', anim = TRUE, time = 0.2)
    # hide('favor2', anim = TRUE, time = 0.2)
    # hide('favor3', anim = TRUE, time = 0.2)
    hide('Logic', anim = TRUE, time = 0.2)
    hide('Bought', anim = TRUE, time = 0.2)
    hide('LogicPanel', anim = TRUE, time =0.2)
  })
  
  observeEvent(input$catWiki, {
    showModal(modalDialog(
      title = "品类选择",
      footer = modalButton("返回"),
      size = 'm',
      "用户可以根据自身需求定制家电品类。模型按照品类的排列顺序，确定产品的优先选择权。例如，如果排列顺序为空调、洗衣机、冰箱，模型会根据“热卖产品”或“用户喜爱”模式，优先推荐空调产品，再依次推荐洗衣机和冰箱产品。",
      easyClose = TRUE
    ))
  })
  
  
  output$AddressOut <- renderUI({
    tagList(
    p(style = 'color: gold; font-size:20px', paste(input$UserProvince, input$City, input$Address, ifelse(input$UserProvince %in% c("山西省", "吉林省", "宁夏回族自治区", "北京市", "辽宁省", "黑龙江省", "新疆维吾尔自治区", "内蒙古自治区", "河北省", "青海省",          
                                                                                          "甘肃省", "西藏自治区", "天津市", "陕西省", "四川省", "山东省"), '，气候偏干燥，模型优先推荐带有加湿功能的产品', '，气候偏潮湿，模型优先推荐带有除湿功能的产品'), sep = '')
    ),
    p(style = 'color:gold; font-size:20px', paste(ifelse(input$UserProvince %in% c('河北省', '辽宁省', '吉林省', '黑龙江省'), 'pm 2.5污染指数严重，模型优先推荐自清洁功能的产品', 'pm2.5污染指数良好')))
    )
  })
  
  output$Member <- renderUI({
    tagList(
      p(style = 'color:gold; font-size:20px',
    paste(
      '家中',
      input$Old,
      '老人，',
      ifelse(input$Child == '无', '', '有'),
      input$Child,
      '小孩',
      ifelse(input$Old == '无' & input$Child == '无', '', '，提高安全系数比重'),
      sep = ''
    )
      )
    )
  })
  
  output$PhoneOut <- renderUI({
    tagList(
    p(style = 'color:gold; font-size:20px',
      if(input$Phone == '13901682345'){
    paste('用户', input$Phone, '，查找到购买记录，已购买产品包含以下标签：节能|环保', sep = '', '，根据购买记录匹配推荐产品优先级')
      }else{
          paste('用户', input$Phone, '，未查找到购买记录', sep='')
        }
    )
    )
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
      tagList(
        tags$style(
          ".js-irs-0 .irs-bar {
border-top-color: #fbf23d;
border-bottom-color: #fbf23d;
color:black;
} 

.js-irs-0 .irs-bar-edge {
border-color: #fbf23d;
color:black;
}

.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
background: #fbf23d;
color:black;
}"
        ),
      sliderInput(
        'NoFloorAir',
        '立式空调',
        min = 0,
        max = input$Living + input$Bed,
        step = 1,
        value = input$Living
      )
      )
    }
  })
  
  output$UserCity <- renderUI({
    city <- filter(china_pcr, province == input$UserProvince) %>%.$city %>% unique %>% as.character
    ui <- list(
      selectizeInput('City', NULL, choices = city, selected = NULL, options = list(placeholder = '城市', onInitialize = I('function() { this.setValue(""); }')))
      
    )
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