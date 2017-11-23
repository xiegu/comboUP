library(dplyr)
library(jiebaR)

haier_score <- filter(score_full, brand == '海尔')
haier_p <- filter(com, brand == '海尔') %>% select(id, p)

haier_data <-
  inner_join(haier_score, haier_p, by = c('id' = 'id')) %>% unique

# Text mining on product name
stop_words <- list(
  'ref' = c('冰箱', '升', 'BCD', '京东', '电冰箱'),
  'air' = c('柜机',  '空调', 'KFR', 'GW', '匹' , '挂机', '壁挂式', '京东'),
  'wash' = c('洗', '洗衣机', '公斤', '京东'),
  'tv' = c('电视机', '电视', '英寸', '京东'),
  'gas' = c('燃气灶', 'JZY', 'JZT', '煤气灶',  '京东'),
  'hood' = c('油烟机', '吸油烟机', '抽油烟机',  '安装', 'CXW', '京东'),
  'dish' = c('洗碗机' , '套', '京东')
)

wk <- worker()

name_seg <-
  lapply(haier_data$name, function(x)
    wk[x]) %>% lapply(., function(x)
      x[-grep("([A-Z]|[0-9]|[a-z]|海尔)", x)])

# Summarize hot tag in each category
hot_tag <- function(category = 'ref') {
  index <- which(haier_data$category == category)
  tag <- name_seg[index] %>% unlist %>% table
  tag_df <-
    data.frame(tag = names(tag), count = as.numeric(tag)) %>% filter(.,!tag %in% stop_words[[category]]) %>% arrange(.,-count)
  return(tag_df)
}

ref_tag <- hot_tag('ref')
air_tag <- hot_tag('air')
wash_tag <- hot_tag('wash')
tv_tag <- hot_tag('tv')
hood_tag <- hot_tag('hood')
dish_tag <- hot_tag('dish')
gas_tag <- hot_tag('gas')

floor_air_tag <- c('立式', '圆柱', '柜式', '立柜')

air_tag <- filter(air_tag,!tag %in% floor_air_tag)

ref_hot_tag <- head(ref_tag, 20)
air_hot_tag <- head(air_tag, 40)
wash_hot_tag <- head(wash_tag, 20)
tv_hot_tag <- head(tv_tag, 30)
hood_hot_tag <- head(hood_tag, 40)
dish_hot_tag <- head(dish_tag, 20)
gas_hot_tag <- head(gas_tag, 20)

# Configration
#layout <- data.frame(living = 1, room = 3)
#range <- 0.1 # +-10%

#air_no <- layout$living + layout$room #max
sub_brand <- c('卡萨帝')
categories <- unique(haier_data$category)
