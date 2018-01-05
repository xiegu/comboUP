library(dplyr)
library(magrittr)
library(XML)
#library(rdom)
library(xml2)
library(RSelenium)

# login configuration
num_fails_to_lockout <- 5

# functions for modeling
multiAndGrep <- function(patterns, x) {
  n <- 0
  for (i in patterns) {
    y <- grepl(i, x)
    n <- n + y
  }
  if (n > 0) {
    return(n)
  } else
    (return(0))
}

tableFilter <-
  function(cat = 'ref',
           preference,
           brand,
           tag,
           no_floor = 1,
           no_room = 2) {
    if (cat == 'air') {
      if (no_floor == 0) {
        haier_data <-
          filter(haier_data, (!grepl(
            paste(floor_air_tag, collapse = '|'), name
          )) & category == cat)
      } else if (no_floor == no_room) {
        haier_data <-
          filter(haier_data, grepl(paste(floor_air_tag, collapse = '|'), name) &
                   category == cat)
      } else{
        haier_data_floor <-
          filter(haier_data, grepl(paste(floor_air_tag, collapse = '|'), name) &
                   category == cat) %>%
          mutate(match = sapply(name, function(x)
            multiAndGrep(tag, x)) / length(tag)) %>% filter(match > 0) #%>% select(-match)
        haier_data_wall <-
          filter(haier_data, (!grepl(
            paste(floor_air_tag, collapse = '|'), name
          )) & (category == cat)) %>%
          mutate(match = sapply(name, function(x)
            multiAndGrep(tag, x)) / length(tag)) %>% filter(match > 0) #%>% select(-match)
        if (preference == 'sale') {
          haier_data_floor <-
            arrange(haier_data_floor,-match,-comment_count)
          haier_data_wall <-
            arrange(haier_data_wall,-match,-comment_count)
        } else if (preference == 'like') {
          haier_data_floor <- arrange(haier_data_floor,-match,-score)
          haier_data_wall <- arrange(haier_data_wall,-match,-score)
        } else{
          haier_data_floor <- haier_data_floor
          haier_data_wall <- haier_data_wall
        }
        if (isTRUE(brand)) {
          haier_data_floor2 <-
            filter(haier_data_floor, grepl(paste(sub_brand, collapse = '|'), name))
          haier_data_wall2 <-
            filter(haier_data_wall, grepl(paste(sub_brand, collapse = '|'), name))
        } else{
          haier_data_floor2 <- haier_data_floor
          haier_data_wall2 <- haier_data_wall
        }
        if (nrow(haier_data_floor2) == 0) {
          haier_data_floor <- haier_data_floor
        } else{
          haier_data_floor <- haier_data_floor2
        }
        if (nrow(haier_data_wall2) == 0) {
          haier_data_wall <- haier_data_wall
        } else{
          haier_data_wall <- haier_data_wall2
        }
        haier_data_floor <-
          select(haier_data_floor, name, p, match) %>% rename(price = p)
        haier_data_wall <-
          select(haier_data_wall, name, p, match) %>% rename(price = p)
        haier_data <-
          list(floor = haier_data_floor, wall = haier_data_wall)
        return(haier_data)
      }
    } else{
      haier_data <- haier_data
    }
    dt <-
      filter(haier_data, category == cat) %>% mutate(match = sapply(name, function(x)
        multiAndGrep(tag, x)) / length(tag)) %>% filter(match > 0) #%>% select(-match)
    if (preference == 'sale') {
      dt <- arrange(dt,-match,-comment_count)
    } else if (preference == 'like') {
      dt <- arrange(dt,-match,-score)
    } else{
      dt <- dt
    }
    if (isTRUE(brand)) {
      dt2 <- filter(dt, grepl(paste(sub_brand, collapse = '|'), name))
    } else{
      dt2 <- dt
    }
    if (nrow(dt2) == 0) {
      dt <- dt
    } else{
      dt <- dt2
    }
    dt <- select(dt, name, p, match) %>% rename(price = p)
    return(dt)
  }

budgetChecker <- function(p, b, r) {
  check <- ifelse(p > b * (1 + r) | p < b * (1 - r), FALSE, TRUE)
  check
}

comboModeler <-
  function(categories,
           tableSelector,
           li,
           tablei,
           room,
           floor,
           b,
           r) {
    if (budgetChecker(sum(tablei$total), b, r)) {
      table <- tablei
      l <- li
    } else{
      l1 <- li
      for (i in categories) {
        indexi <- which(categories == i)
        if (indexi > 1) {
          n <-
            min(20, nrow(tableSelector[[indexi - 1]])) # maximum 20 attempts only
          price2ndMax <-
            sort(head(tableSelector[[indexi - 1]]$price, n), decreasing = TRUE)[2]
          indexP2ndMax <-
            which(head(tableSelector[[indexi - 1]]$price, n) == price2ndMax)[1]
          l1[[indexi - 1]] <-
            tableSelector[[indexi - 1]][indexP2ndMax,]
        }
        n <-
          min(20, nrow(tableSelector[[indexi]])) # maximum 20 attempts only
        for (j in 2:n) {
          product <- tableSelector[[i]][j,]
          l1[[i]] <- product
          table1 <- do.call(rbind, l1)
          table1 <-
            mutate(
              table1,
              category = categories,
              total = ifelse(
                category == 'floor',
                floor * price,
                ifelse(category == 'wall', (room - floor) * price, price)
              )
            )
          if (budgetChecker(sum(table1$total), b, r)) {
            break
          } else{
            next
          }
        }
        if (budgetChecker(sum(table1$total), b, r)) {
          break
        } else{
          next
        }
      }
      
      if (budgetChecker(sum(table1$total), b, r)) {
        table <- table1
        l <- l1
      } else{
        l2 <- l1
        for (i in categories) {
          indexi <- which(categories == i)
          ts <- arrange(tableSelector[[i]],-price)
          if (indexi > 1) {
            price2ndMax <-
              sort(tableSelector[[indexi - 1]]$price, decreasing = TRUE)[2]
            indexP2ndMax <-
              which(tableSelector[[indexi - 1]]$price == price2ndMax)[1]
            l2[[indexi - 1]] <-
              tableSelector[[indexi - 1]][indexP2ndMax,]
          }
          n <- which(ts$price == l1[[i]]$price)[1]
          for (j in 1:n) {
            product <- ts[j,]
            l2[[i]] <- product
            table2 <- do.call(rbind, l2)
            table2 <-
              mutate(
                table2,
                category = categories,
                total = ifelse(
                  category == 'floor',
                  floor * price,
                  ifelse(category == 'wall', (room - floor) * price, price)
                )
              )
            if (budgetChecker(sum(table2$total), b, r)) {
              break
            } else{
              next
            }
          }
          if (budgetChecker(sum(table2$total), b, r)) {
            break
          } else{
            next
          }
        }
        if (budgetChecker(sum(table2$total), b, r)) {
          table <- table2
          l <- l2
        } else{
          return(NULL)
        }
      }
    }
    return(list(l = l, table = table))
  }


comboModelerNext <- function(cat, tabSel, ls, ro, f, b, r) {
  for (i in cat) {
    tabSel[[i]] <- tabSel[[i]][-which(tabSel[[i]]$name == ls[[i]]$name), ]
  }
  
  li <- list()
  for (i in cat) {
    product <- tabSel[[i]][1, ]
    li[[i]] <-  product
  }
  tablei <- do.call(rbind, li)
  tablei <-
    mutate(
      tablei,
      category = cat,
      total = ifelse(
        category == 'floor',
        f * price,
        ifelse(category == 'wall', (ro - f) * price, price)
      )
    )
  if (budgetChecker(sum(tablei$total), b, r)) {
    table <- tablei
    l <- li
    tabSel <- tabSel
  } else{
    l1 <- li
    for (i in cat) {
      indexi <- which(cat == i)
      if (indexi > 1) {
        n <- min(20, nrow(tabSel[[indexi - 1]]))# maximum 20 attempts only
        price2ndMax <-
          sort(head(tabSel[[indexi - 1]]$price, n), decreasing = TRUE)[2]
        indexP2ndMax <-
          which(head(tabSel[[indexi - 1]]$price, n) == price2ndMax)[1]
        l1[[indexi - 1]] <- tabSel[[indexi - 1]][indexP2ndMax,]
      }
      n <- min(20, nrow(tabSel[[indexi]]))# maximum 20 attempts only
      for (j in 2:n) {
        product <- tabSel[[i]][j,]
        l1[[i]] <- product
        table1 <- do.call(rbind, l1)
        table1 <-
          mutate(
            table1,
            category = cat,
            total = ifelse(
              category == 'floor',
              f * price,
              ifelse(category == 'wall', (ro - f) * price, price)
            )
          )
        if (budgetChecker(sum(table1$total), b, r)) {
          break
        } else{
          next
        }
      }
      if (budgetChecker(sum(table1$total), b, r)) {
        break
      } else{
        next
      }
    }
    
    if (budgetChecker(sum(table1$total), b, r)) {
      table <- table1
      l <- l1
      tabSel <- tabSel
    } else{
      l2 <- l1
      for (i in cat) {
        indexi <- which(cat == i)
        ts <- arrange(tabSel[[i]],-price)
        if (indexi > 1) {
          price2ndMax <- sort(tabSel[[indexi - 1]]$price, decreasing = TRUE)[2]
          indexP2ndMax <-
            which(tabSel[[indexi - 1]]$price == price2ndMax)[1]
          l2[[indexi - 1]] <- tabSel[[indexi - 1]][indexP2ndMax,]
        }
        n <- which(ts$price == l1[[i]]$price)[1]
        for (j in 1:n) {
          product <- ts[j,]
          l2[[i]] <- product
          table2 <- do.call(rbind, l2)
          table2 <-
            mutate(
              table2,
              category = cat,
              total = ifelse(
                category == 'floor',
                f * price,
                ifelse(category == 'wall', (ro - f) * price, price)
              )
            )
          if (budgetChecker(sum(table2$total), b, r)) {
            break
          } else{
            next
          }
        }
        if (budgetChecker(sum(table2$total), b, r)) {
          break
        } else{
          next
        }
      }
      if (budgetChecker(sum(table2$total), b, r)) {
        table <- table2
        l <- l2
        tabSel <- tabSel
      } else{
        return(NULL)
      }
    }
  }
  return(list(
    l = l,
    table = table,
    tableSelector = tabSel
  ))
}

# pm25 table
pm25Table <- readHTMLTable("http://www.pm25.in/rank",  
                      encoding = "UTF-8", stringsAsFactors = F)[[1]] %>% select(., 1:4)

aqiHistory <- function(city){
  url<-paste0("https://www.aqistudy.cn/historydata/monthdata.php?city=", city) %>% 
    xml2::url_escape(reserved ="][!$&'()*+,;=:/?@#")
  # remDr <- remoteDriver(remoteServerAddr = "localhost" 
  #                       , port = 4445L
  #                       , browserName = "phantomjs"
  # )
  # rD <- rsDriver(browser = 'phantomjs')
  # remDr <- rD[['client']]
  # remDr$open()
  # remDr$navigate(url)
  # content <- remDr$getPageSource()[[1]]
  # 
  # remDr$close()
  # rD[['server']]$stop()
  # 
  #table <- XML::readHTMLTable(content, header=TRUE, stringsAsFactors = F) %>%.[[1]]
  table <- rdom(url) %>% XML::readHTMLTable(header=TRUE, stringsAsFactors = F) %>%.[[1]]
  table <- dplyr::mutate(table, AQI_min = sapply(table$`范围`, function(x) strsplit(x, '~')[[1]][1]), AQI_max = sapply(table$`范围`, function(x) strsplit(x, '~')[[1]][2]))
  return(table)
}