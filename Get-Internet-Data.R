library(rvest)
surl = "http://www.appledaily.com.tw/realtimenews/section/new/"
apple = read_html(surl,encoding="UTF-8")
apple %>% iconv(from = 'UTF-8', to = 'UTF-8')
rddt = apple %>% html_nodes('.rtddt')
time = rddt %>% html_nodes('time') %>% html_text()
title = rddt %>% html_nodes('h1') %>% html_text() %>% iconv(from = 'UTF-8', to = 'UTF-8')
category = rddt %>% html_nodes('h2') %>% html_text() %>% iconv(from = 'UTF-8', to = 'UTF-8')
domain = "http://www.appledaily.com.tw"
url = rddt %>% html_nodes('a') %>% html_attr('href')
url = paste0(domain, url)

news = data.frame(time=time, title=title, category=category, url=url)