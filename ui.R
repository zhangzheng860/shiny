library(shiny)
library(tidyverse)
library(readxl)
library(tidygraph)
library(ggraph)
library(igraph)
library(ggnewscale)
library(patchwork)
library(cowplot)
library(colourpicker)
# 模拟布局函数（需根据实际需求完善，此处简化）
layout_function <- function(graph) {
  graph %>% 
    as_tibble() %>% 
    mutate(x = runif(n()), y = runif(n()))
}

ui <- fluidPage(
  titlePanel("环形网络组合图绘制"),
 fluidRow(sidebarLayout(
    sidebarPanel(width=3,height=8,
      fileInput("nodes_file", "节点", accept = c(".xlsx")),
      helpText('格式提示：每列依次为名字和类型'),
      fileInput("ppi_file", "蛋白质互作", accept = c(".xlsx")),
      helpText('格式提示：每列依次为起始、终止和类型'),
      fileInput("data_ex_file", "数据", accept = c(".xlsx")),
      helpText('格式提示：每列依次为序号、名字、数据1、数据2、数据3、数据4'),
      fileInput("edges_file", "边缘", accept = c(".xlsx")),
      helpText('格式提示：每列依次为起始、终止和类型'),
     
      h4("颜色设置"),
      colourInput("cis_color", "Cis",value  = "red" ),
      colourInput("ctrl_color", "Ctrl", value = "green")
      
      
      
    ),mainPanel(
      plotOutput("main_plot"),width =9 
      ,height = 9.9
     )
  )
))
