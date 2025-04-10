#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(readxl)
library(tidygraph)
library(ggraph)
library(igraph)
library(ggnewscale)
library(patchwork)
library(cowplot)
# 模拟布局函数（需根据实际需求完善，此处简化）
layout_function <- function(graph) {
  graph %>% 
    as_tibble() %>% 
    mutate(x = runif(n()), y = runif(n()))
}


        server <- function(input, output) {
          output$main_plot <- renderPlot({ 
            # 读取数据
            nodes <- read_excel(input$nodes_file$datapath)
            ppi <- read_excel(input$ppi_file$datapath)
            data_ex <- read_excel(input$data_ex_file$datapath)
            edges <- read_excel(input$edges_file$datapath) 
            colnames(nodes) = c("name", "type")         # nodes文件列名映射
            colnames( edges) = c("from", "to", "type")   # edges文件列名映射
            colnames(data_ex) = c("id", "name", "data1", "data2", "data3", "data4")  # data_ex列名映射
            colnames(ppi) = c('from','to','type') 
            
            graph <- graph_from_data_frame(
              d = edges,         # 边数据框
              vertices = nodes,  # 节点数据框
              directed = FALSE   # 根据实际情况设置是否是有向图
            ) %>% 
              as_tbl_graph() %>%  # 转换为tidygraph对象
              # 节点属性增强
              mutate(
                Popularity = centrality_degree(mode = "all"),  # 计算节点度中心性
                is_center = name %in% c("Cis", "Ctrl"),        # 标记中心节点
                node_size = ifelse(is_center, 15, 5)           # 设置差异大小
              )
            
            # ----------------------------
            # 环形布局生成（关键步骤）
            # ----------------------------
            set.seed(666)  # 固定随机种子保证布局可重复
            
            # 生成基础极坐标布局
            layout <- create_layout(
              graph = graph,
              layout = "linear",      # 线性布局基础
              circular = TRUE         # 转换为环形排列
            ) 
            
            # 调整节点角度实现双中心效果
            adjusted_layout <- layout %>% 
              mutate(
                angle = atan2(y, x),  # 计算原始角度
                # 将非中心节点推到外围
                radius = ifelse(is_center, 0.8, 2),
                # 创建双中心对称分布
                x = radius * cos(angle + ifelse(name == "Cis", pi, 0.4*pi)),
                y = radius * sin(angle + ifelse(name == "Cis", pi, 0.4*pi))
              )
            
            # ----------------------------
            # 可视化阶段
            # ----------------------------
            p5<-ggraph(adjusted_layout) +
              # 弧形边绘制（辐射状效果）
              geom_edge_diagonal(
                aes(edge_color = type),  # 按类型着色
                strength = 0.6,          # 边缘弯曲度
                edge_alpha = 0.8,        # 边缘透明度
                edge_width = 0.5         # 边缘线宽
              ) +
              # 节点绘制（双中心突出）
              geom_node_point(
                aes(
                  size = node_size,
                  color = ifelse(is_center, name, type)  # 中心节点按名称着色
                ),
                alpha = 1
              ) +
              # 中心节点标签
              geom_node_text(
                data = filter(adjusted_layout, is_center),
                aes(label = name),
                color = "black",         # 白色字体
                size = 6,                # 字号大小
                fontface = "bold",       # 粗体显示
                family = "serif"         # 字体类型（可选）
              ) +
              # 视觉映射设置
              scale_size_identity() +    # 直接使用数值映射大小
              scale_color_manual(
                values = c(
                  "Cis" = input$cis_color,     # 红色系中心节点
                  "Ctrl" = input$ctrl_color,    # 蓝色系中心节点
                  "TypeA" = 'red',   # 根据实际数据调整类型颜色
                  "TypeB" ='green'    # 示例类型颜色
                )
              ) +
              # 坐标系与主题设置
              coord_fixed() +            # 防止图形形变
              theme_void() +             # 空白背景
              theme(
                legend.position = "none",      # 隐藏所有图例
                plot.margin = unit(c(0,0,0,0), "cm")  # 去除边距
              )
            
            p2 <- ggplot(data = data_ex) +  # 使用数据框对象而非文件名
              geom_tile(aes(x = id, y = 1, fill = data1), height = 0.2) +
              scale_fill_manual(values = c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3'), na.value = "transparent") +  
              ggnewscale::new_scale_fill() +
              geom_tile(aes(x = id, y = 1.25, fill = data2), height = 0.2) +
              scale_fill_gradient(low = "#fcc5c0", high = "#dd3497", na.value = "transparent") +  
              ggnewscale::new_scale_fill() +  
              geom_tile(aes(x = id, y = 1.5, fill = data3), height = 0.2) +   
              scale_fill_gradient(low = "#dadaeb", high = "#807dba", na.value = "transparent") +  
              coord_polar(start = 1.17*pi) +    
              scale_y_continuous(limits = c(-1, 2.5)) +  
              theme_void()+theme(legend.position =c(1.2,0.5))
            
            
            # 处理 data_ex 数据
            data_ex <- data_ex %>% 
              mutate(id = as.numeric(id))
            
            data_ex_add_angle <- data_ex %>%
              mutate(
                angle = 90 - 360 * (id - 0.5) / nrow(.),
                hjust = ifelse(angle < -90, 0, 1),
                angle = ifelse(angle < -90, angle + 150, angle - 25)
              )
            
            p3 <- ggplot(data = data_ex_add_angle) + 
              geom_bar(aes(x = id, y = data4), stat = "identity", fill = "skyblue") + 
              geom_text(
                data = data_ex_add_angle %>% filter(!name %in% c("Cis", "Ctrl")),
                aes(x = id, y = data4 + 1, label = name, hjust = hjust),
                angle = pull(data_ex_add_angle %>% filter(!name %in% c("Cis", "Ctrl")), angle)
              ) +
              coord_polar(start = 1.17 * pi) + 
              scale_y_continuous(limits = c(-50, 30)) + 
              theme_void()
  
            p4<-ggdraw()+draw_plot(p3, x=0,y=0,  hjust =0,vjust = 0)+
              draw_plot(p2,x = 0.5,y = 0.5,width =0.83,height = 0.83,
                        hjust = 0.5,vjust = 0.5)+
              draw_plot(p5,x = 0.5,y = 0.5,width =0.38,height = 0.38,
                        hjust = 0.5,vjust = 0.5)
        
            print(p4)
            })
            
             shinyApp(ui = ui, server = server)}
        
    

 
