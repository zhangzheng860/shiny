library(shiny)
library(colourpicker)
library(tidyverse)
library(readxl)
library(tidygraph)
library(ggraph)
library(igraph)
library(ggnewscale)
library(patchwork)
library(cowplot)
library(shinyBS)
# 用户界面
ui <- navbarPage(
  title = "生物环网数据可视化分析平台",
  
  # 绘图页面
  tabPanel(
    title = "绘图",
    icon = icon("palette"),
    sidebarLayout(
      sidebarPanel(width=3,
                   h5('示例数据下载'),
                   downloadButton("Download", "data_ex.csv"),
                   
                   fileInput("data_ex", "上传数据.csv/xlsx", accept = c('.csv','.xlsx'))
                   ,
                   bsCollapsePanel(
                     title = tagList("环形网络图设置"),
                     value = "network_panel",
                     numericInput("seed", "随机种子", value = 123),
                     sliderInput('center_size','中心节点大小',0,10,10),
                     uiOutput('dynamic_color_controls'),
                     sliderInput('edge_width','连接线宽度',0,10,10),
                     sliderInput('normal_size','普通节点大小',0,10,10),
                     sliderInput("transparency",label = "节点透明度",min = 0,max = 1,value = 1),
                     selectInput("shape",label = "节点形状",choices = list("circle"=21,"square"=22,"lozenge"=23,"Regular triangle"=24,"Inverted triangle"=25))),
                   
                   bsCollapsePanel(
                     title = tagList( "环形热力图设置"),
                     sliderInput('tile1_height','内环高度',0,1,1),
                     uiOutput("tile1_color_controls"),
                     sliderInput('tile2_height','中环高度',0,1,1),
                     colourInput("tile21_color", "中环低值色", value = "white"),
                     colourInput("tile22_color", "中环高值色", value = "red"),
                     
                     sliderInput('tile3_height','外环高度',0,1,1),
                     colourInput("tile31_color", "外环低值色", value = "white"),
                     colourInput("tile32_color", "外环高值色", value = "green")),
                   bsCollapsePanel(title = tagList( "环形柱状图设置"),
                                   colourInput("bar_color", "柱状图Cis颜色", value = "gray"),
                                   sliderInput('label_size','标签大小',0,10,10),
                                   colourInput("label_color", "标签颜色", value = "black"),
                                   selectInput("writetype",label = "字体类型",choices = list( "serif", "SimHei", "mono","Arial","Times New Roman","SimSun"))),
                   
                   actionButton("render_btn", "确认")
      ),
      mainPanel(
        width = 9,
        downloadButton('downloadPlot','download pdf-file'),
        plotOutput("main_plot")
      )
    )
  ),
  
  # 注释页面
  tabPanel(
    title = "帮助",
    icon = icon("book"),
    fluidPage(
      tags$div(
        style = "max-width: 1200px; margin: 0 auto; padding: 20px;",
        h2("平台使用指南", style = "color: #2c3e50; border-bottom: 2px solid #3498db;"),
        
      ),
      # 数据准备说明
      tags$div(
        style = "background-color: #f8f9fa; padding: 20px; border-radius: 10px; margin-top: 20px;",
        h3("数据格式要求", icon("database")),
        tags$ul(
          tags$li("列名： ", tags$code("一共六列")),
          tags$li("第一列 ：", tags$code("数字序号")),
          tags$li("第二列 ：", tags$code("字符串（物种-基因名称、物种—基因名称）")),
          tags$li("第三列 ：", tags$code("字符（每个字母代表一项指标,指标未知用空值表示），用于绘制环形热力图的内环")),
          tags$li("第四列 ：", tags$code("数值，用于绘制环形热力图的中环")),
          tags$li("第五列 ：", tags$code("数值，用于绘制环形热力图的外环")),
          tags$li("第六列： ", tags$code("数值，用于绘制环形柱状图")),
          tags$li("示例数据格式 ", tags$code("csv")),
          tags$li("数据上传格式： ", tags$code("csv/xlsx")),
          tags$li("文件下载格式 ", tags$code("PDF"))
        )
      ),
      tags$div(
        style = "margin-top: 30px;",
        h3("参数调节说明", icon("sliders")),
        tags$div(
          style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px; margin-top: 15px;"), 
      ),
      # 网络参数
      tags$div(
        style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
        h4("环形网络图参数", style = "color: #e74c3c;"),
        tags$ul(
          tags$li(tags$strong("中心节点大小："), "控制两个中心节点的显示尺寸"),
          tags$li(tags$strong("节点颜色："), "调节中心节点与其相连的普通节点的颜色"),
          tags$li(tags$strong("连接线宽度："), "调节普通节点和中心节点连线的线条的粗细"),
          tags$li(tags$strong("普通节点大小："), "控制普通节点的显示尺寸"),
          tags$li(tags$strong("节点透明度："), "控制中心节点和普通节点的透明程度"),
          tags$li(tags$strong("节点形状："), "调节中心节点和普通节点的形状（圆形、方形、菱形、正三角、倒三角）")
        )
      ),
      #环形图参数
      tags$div(
        style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
        h4("环形热力图参数", style = "color: #e74c3c;"),
        tags$ul(
          tags$li(tags$strong("内环高度："), "调节内环图的宽度"),
          tags$li(tags$strong("内环类型颜色："), "调节第三列中各个指标的颜色"),
          tags$li(tags$strong("中环高度："), "调节中环的宽度"),
          tags$li(tags$strong("中环低值色："), "调节第四列的低值的颜色"),
          tags$li(tags$strong("中环高值色："), "调节第四列的高值的颜色"),
          tags$li(tags$strong("外环高度："), "调节外环的宽度"),
          tags$li(tags$strong("外环低值色："), "调节第五列的低值的颜色"),
          tags$li(tags$strong("外环高值色："), "调节第五列的高值的颜色")
        )
      ),
      tags$div(
        style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
        h4("环形柱状图参数", style = "color: #e74c3c;"),
        tags$ul(
          tags$li(tags$strong("柱状图颜色："), "调节环形柱状图里填充的颜色类型"),
          tags$li(tags$strong("标签大小："), "调节环形柱状图文字标签的字体的大小"),
          tags$li(tags$strong("标签颜色："), "调节环形柱状图文字标签的字体的颜色"),
          tags$li(tags$strong("字体类型："), "调节环形柱状图文字标签的字体类型（serif, SimHei, mono,Arial,Times New Roman,SimSun）")
        )
      )
    )
  ))
server <- function(input, output) {
  output$dynamic_color_controls <- renderUI({
    req(input$data_ex)
    data_ex <- tryCatch({
      ext <- tolower(tools::file_ext(input$data_ex$datapath))
      if (ext == "csv") {
        read.csv(input$data_ex$datapath, sep = ',', header = TRUE, na.strings = "")
      } else if (ext == "xlsx") {
        readxl::read_excel(input$data_ex$datapath)
      }
    }, error = function(e) {
      return(NULL)
    })
    colnames(data_ex) <- c("id", "name", "data1", "data2", "data3", "data4")
    if (is.null(data_ex)) {
      showNotification("无法读取文件，请确保格式正确", type = "error")
      return(NULL)
    }
    if (!"name" %in% colnames(data_ex)) {
      showNotification("数据需要包含'name'列", type = "error")
      return(NULL)
    }
    
    data_ex$name <- as.character(data_ex$name)
    center_types <- unique(str_extract(data_ex$name, "^[A-Za-z]+"))
    center_types <- center_types[!is.na(center_types)]
    
    if (length(center_types) == 0) {
      showNotification("无法从'name'列提取类型前缀", type = "error")
      return(NULL)
    }
    tagList(
      h4("节点颜色设置"),
      lapply(center_types, function(type) {
        colourInput(
          inputId = paste0("color_", make.names(type)),
          label = paste(type,"节点颜色"),
          value = scales::hue_pal()(length(center_types))[match(type, center_types)]
        )
      })
    )
  })
  
  output$tile1_color_controls <- renderUI({
    req(input$data_ex)
    
    data_ex <- tryCatch({
      ext <- tolower(tools::file_ext(input$data_ex$datapath))
      if (ext == "csv") {
        read.csv(input$data_ex$datapath, sep = ',', header = TRUE, na.strings = "")
      } else if (ext == "xlsx") {
        readxl::read_excel(input$data_ex$datapath)
      }
    }, error = function(e) {
      return(NULL)
    })
    
    if (!is.null(data_ex)) {
      colnames(data_ex) <- c("id", "name", "data1", "data2", "data3", "data4")
      categories <- unique(na.omit(data_ex$data1))
      categories <- categories[categories != ""]
      
      if (length(categories) == 0) return(NULL)
      tagList(
        lapply(categories, function(cat) {
          cat_id <- make.names(cat)  
          colourInput(
            inputId = paste0("tile1_color_", cat_id),
            label = paste(cat, "类型 ", " 颜色"),
            value = scales::hue_pal()(length(categories))[match(cat, categories)]
          )
        })
      )
    }
  })
  
  wrap_by_words <- function(text, words_per_line = 3) {
    words <- unlist(str_split(text, "\\s+"))
    groups <- (seq_along(words) - 1) %/% words_per_line + 1
    lines <- map_chr(
      split(words, groups),
      ~ paste(.x, collapse = " ")
    )
    paste(lines, collapse = "\n")
  }
  
  wrap_by_chars <- function(text, chars_per_line = 3) {
    chars <- unlist(str_split(text, ""))
    groups <- (seq_along(chars) - 1) %/% chars_per_line + 1
    lines <- map_chr(
      split(chars, groups),
      ~ paste(.x, collapse = "")
    )
    paste(lines, collapse = "\n")
  }
  
  smart_wrap <- function(text, words_en = 3, chars_zh = 3) {
    zh_groups <- gregexpr("[\u4e00-\u9fa5]+", text, perl = TRUE)
    zh_pos <- unlist(zh_groups)
    
    if (length(zh_pos) == 1 && zh_pos == -1) {
      return(wrap_by_words(text, words_en))
    } else {
      segs <- character(0)
      start <- 1
      
      for (i in seq_along(zh_pos)) {
        zh_start <- zh_pos[i]
        zh_len <- attr(zh_groups[[1]], "match.length")[i]
        
        if (zh_start > start) {
          segs <- c(segs, substr(text, start, zh_start - 1))
        }
        
        zh_text <- substr(text, zh_start, zh_start + zh_len - 1)
        segs <- c(segs, wrap_by_chars(zh_text, chars_zh))
        
        start <- zh_start + zh_len
      }
      
      if (start <= nchar(text)) {
        segs <- c(segs, substr(text, start, nchar(text)))
      }
      
      return(paste(segs, collapse = ""))
    }
  }
  
  observeEvent(input$render_btn, {
    req(input$data_ex)
    data_ex <- input$data_ex$datapath
    
    read_multi_format <- function(file_path) {
      ext <- tolower(tools::file_ext(file_path))
      
      if (ext == "csv") {
        data <- read.csv(file_path, sep = ',', header = T, na.strings = "")
      } else if (ext == "xlsx") {
        data <- readxl::read_excel(file_path)
      } else {
        stop("不支持的文件格式，仅支持 .csv 和 .xlsx 文件！")
      }
      
      return(data)
    }
    
    data_ex <- read_multi_format(data_ex)
    original_colnames <- colnames(data_ex)
    colnames(data_ex) <- c("id", "name", "data1", "data2", "data3", "data4")
    
    # 数据处理
    data_ex$group <- sapply(strsplit(data_ex$name, "-|—"), function(x) x[1])
    data_ex$gene_num <- as.numeric(str_extract(data_ex$name, "\\d+$"))
    data_ex <- data_ex[order(data_ex$group, data_ex$gene_num), ]
    data_ex$id <- 1:nrow(data_ex)
    data_ex$group <- NULL
    data_ex$gene_num <- NULL
    data_ex$type <- str_extract(data_ex$name, '^[A-Za-z]+')
    data_ex$type <- ifelse(is.na(data_ex$type), 'other', data_ex$type)
    
    # 创建网络数据
    edges <- data.frame(from = data_ex$type, to = data_ex$name)
    new_row <- data.frame(name = c(unique(data_ex$type)), type = c(unique(data_ex$type)))
    nodes <- data.frame(name = data_ex$name, type = data_ex$type)
    nodes <- rbind(nodes, new_row)
    
    set.seed(input$seed)
    output$main_plot <- renderPlot({ 
      center_types <- unique(data_ex$type)
      
      # 计算统一的角度 - 基于数据顺序
      n_total <- nrow(data_ex)
      # 为每个数据点计算基础角度 (从顶部开始，逆时针)
      data_ex$base_angle <- 2 * pi * (data_ex$id - 1) / n_total
      
      # 统一的起始偏移量（与p2和p3保持一致）
      start_offset <- 1.17 * pi
      
      # 转换为最终角度（应用起始偏移）
      data_ex$final_angle <- data_ex$base_angle + start_offset
      
      # 为每个中心节点计算平均角度（基于其子节点）
      center_angle_map <- data_ex %>%
        group_by(type) %>%
        summarise(avg_angle = mean(final_angle))
      
      # 为每个节点分配角度和位置
      node_angles <- nodes %>%
        left_join(
          bind_rows(
            data_ex %>% select(name, final_angle),
            center_angle_map %>% rename(name = type, final_angle = avg_angle)
          ),
          by = "name"
        ) %>%
        mutate(
          radius = ifelse(name %in% center_types, 0.5, 2),
          x = radius * cos(final_angle+0.53*pi),
          y = radius * sin(final_angle+0.53*pi),
          is_center = name %in% center_types,
          node_size = ifelse(is_center, input$center_size * 2, input$normal_size * 0.7)
        )
      
      # 创建graph对象
      graph <- graph_from_data_frame(
        edges,
        vertices = node_angles,
        directed = FALSE
      ) %>%  
        as_tbl_graph() %>%
        activate(edges) %>%
        mutate(
          # 添加边的source_type信息
          source_type = .N()$type[from]
        ) %>%
        activate(nodes) %>%
        mutate(
          Popularity = centrality_degree(mode = "all")
        )
      
      color_values <- sapply(center_types, function(type) {
        color_input_id <- paste0("color_", make.names(type))
        
        if (!is.null(input[[color_input_id]])) {
          return(input[[color_input_id]])
        } else {
          return(scales::hue_pal()(length(center_types))[match(type, center_types)])
        }
      })
      names(color_values) <- center_types
      
      # 绘制网络图 - 使用我们计算好的节点位置
      p5 <- ggraph(graph, layout = "manual", x = V(graph)$x, y = V(graph)$y) +
        geom_edge_diagonal(aes(color = source_type), alpha = 0.8, width = input$edge_width * 0.1) +
        geom_node_point(aes(size = node_size, color = type, fill = type), 
                        shape = as.numeric(input$shape), alpha = input$transparency) +
        geom_node_text(
          data = filter(as.data.frame(graph), is_center),
          aes(label = name),
          color = "black",
          size = input$center_size,
          fontface = "bold"
        ) +
        scale_size_identity() +
        scale_color_manual(values = color_values) +
        scale_fill_manual(values = color_values) +
        scale_edge_color_manual(values = color_values) +
        coord_fixed() +
        theme_void() +
        theme(legend.position = 'none')
      
      # 处理扩展数据
      data_ex <- data_ex %>% 
        mutate(id = as.numeric(id))
      
      # 为热力图和柱状图准备统一的角度数据
      data_ex_for_plots <- data_ex %>%
        mutate(
          # 使用相同的角度计算
          angle_for_label = 90-(id - 0.5) * 360 / n_total,  # 转换为角度
          hjust = ifelse(angle_for_label > 90, 0, 1),
          # 调整标签角度使其可读
          label_angle = ifelse(angle_for_label > 90, angle_for_label + 150, angle_for_label-25)
        ) %>%
        arrange(id)
      
      # 获取tile1颜色设置
      tile1_categories <- unique(na.omit(data_ex$data1))
      tile1_categories <- tile1_categories[tile1_categories != ""]
      
      if (length(tile1_categories) > 0) {
        tile1_colors <- sapply(tile1_categories, function(cat) {
          cat_id <- make.names(cat)
          input_id <- paste0("tile1_color_", cat_id)
          
          if (!is.null(input[[input_id]])) {
            return(input[[input_id]])
          } else {
            return(scales::hue_pal()(length(tile1_categories))[match(cat, tile1_categories)])
          }
        })
        names(tile1_colors) <- tile1_categories
      } else {
        tile1_colors <- c()
      }
      
      # 绘制环形热力图
      data_ex <- data_ex %>% 
        mutate(data2 = as.numeric(factor(data2)))
      
      p2 <- ggplot(data_ex_for_plots) +
        geom_tile(aes(x = id, y = 1.5, fill = data3), height = input$tile3_height * 0.25) +
        scale_fill_gradient(low = input$tile31_color, high = input$tile32_color, 
                            na.value = "transparent", guide = guide_legend(order = 3),
                            name = smart_wrap(original_colnames[5], 2)) +
        ggnewscale::new_scale_fill() +
        geom_tile(aes(x = id, y = 1.25, fill = data2), height = input$tile2_height * 0.25) +
        scale_fill_gradient(low = input$tile21_color, high = input$tile22_color,
                            na.value = "transparent", guide = guide_legend(order = 2),
                            name = smart_wrap(original_colnames[4], 2)) +
        ggnewscale::new_scale_fill() 
      
      if (length(tile1_colors) > 0) {
        p2 <- p2 +
          geom_tile(aes(x = id, y = 1, fill = data1), height = input$tile1_height * 0.25,
                    na.rm = TRUE) +
          scale_fill_manual(values = tile1_colors, 
                            na.value = "white", guide = guide_legend(order = 1),
                            name = smart_wrap(original_colnames[3], 2))
      } else {
        p2 <- p2 +
          geom_tile(aes(x = id, y = 1, fill = data1), height = input$tile1_height * 0.25,
                    na.rm = TRUE) +
          scale_fill_discrete(na.value = "white", guide = guide_legend(order = 1),
                              name = smart_wrap(original_colnames[3], 2))
      }
      
      p2 <- p2 +
        coord_polar(start = start_offset) +
        scale_y_continuous(limits = c(-1, 2.5)) +
        theme_void() +
        theme(legend.position = c(1.11, 0.5),
              legend.justification = c(1.11, 0.5))
      
      # 绘制柱状图
      p3 <- ggplot(data_ex_for_plots) + 
        geom_bar(aes(x = id, y = data4), stat = "identity", fill = input$bar_color, alpha = 0.5)  +
        theme_minimal()  +
        theme(
          legend.position = "none",
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(rep(-1, 4), "cm") 
        ) +
        geom_text(
          aes(x = id, y = data4 + 10, label = name, hjust = hjust),
          na.rm = TRUE, size = 0.45 * input$label_size, inherit.aes = FALSE,
          angle = data_ex_for_plots$label_angle,
          color = input$label_color, family = input$writetype
        ) +
        coord_polar(start = start_offset) + 
        scale_y_continuous(limits = c(-50, 30)) + 
        theme_void() +
        theme(legend.position = "none")
      
      # 组合图形
      p4 <<- ggdraw() +
        draw_plot(p3, x = 0.5, y = 0.5, hjust = 0.5, vjust = 0.5, width = 1, height = 1) +
        draw_plot(p2, x = 0.5, y = 0.5, width = 0.83, height = 0.83, hjust = 0.5, vjust = 0.5) +
        draw_plot(p5, x = 0.5, y = 0.5, width = 0.37, height = 0.37, hjust = 0.5, vjust = 0.5)
      
      p4
    }, height = 750, width = 750)
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() { 'downloaded_plot.pdf' },
    content = function(file) {
      showtext::showtext_auto()
      pdf(file, width = 10, height = 10, family = 'GB1')
      print(p4)
      dev.off()
      showtext::showtext_auto(FALSE)
    },
    contentType = "application/pdf"
  )
  
  observe({ 
    data_ex <- data.frame(
      id = 1:51,
      name = c(
        paste0("Cis-Node_", 1:42), 
        paste0("Ctrl-Node_", 1:9)
      ),
      data1 = c(
        "d","c","","","c","a","","b","c","",
        "c","c","b","d","c","","c","","b","b",
        "a","b","d","b","d","c","d","a","","c",
        "d","a","d","d","a","a","d","d","a","d",
        "","c",
        "","c","b","c","d","c","b","d","c"
      ),
      data2 = c(
        9,15,2,10,1,10,2,10,14,6,7,15,19,8,15,14,4,16,18,11,
        7,5,20,7,7,5,14,3,9,10,5,19,5,9,18,15,7,5,19,9,
        7,16,
        3,10,15,19,19,8,6,1,9
      ),
      data3 = c(
        12,9,4,5,6,3,1,11,5,19,16,14,8,10,12,15,16,16,1,3,
        4,11,10,5,15,2,6,10,1,16,11,11,3,1,8,13,8,3,3,1,
        7,10,
        20,11,2,3,12,13,12,1,6
      ),
      data4 = c(
        2,11,4,5,1,6,8,19,2,11,11,7,18,10,3,17,15,18,14,13,
        13,3,3,2,20,18,5,14,5,1,9,2,10,18,2,9,6,10,12,18,
        11,17,
        20,10,14,3,7,6,2,10,7
      ),
      stringsAsFactors = FALSE
    )
    rownames(data_ex) <- NULL 
    
    output$Download <- downloadHandler(
      filename = function() {
        paste("data_ex.csv")
      },
      content = function(file) {
        write.csv(data_ex, file = file,
                  row.names = F,
                  na = "",           
                  quote = TRUE) 
      }, 
      contentType = 'text/csv'
    )
  })
}

 
shinyApp(ui = ui, server = server)
