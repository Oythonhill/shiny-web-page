# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("DT")
library(shiny)
library(shinydashboard)

ui = dashboardPage(skin="blue",
  dashboardHeader(title = "用R制作网页——北京租房分析",titleWidth=350),
  dashboardSidebar(
    sidebarMenu(
      menuItem("首页",tabName = "firstpage",icon = icon("dashboard")),
      menuItem("数据介绍",tabName = "intro",icon = icon("dashboard")),
      menuItem("数据分析",icon = icon("th"),
               menuSubItem("租金的分布情况",tabName = "overall",icon = shiny::icon("angle-double-right")),
               menuSubItem("各区的均价情况",tabName = "mean",icon = shiny::icon("angle-double-right"))),
      menuItem("总结", tabName = "end",icon = icon("file-code-o")),
      menuItem("友情链接",icon = icon("file-code-o"),
               menuSubItem("统计与数学学院官网",href = "http://sam.cufe.edu.cn/" ),
               menuSubItem("我的Github",tabName = "git")
    )
  )),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "firstpage",
              column(width=6,offset=3,
                     br(),br(),br(),br(),br(),br(),br(),br(),br(),
                     h1("用R制作网页——北京租房分析",align="center"),
                     br(),
                     br(),
                     h3("组员介绍",align="center"),
                     h4("尹宏宇   庞绪琳",align="center"),
                     h4("2017.10.31",align="center")
                )
                
              ),
              
      tabItem(tabName = "intro",
              h2("数据介绍",align="center"),
              br(),br(),
              fluidRow(
                sidebarPanel(
                  h3("数据来源"),
                  h4("爬取安居客北京地区租房的数据，观察网页发现每一页有60条记录，总共有50页，
                      全部爬取后最后得到的数据大约是3000条记录
                      ，然后我们对数据进行了整理，最后得到的数据如下表所示"),br(),br(),
                  img(src="ad.png"),
                  br(),br()),
                mainPanel(
                     a(img(src="anjuke.png",width=650,height=400),href="https://bj.zu.anjuke.com/fangyuan/x2/")
                    )
                ),
              fluidRow(
                       box(
                         DT::dataTableOutput("data"),width=800))
              ),
      # Second tab content
      tabItem(
        
        tabName = "overall",
        h2("描述性分析"),
        h3("1 总体租金的分布情况"),
        fluidRow(
          box(  plotOutput("plot1",height = 400,width = 500)),
          box(  sliderInput("range","租金范围",min=2000,max=200000,value=20,step=10),
                sliderInput("range2","分组",min=5,max=16,value=1,step=1)
              )
          ),
        h3("2 各城区租金的分布情况"),
        fluidRow(
          box(radioButtons("dist", "城区",
                           list("东城" = "东城",
                                "西城" = "西城","朝阳" = "朝阳",
                                "海淀" = "海淀","昌平"="昌平",
                                "房山"="房山","门头沟"="门头沟",
                                "大兴"="大兴","石景山"="石景山",
                                "丰台"="丰台","密云"="密云",
                                "顺义"="顺义","通州"="通州","怀柔"="怀柔"
                                ))
              ),
          
          box(plotOutput("plot2",height=400,width=500))
          
        ) 
              ),
      
      tabItem(tabName = "mean",
              h2("描述性分析"),
              h3("各城区不同的平均房租价格"),
              box(plotOutput("plot3"),height=420,width=480),
              box(
                h4("过滤掉价格大于8000元的房子后，各城区的房租均价情况如下"),
                plotOutput("plot4"),height=470,width=480)),
      tabItem(tabName = "end",
              br(),br(),br(),br(),br(),br(),br(),br(),
              h2("建议大家去雄安发展,为祖国的建设添砖加瓦",align="center"),
              br(),br(),br(),
              a(h1("雄安欢迎你",align="center"),href="http://opinion.people.com.cn/n1/2017/0412/c1003-29203839.html")),
      tabItem(tabName="git",
              mainPanel(width=800,height=800,
                  br(),br(),br(),br(),br(),
                a(img(src="git.png"),href="https://github.com/Oythonhill"),align="center")
              )
    )
    
  )
)
library(DT)
library(ggplot2)
server = function(input,output){
  load("C:\\Users\\YHY\\Desktop\\statistic_calulation\\mydata.Rdata")
  output$plot1 = renderPlot(
    hist(mydata[mydata$价格<input$range,]$价格,breaks=input$range2,col="dodgerblue4",border="white",
         main="租金的价格分布",xlab="价格",ylab="频数")
    )
  output$plot2 = renderPlot({
    hist(mydata[mydata$城区==input$dist,]$价格,breaks=8,col="tomato3",border="white",
         main="租金的价格分布",xlab="价格",ylab="频数")
  })
  output$plot3 = renderPlot({
    barplot(aggregate(价格~城区,data=data.frame(mydata),mean)$价格,
            names.arg =aggregate(价格~城区,data=data.frame(mydata),mean)$城区,
            border=NA,
            col="dodgerblue3",
            ylab="平均房租价格")
  })
  output$plot4 = renderPlot({
    barplot(aggregate(价格~城区,data=data.frame(mydata[mydata$价格<8000,]),mean)$价格,
            names.arg =aggregate(价格~城区,data=data.frame(mydata),mean)$城区,
            border=NA,
            col="cyan4",
            ylab="平均房租价格")
  })
  output$data=DT::renderDataTable(
    DT::datatable(mydata)
  )
}

shinyApp(ui,server)


