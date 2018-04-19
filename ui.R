library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title.
  headerPanel("Student performance"),
  
  sidebarPanel(
    textInput("ufid", label="UFID (xxxx-xxxx)", value='',placeholder='0000-0000'),
    textInput("email", label="UF email", value='',placeholder='xxxx@ufl.edu'),
    textInput("passwd", label="Password", value='',placeholder='AAAAA'),
    h6("If you have questions regarding this webpage, please contact us: Denis Valle (drvalle@ufl.edu)")
  ),
  
  mainPanel(
    plotOutput('plot1'),
    plotOutput('plot2')
  )
))
