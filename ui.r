library(shiny)
library(shinyjs)
library(plotly)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
    tags$style(HTML("
      body{background-color:#f0f4f7;font-family:Arial,sans-serif;margin:0;padding:0;}
      .title{font-family:'Helvetica Neue',Helvetica,Arial,sans-serif;font-weight:300;color:#007bff;margin-bottom:20px;text-align:center;padding:20px;background-color:#ffffff;box-shadow:0 2px 4px rgba(0,0,0,0.1);position:relative;}
      .title:before{content:'\\f0f1';font-family:'Font Awesome 5 Free';font-weight:900;position:absolute;left:10px;top:10px;font-size:40px;color:#007bff;}
      .chat-container{display:flex;flex-direction:column;height:65vh;max-width:600px;margin:40px auto;border:1px solid #ddd;border-radius:10px;background-color:#ffffff;padding:20px;box-shadow:0 4px 8px rgba(0,0,0,0.1);}
      .chat-history{flex-grow:1;overflow-y:auto;display:flex;flex-direction:column;gap:10px;padding-right:10px;}
      .chat-history::-webkit-scrollbar{width:8px;}
      .chat-history::-webkit-scrollbar-thumb{background-color:#007bff;border-radius:4px;}
      .chat-history::-webkit-scrollbar-track{background:#f1f1f1;}
      .container{border:2px solid #dedede;background-color:#e1f5fe;border-radius:5px;padding:10px;margin:10px 0;max-width:80%;word-wrap:break-word;box-shadow:0 2px 4px rgba(0,0,0,0.1);position:relative;}
      .darker{border-color:#ccc;background-color:#c8e6c9;}
      .container::after{content:'';clear:both;display:table;}
      .input-container{display:flex;align-items:center;border-top:1px solid #ddd;padding-top:10px;padding-bottom:10px;background-color:#ffffff;border-radius:0 0 10px 10px;}
      .send-button,.restart-button{background-color:#007bff;border-color:#007bff;color:#fff;padding:8px 12px;border-radius:5px;font-size:14px;margin-left:10px;cursor:pointer;transition:background-color 0.3s,box-shadow 0.3s;}
      .send-button i,.restart-button i{font-size:16px;}
      .send-button:hover,.restart-button:hover{background-color:#0056b3;border-color:#004085;box-shadow:0 4px 8px rgba(0,0,0,0.2);}
      .chat-input{flex-grow:1;margin-right:10px;padding:10px;font-size:16px;border:1px solid #ddd;border-radius:5px;transition:border-color 0.3s,box-shadow 0.3s;background-color:#fff;}
      .chat-input:focus{border-color:#007bff;box-shadow:0 0 5px rgba(0,123,255,0.5);outline:none;}
    ")),
    tags$script(HTML("
      var shouldScroll = true;
      function scrollToBottom() {if (shouldScroll) {
          var chatHistory = document.getElementsByClassName('chat-history')[0];
          chatHistory.scrollTop = chatHistory.scrollHeight; } }
      Shiny.addCustomMessageHandler('scrollToBottom', function(message) { scrollToBottom(); });
      $(document).on('shiny:value', '.chat-history', function(event) { scrollToBottom(); });
      $(document).ready(function() {
        var chatHistory = document.getElementsByClassName('chat-history')[0];
        chatHistory.addEventListener('scroll', function() {
          var isAtBottom = chatHistory.scrollHeight - chatHistory.scrollTop === chatHistory.clientHeight;
          shouldScroll = isAtBottom; }); }); 
    "))
  ),
  titlePanel(div(class = "title", "Ms. Martha, Fuzzy Medical Chatbot")),
  tabsetPanel(
    tabPanel("Chatbot",
             div(class = "chat-container",
                 div(class = "chat-history", uiOutput("chatHistory")),
                 div(class = "input-container",
                     tags$input(id = "userMessage", type = "text", class = "chat-input", placeholder = "Message Ms. Martha"),
                     actionButton("sendButton", tags$i(class = "fas fa-paper-plane"), class = "send-button"),
                     actionButton("restartButton", tags$i(class = "fas fa-redo"), class = "restart-button")
                 )
             )
    ),
    tabPanel("Fuzzy Inference",
             fluidPage(br(),
                       fluidRow(column(width = 1),
                                column(width = 5, plotlyOutput('g.plot'), p(htmlOutput('rules_fired'), class = 'text-center text-info')),
                                column(width = 5, plotOutput('plot')),
                                column(width = 1) ), br(),
                       fluidRow(
                         column(width = 1),
                         column(width = 5,  plotOutput('temp_plot', height = "300px"),  p(textOutput('input_temp'), class = "text-center text-info") ),
                         column(width = 5,  plotOutput('head_plot', height = "300px"),  p(textOutput('input_head'), class = "text-center text-info") ),
                         column(width = 1) ), br(),
                       fluidRow(
                         column(width = 1),
                         column(width = 5, plotOutput('age_plot', height = "300px"),  p(textOutput('input_age'), class = "text-center text-info") ),
                         column(width = 5, plotOutput('urgency_plot', height = "300px")),
                         column(width = 1) ), br()
             )
    )
  )
)
