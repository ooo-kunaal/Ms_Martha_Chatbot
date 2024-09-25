library(shiny)
library(httr)
library(jsonlite)
library(stringr)
library(plotly)

# Function to validate UK postcode
is_valid_postcode <- function(postcode) {
  grepl("^([A-Z]{1,2}[0-9][0-9A-Z]? ?[0-9][A-Z]{2})$", postcode, ignore.case = TRUE)
}

# Function to get coordinates from postcode using OpenStreetMap Nominatim API
get_coordinates <- function(postcode) {
  encoded_postcode <- URLencode(postcode)
  url <- paste0("https://nominatim.openstreetmap.org/search?q=", encoded_postcode, "&format=json&addressdetails=1&limit=1")
  response <- content(GET(url), "text", encoding = "UTF-8")
  response_json <- fromJSON(response, simplifyDataFrame = FALSE)
  if (length(response_json) > 0) {
    lat <- as.numeric(response_json[[1]]$lat)
    lon <- as.numeric(response_json[[1]]$lon)
    return(c(lat, lon))
  } else {
    return(NULL)
  }
}

# Function to get nearest hospitals and doctors using Overpass API
get_nearest_emergency_facilities <- function(lat, lon, postcode) {
  query <- paste0('[out:json];(',
                  'node[amenity=hospital](around:5000,', lat, ',', lon, ');',
                  'node[amenity=doctors](around:5000,', lat, ',', lon, ');',
                  ');out body;>;out skel qt;')
  url <- paste0("http://overpass-api.de/api/interpreter?data=", URLencode(query))
  response <- content(GET(url), "text", encoding = "UTF-8")
  response_json <- fromJSON(response, simplifyDataFrame = FALSE)
  if (length(response_json$elements) > 0) {
    facilities <- lapply(response_json$elements, function(element) {
      name <- element$tags$name
      address <- paste0(
        ifelse(is.null(element$tags$`addr:housenumber`), "", paste0(element$tags$`addr:housenumber`, " ")),
        ifelse(is.null(element$tags$`addr:street`), "", paste0(element$tags$`addr:street`, ", ")),
        ifelse(is.null(element$tags$`addr:city`), "", paste0(element$tags$`addr:city`, ", ")),
        ifelse(is.null(element$tags$`addr:postcode`), "", element$tags$`addr:postcode`)
      )
      fac_lat <- element$lat
      fac_lon <- element$lon
      google_maps_link <- paste0("https://www.google.com/maps/dir/?api=1&origin=", URLencode(postcode), "&destination=", fac_lat, ",", fac_lon)
      list(name = name, address = address, link = google_maps_link)
    })
    return(facilities[1:min(5, length(facilities))])
  } else {
    return(list(list(name = "No nearby hospitals or doctors found.")))
  }
}

# Fuzzy logic functions
temperature_domain = seq(30, 50, 0.01)
headache_domain = seq(0, 10, 0.1)
age_domain = seq(0, 130, 0.1)
urgency_domain = seq(0, 100, 0.1)

temp.too.low  = function(x){ ifelse(x <= 35,  1, ifelse(x <= 36,  exp(-0.5*(x-35)^2/0.3^2),  0)) }
temp.normal   = function(x){ ifelse(x <= 35,  0, ifelse(x <  38,  exp(-0.5*(x-36.5)^4/0.9^4),0)) }
temp.high     = function(x){ ifelse(x <= 37,  0, ifelse(x <= 40,  exp(-0.5*(x-38.5)^4/0.9^4),  0)) }
temp.too.high = function(x){ ifelse(x <= 38,  0, ifelse(x <= 40,  exp(-0.5*(x-40)^2/0.3^2),  1)) }

head.mild     = function(x){ ifelse(x <= 3, 1, ifelse(x <= 4, exp(-0.5*(x-3)^2/0.3^2),  0)) }
head.moderate = function(x){ ifelse(x <= 3, 0, ifelse(x < 6, exp(-0.5*(x-4.5)^4/0.9^4),0)) }
head.severe   = function(x){ ifelse(x <= 5, 0, ifelse(x <= 6, exp(-0.5*(x-6)^2/0.3^2),  1)) }

trapezoid = function(x, a, b, c, d){ ifelse(x <= a, 0, ifelse(x <= b, (x-a)/(b-a), ifelse(x <= c, 1, ifelse(x <= d, (d-x)/(d-c), 0)))) }
age.infant = function(x){ ifelse(x <=  5, 1, ifelse(x <= 8, (8-x)/3, 0)) }
age.child  = function(x){ trapezoid(x, 5, 8, 16, 18) }
age.adult  = function(x){ trapezoid(x, 16, 18, 63, 65) }
age.old    = function(x){ ifelse(x <= 60, 0, ifelse(x <= 65, (x-60)/5, 1)) }

not.urgent  = function(x){ ifelse(x < 33.3, 1, ifelse(x < 50, exp(-0.5*(x-33.3)^2/3^2), 0)) }
urgent      = function(x){ ifelse(x < 33.33, 0, ifelse(x < 66.67, exp(-0.5*(x-50)^4/9^4), 0)) }
very.urgent = function(x){ ifelse(x < 50, 0, ifelse(x < 66.67, exp(-0.5*(x-66.67)^2/3^2), 1)) }

fuzzy.inf <- function(age, temp, head) {
  age_inf <- age.infant(age)
  age_chd <- age.child(age)
  age_adl <- age.adult(age)
  age_old <- age.old(age)
  
  temp_tl <- temp.too.low(temp)
  temp_n  <- temp.normal(temp)
  temp_h  <- temp.high(temp)
  temp_th <- temp.too.high(temp)
  
  head_m  <- head.mild(head)
  head_mod <- head.moderate(head)
  head_s  <- head.severe(head)
  
  # Define the rules and keep track of which rules fired
  rules <- list()
  rules_text <- list()
  
  rule1 <- pmin(age_inf, temp_n, head_m)
  rules <- c(rules, list(rule1))
  rules_text <- c(rules_text, "Rule 1: If age is infant and temperature is normal and headache is mild THEN urgency is low")
  
  rule2 <- pmin(age_inf, temp_n, head_mod)
  rules <- c(rules, list(rule2))
  rules_text <- c(rules_text, "Rule 2: If age is infant and temperature is normal and headache is moderate THEN urgency is medium")
  
  rule3 <- pmin(age_inf, temp_n, head_s)
  rules <- c(rules, list(rule3))
  rules_text <- c(rules_text, "Rule 3: If age is infant and temperature is normal and headache is severe THEN urgency is high")
  
  rule4 <- pmin(age_inf, temp_h)
  rules <- c(rules, list(rule4))
  rules_text <- c(rules_text, "Rule 4: If age is infant and temperature is high THEN urgency is high")
  
  rule5 <- pmin(age_chd, temp_n, head_s)
  rules <- c(rules, list(rule5))
  rules_text <- c(rules_text, "Rule 5: If age is child and temperature is normal and headache is severe THEN urgency is high")
  
  rule6 <- pmin(age_chd, temp_h, head_s)
  rules <- c(rules, list(rule6))
  rules_text <- c(rules_text, "Rule 6: If age is child and temperature is high and headache is severe THEN urgency is high")
  
  rule7 <- pmin(age_chd, temp_h, head_mod)
  rules <- c(rules, list(rule7))
  rules_text <- c(rules_text, "Rule 7: If age is child and temperature is high and headache is moderate THEN urgency is medium")
  
  rule8 <- pmin(age_chd, temp_h, head_m)
  rules <- c(rules, list(rule8))
  rules_text <- c(rules_text, "Rule 8: If age is child and temperature is high and headache is mild THEN urgency is medium")
  
  rule9 <- pmin(age_adl, temp_n, head_s)
  rules <- c(rules, list(rule9))
  rules_text <- c(rules_text, "Rule 9: If age is adult and temperature is normal and headache is severe THEN urgency is high")
  
  rule10 <- pmin(age_adl, temp_n, head_mod)
  rules <- c(rules, list(rule10))
  rules_text <- c(rules_text, "Rule 10: If age is adult and temperature is normal and headache is moderate THEN urgency is medium")
  
  rule11 <- pmin(age_adl, temp_n, head_m)
  rules <- c(rules, list(rule11))
  rules_text <- c(rules_text, "Rule 11: If age is adult and temperature is normal and headache is mild THEN urgency is low")
  
  rule12 <- pmin(age_adl, temp_h, head_s)
  rules <- c(rules, list(rule12))
  rules_text <- c(rules_text, "Rule 12: If age is adult and temperature is high and headache is severe THEN urgency is high")
  
  rule13 <- pmin(age_adl, temp_h, head_mod)
  rules <- c(rules, list(rule13))
  rules_text <- c(rules_text, "Rule 13: If age is adult and temperature is high and headache is moderate THEN urgency is medium")
  
  rule14 <- pmin(age_adl, temp_h, head_m)
  rules <- c(rules, list(rule14))
  rules_text <- c(rules_text, "Rule 14: If age is adult and temperature is high and headache is mild THEN urgency is medium")
  
  rule15 <- pmin(age_old, temp_h, head_mod)
  rules <- c(rules, list(rule15))
  rules_text <- c(rules_text, "Rule 15: If age is old and temperature is high and headache is moderate THEN urgency is medium")
  
  rule16 <- pmin(age_old, temp_h, head_s)
  rules <- c(rules, list(rule16))
  rules_text <- c(rules_text, "Rule 16: If age is old and temperature is high and headache is severe THEN urgency is high")
  
  rule17 <- pmin(age_old, temp_n, head_s)
  rules <- c(rules, list(rule17))
  rules_text <- c(rules_text, "Rule 17: If age is old and temperature is normal and headache is severe THEN urgency is high")
  
  rule18 <- temp_th
  rules <- c(rules, list(rule18))
  rules_text <- c(rules_text, "Rule 18: If temperature is too high THEN urgency is high")
  
  rule19 <- pmin(age_inf, temp_tl)
  rules <- c(rules, list(rule19))
  rules_text <- c(rules_text, "Rule 19: If age is infant and temperature is too low THEN urgency is high")
  
  rule20 <- pmin(age_chd, temp_tl)
  rules <- c(rules, list(rule20))
  rules_text <- c(rules_text, "Rule 20: If age is child and temperature is too low THEN urgency is high")
  
  rule21 <- pmin(age_adl, temp_tl)
  rules <- c(rules, list(rule21))
  rules_text <- c(rules_text, "Rule 21: If age is adult and temperature is too low THEN urgency is high")
  
  rule22 <- pmin(age_old, temp_tl)
  rules <- c(rules, list(rule22))
  rules_text <- c(rules_text, "Rule 22: If age is old and temperature is too low THEN urgency is high")
  
  # Aggregate the rules
  low_urgency <- pmax(rule1, rule11, rule14)
  medium_urgency <- pmax(rule2, rule5, rule7, rule8, rule10, rule13, rule15, rule17)
  high_urgency <- pmax(rule3, rule4, rule6, rule9, rule12, rule16, rule18, rule19, rule20, rule21, rule22)
  aggregated_urgency <- pmax(pmin(low_urgency, not.urgent(urgency_domain)), pmin(medium_urgency, urgent(urgency_domain)), pmin(high_urgency, very.urgent(urgency_domain)))
  centroid_value = round(sum(urgency_domain * aggregated_urgency) / sum(aggregated_urgency), 3)
  
  # Find the fired rules
  fired_rules <- rules_text[which(sapply(rules, function(rule) max(rule) > 0))]
  fired_rules_paragraph <- paste(fired_rules, collapse = "<br>")
  
  return(list(aggregated_urgency, centroid_value, fired_rules_paragraph))
}

# Function to check if input word is a synonym for the main word using Hugging Face
is_synonym <- function(input_word, main_word) {
  api_key <- Sys.getenv("HUGGINGFACE_API_KEY")
  
  response <- POST(
    url = "https://api-inference.huggingface.co/models/facebook/bart-large-mnli",
    add_headers(
      `Authorization` = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ),
    body = toJSON(list(inputs = paste("This means", main_word), parameters = list(candidate_labels = c(input_word))), auto_unbox = TRUE)
  )
  
  content <- tryCatch({
    fromJSON(content(response, "text", encoding = "UTF-8"))
  }, error = function(e) {
    print(paste("Error fetching data from Hugging Face API:", e))
    return(NULL)
  })
  
  print(paste("API response content:", toString(content)))
  
  if (!is.null(content) && "scores" %in% names(content) && length(content$scores) > 0) {
    entailment_score <- content$scores[1]
    print(paste("Entailment score for", input_word, "and", main_word, ":", entailment_score))
    if (entailment_score > 0.5) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    print("No valid scores found in the API response.")
    return(FALSE)
  }
}

# Function to call Hugging Face NER API and extract names
extract_name_huggingface <- function(message) {
  api_key <- Sys.getenv("HUGGINGFACE_API_KEY")
  
  response <- POST(
    url = "https://api-inference.huggingface.co/models/dbmdz/bert-large-cased-finetuned-conll03-english",
    add_headers(
      `Authorization` = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ),
    body = toJSON(list(inputs = message), auto_unbox = TRUE)
  )
  
  content <- tryCatch({
    fromJSON(content(response, "text", encoding = "UTF-8"))
  }, error = function(e) {
    return(NULL)
  })
  
  name <- if (is.data.frame(content)) {
    names <- content$word[content$entity_group == "PER"]
    if (length(names) > 0) paste(names, collapse = " ") else NA
  } else {
    NA
  }
  
  return(name)
}

# Health tips based on conditions
get_health_tips <- function(age, temp, head) {
  tips <- list()
  if (temp.too.low(temp) > 0.5) {
    tips <- c(tips, "<br><br> Hypothermia Tips:<br> 1. Seek Medical Attention immediately.<br> 2. Blankets to warm yourself, use warm compresses to center of body, neck, chest, and groin. Avoid using direct heat such as hot water, heating pad, or heat lamp.<br> 3. Drink warm (not hot) sweetened, non-alcoholic beverages. Avoid caffeine and alcohol.<br> 4. Do Not Massage or Rub Extremities.<br> 5. Avoid Sudden Movements.")
  }
  if (temp.too.high(temp) > 0.5) {
    tips <- c(tips, "<br><br>Hyperpyrexia Tips:<br> 1. Seek Medical Attention immediately.<br> 2. Move to a Cooler Environment.<br> 3. Remove Excess Clothing to help cool down.<br> 4. Apply cool (not ice-cold) compresses or cloths to the forehead, neck, armpits, and groin. You can also use a sponge or cloth to gently dampen the skin with lukewarm water.<br> 5. Drink plenty of fluids, preferably water or electrolyte solutions, to prevent dehydration. Avoid caffeine and alcohol.")
  }
  if (temp.high(temp) > 0.5 & temp.too.high(temp) <= 0.5) {
    tips <- c(tips, "<br><br>Fever Tips:<br> 1. Drink Plenty of Fluids. Water, herbal teas, clear broths, or electrolyte solutions.<br> 2. Take a lukewarm bath or sponge bath.<br> 3. Cool Compress: Apply a cool, damp washcloth to your forehead, neck, armpits, and groin.<br> 4. Broths and Soups: Chicken soup or vegetable broth can be nourishing and help keep you hydrated.<br> 5. Fever Reducers: Use acetaminophen (Tylenol) or ibuprofen (Advil, Motrin) as directed to reduce fever and alleviate discomfort.")
  }
  if (head.moderate(head) > 0.5) {
    tips <- c(tips, "<br><br>Moderate Headache Tips:<br> 1. Drink Water: Dehydration is a common cause of headaches. Drink plenty of water throughout the day.<br> 2. Eat a Healthy Snack: Low blood sugar can cause headaches. A small, healthy snack like a piece of fruit, yogurt, or nuts might help.<br> 3. Deep Breathing: Practice deep breathing exercises to help relax and reduce stress.<br> 4. Limit Caffeine: While a small amount of caffeine can sometimes help alleviate headaches, too much can cause them. Monitor your intake.<br> 5. Pain Relievers: If the headache persists, you can take over-the-counter pain relievers like acetaminophen (Tylenol), ibuprofen (Advil, Motrin), or aspirin as directed.")
  }
  if (head.severe(head) > 0.5) {
    tips <- c(tips, "<br><br>Severe Headache Tips:<br> 1. Over-the-Counter Pain Medications: Take pain relievers like acetaminophen (Tylenol), ibuprofen (Advil, Motrin), or aspirin as directed. For migraine sufferers, medications like Excedrin (a combination of acetaminophen, aspirin, and caffeine) may be effective. Follow the dosage instructions carefully.<br> 2. Rest in a Dark, Quiet Room: Reduce sensory stimulation by lying down in a dark, quiet room. Close your eyes and try to relax.<br> 3. Stay Hydrated: Drink plenty of water. Dehydration can worsen headaches, so ensure you are adequately hydrated.<br> 4. Massage: Gently massage your temples, neck, and shoulders to help relieve muscle tension that may be contributing to the headache.<br> 5. Medication Overuse: Be cautious with the frequent use of pain relievers, as overuse can lead to rebound headaches.")
  }
  return(tips)
}

# Server logic
server <- function(input, output, session) {
  chatHistory <- reactiveVal(list(
    list(type = "bot", text = "Welcome! I am Ms. Martha, your Fuzzy Medical Chatbot."),
    list(type = "bot", text = "I will ask you a few simple questions and use fuzzy logic to assess how urgently you should seek medical attention. My aim is to provide you with quick and helpful guidance during times of need."),
    list(type = "bot", text = "Let's get started! May I know your name?")
  ))
  
  currentQuestion <- reactiveVal("name")
  userInfo <- reactiveValues(name = NULL, age = NULL, temperature = NULL, headache = NULL, pinCode = NULL)
  
  observeEvent(input$sendButton, {
    user_message <- input$userMessage
    if (user_message != "") {
      history <- chatHistory()
      history <- c(history, list(list(type = "user", text = user_message)))
      
      if (currentQuestion() == "name") {
        extracted_name <- extract_name_huggingface(user_message)
        if (!is.na(extracted_name)) {
          userInfo$name <- extracted_name
          bot_response <- paste0("Nice to meet you, ", extracted_name, "! How old are you?")
          currentQuestion("age")
        } else {
          bot_response <- "I think I missed your name, can you repeat it correctly?"
        }
      } else if (currentQuestion() == "age") {
        ages <- str_extract_all(user_message, "-?\\d+")[[1]]
        months <- str_detect(user_message, "(month|months)")
        if (length(ages) == 1) {
          age <- as.numeric(ages)
          if (months) {
            age <- age / 12
          }
          if (age > 0 && age <= 130) {
            userInfo$age <- age
            bot_response <- paste0("Got it. You are ", age, " years old. What is your temperature?")
            currentQuestion("temperature")
          } else {
            bot_response <- "Please enter a valid age between 0 and 130 years."
          }
        } else {
          bot_response <- "Please enter a valid age between 0 and 130 years."
        }
      } else if (currentQuestion() == "temperature") {
        temperatures <- str_extract_all(user_message, "-?\\d+\\.?\\d*")[[1]]
        temperature_f <- str_detect(tolower(user_message), "f")
        temperature_k <- str_detect(tolower(user_message), "k")
        
        if (length(temperatures) == 1) {
          temperature <- as.numeric(temperatures)
          
          if (temperature_f) {
            temperature <- (temperature - 32) * 5 / 9
          } else if (temperature_k) {
            temperature <- temperature - 273.15
          }
          
          if (temperature >= 30 && temperature <= 50) {
            userInfo$temperature <- temperature
            bot_response <- paste0("Thank you. Your temperature is ", round(temperature, 1), "°C. How severe is your headache on a scale of 1 to 10?")
            currentQuestion("headache")
          } else {
            bot_response <- "It's a bit strange, can you please re-check your temperature and enter a correct value between 30 to 50 degrees Celsius."
          }
        } else {
          bot_response <- "It's a bit strange, can you please re-check your temperature and enter a correct value between 30 to 50 degrees Celsius."
        }
      } else if (currentQuestion() == "headache") {
        headaches <- str_extract_all(user_message, "-?\\d+")[[1]]
        if (length(headaches) == 1) {
          headache <- as.numeric(headaches)
          if (headache >= 0 && headache <= 10) {
            userInfo$headache <- headache
            bot_response <- paste0("Noted! Your headache severity is ", headache, ".")
            history <- c(history, list(list(type = "bot", text = bot_response)))
            
            # Calculate fuzzy logic urgency
            fuzzy_result <- fuzzy.inf(userInfo$age, userInfo$temperature, userInfo$headache)
            centroid_value <- fuzzy_result[[2]]
            
            bot_response1 <- paste0("Based on the information you've provided, the urgency of visiting the hospital is ", centroid_value, " out of 100.")
            history <- c(history, list(list(type = "bot", text = bot_response1)))
            
            bot_response <- "Can you share your pin code? I will provide directions to the nearest hospitals."
            currentQuestion("pinCode")
          } else {
            bot_response <- "Please enter a valid headache severity between 0 and 10."
          }
        } else {
          bot_response <- "Please enter a valid headache severity between 0 and 10."
        }
      } else if (currentQuestion() == "pinCode") {
        pinCode <- user_message
        if (is_valid_postcode(pinCode)) {
          userInfo$pinCode <- pinCode
          coordinates <- get_coordinates(pinCode)
          if (!is.null(coordinates)) {
            lat <- coordinates[1]
            lon <- coordinates[2]
            facilities <- get_nearest_emergency_facilities(lat, lon, pinCode)
            if (!is.null(facilities) && length(facilities) > 0) {
              hospital_list <- paste(sapply(facilities, function(facility) {
                name <- facility$name
                address <- facility$address
                link <- facility$link
                paste0("- ", name, ", ", address, " <a href='", link, "' target='_blank'>Directions</a>")
              }), collapse = "<br>")
              bot_response <- paste("Nearby hospitals:<br>", hospital_list)
              history <- c(history, list(list(type = "bot", text = bot_response)))
              
              bot_response <- "Would you like some home remedies and health tips for intermediate care?"
              currentQuestion("healthTips")
            } else {
              bot_response <- "Thank you for the information. However, I couldn't find any hospitals near your location."
            }
          } else {
            bot_response <- "Sorry, I couldn't retrieve the coordinates for the provided pin code."
          }
        } else {
          bot_response <- "Please enter a valid UK pin code."
        }
      } else if (currentQuestion() == "healthTips") {
        print(paste("User response for health tips:", user_message))
        if (is_synonym(user_message, "yes")) {
          print("User response classified as 'yes'")
          health_tips <- get_health_tips(userInfo$age, userInfo$temperature, userInfo$headache)
          if (length(health_tips) > 0) {
            bot_response <- paste("Here are some health tips for you:<br>", 
                                  paste(health_tips, collapse = " "), "<br><br> Take care! We hope you get well soon.")
          } else {
            bot_response <- "I couldn't find any specific health tips for your condition."
          }
        } else if (is_synonym(user_message, "no")) {
          print("User response classified as 'no'")
          bot_response <- "Alright! Take care and stay healthy!"
        } else {
          print("User response not understood.")
          bot_response <- "I didn't understand that. Please respond with 'yes' or 'no'."
        }
        currentQuestion("done")
      } else {
        bot_response <- "Thank you for the information."
      }
      history <- c(history, list(list(type = "bot", text = bot_response)))
      chatHistory(history)
      
      # If the chat is over, disable input
      if (currentQuestion() == "done") {
        shinyjs::disable("userMessage")
        updateTextInput(session, "userMessage", placeholder = "Restart the chat")
      }
      
      updateTextInput(session, "userMessage", value = "")
      session$sendCustomMessage(type = 'scrollToBottom', message = list())
    }
  })
  
  observeEvent(input$restartButton, {
    session$reload()
  })
  
  output$chatHistory <- renderUI({
    history <- chatHistory()
    if (length(history) > 0) {
      tags$div(
        lapply(history, function(message) {
          message_class <- if (message$type == "bot") "container" else "container darker"
          tags$div(class = message_class, tags$p(HTML(message$text)))
        })
      )
    }
  })
  
  observe({ invalidateLater(100, session)
    session$sendCustomMessage(type = 'scrollToBottom', message = list())
  })
  
  # Fuzzy inference logic
  output$temp_plot = renderPlot({
    req(userInfo$age, userInfo$temperature, userInfo$headache)
    plot(temperature_domain, temp.too.low(temperature_domain),type='l',col='navy',lwd=2,xlim=c(33,42), xlab='Temperature',ylab='Membership Value',main='Temperature')
    lines(temperature_domain, temp.normal(temperature_domain),type='l',col='green',lwd=2)
    lines(temperature_domain, temp.high(temperature_domain),type='l',col='red',lwd=2)
    lines(temperature_domain, temp.too.high(temperature_domain),type='l',col='darkred',lwd=2)
    abline(v=userInfo$temperature, col='darkblue',lty=2)
    legend('right', c('Too Low','Normal','High','Too High','Input'),col=c('navy','green','red','darkred','darkblue'),lty=1,lwd=2,bty='n')
  })
  output$head_plot = renderPlot({
    req(userInfo$age, userInfo$temperature, userInfo$headache)
    plot(headache_domain, head.mild(headache_domain), type='l', xlim = c(0,10), col='navy', xlab='Headache',ylab='',main='Headache',lwd=2)
    lines(headache_domain, head.moderate(headache_domain), type='l', col='green',lwd=2)
    lines(headache_domain, head.severe(headache_domain), type='l', col='red',lwd=2)
    abline(v=userInfo$headache, col='darkblue',lty=2)
    legend('right', c('Mild','Moderate','Severe','Input'),col=c('navy','green','red','darkblue'),lty=1,lwd=2,bty='n')
  })
  output$age_plot = renderPlot({
    req(userInfo$age, userInfo$temperature, userInfo$headache)
    plot(age_domain, age.infant(age_domain),type='l',col='navy',lwd=2,xlim=c(0,100), xlab='Age',ylab='',main='Age')
    lines(age_domain, age.child(age_domain),type='l',col='green',lwd=2)
    lines(age_domain, age.adult(age_domain),type='l',col='red',lwd=2)
    lines(age_domain, age.old(age_domain),type='l',col='darkgreen',lwd=2)
    abline(v=userInfo$age, col='darkblue',lty=2)
    legend('right', c('Infant','Child','Adult','Old','Input'),col=c('navy','green','red','darkred','darkblue'),lty=1,lwd=2,bty='n')
  })
  output$urgency_plot = renderPlot({
    req(userInfo$age, userInfo$temperature, userInfo$headache)
    plot(urgency_domain, not.urgent(urgency_domain), type='l', xlim = c(0,100), col='navy', xlab='x',ylab='Membership Value',main='Urgency',lwd=2)
    lines(urgency_domain, urgent(urgency_domain), type='l', col='green',lwd=2)
    lines(urgency_domain, very.urgent(urgency_domain), type='l', col='red',lwd=2)
    legend('right', c('Not Urgent','Urgent','Very Urgent'),col=c('navy','green','red'),lty=1,lwd=2,bty='n')
  })
  output$plot = renderPlot({
    req(userInfo$age, userInfo$temperature, userInfo$headache)
    a = fuzzy.inf(userInfo$age, userInfo$temperature, userInfo$headache)
    plot(urgency_domain, a[[1]],type='l', col='navy', xlab='Ugrency Output', ylab='Membership Value',main=paste('Fuzzy Urgency Centroid =',a[[2]]),lwd=2, ylim=c(0, 1) )
    abline(v=a[[2]], lty=2, lwd=3, col='red')
    legend('topright',c('Output','Centroid'),lty=2,lwd=3, col=c('navy','red'),bty='n')
  })
  output$g.plot <- renderPlotly({ req(userInfo$age, userInfo$temperature, userInfo$headache)
    a <- fuzzy.inf(userInfo$age, userInfo$temperature, userInfo$headache)
    urgency_value <- a[[2]]
    plot_ly(type = "indicator", mode = "gauge+number", value = urgency_value, title = list(text = "Fuzzy Urgency Centroid", font = list(size = 24)), number = list(font = list(size = 36)),
            gauge = list(axis = list(range = list(0, 100), tickwidth = 2, tickcolor = "darkblue", nticks = 10), bar = list(color = "darkblue", thickness = 0.3), bgcolor = "white", borderwidth = 2, bordercolor = "gray", 
                         steps = list(list(range = c(0, 30), color = 'rgba(144,238,144,1)'), list(range = c(30, 33.3), color = 'rgba(144,238,144,0.5)'),
                                      list(range = c(33.3, 36.7), color = 'rgba(255,255,0,0.5)'), list(range = c(36.7, 50), color = 'rgba(255,255,0,1)'),
                                      list(range = c(50, 60), color = 'rgba(255,165,0,0.5)'), list(range = c(60, 66.7), color = 'rgba(255,69,0,0.5)'),
                                      list(range = c(66.7, 100), color = 'rgba(255,69,0,1)') ), threshold = list(line = list(color = "red", width = 4), thickness = 0.75, value = urgency_value)) ) %>% layout(margin = list(l = 20, r = 20, t = 50, b = 20), paper_bgcolor = "white", font = list(color = "darkblue", family = "Arial"))
  })
  output$rules_fired = renderText({ req(userInfo$age, userInfo$temperature, userInfo$headache)
    fuzzy.inf(userInfo$age, userInfo$temperature, userInfo$headache)[[3]] })
  output$input_temp = renderText({ ifelse(userInfo$temperature <= 35, paste('Temperature input belongs to "low" fuzzy set.'),ifelse(userInfo$temperature <= 36,  paste('Temperature input has membership values for both "low" & "normal" fuzzy sets.'), ifelse(userInfo$temperature <= 37,paste('Temperature input belongs to "normal" fuzzy set.'),ifelse(userInfo$temperature <= 38,paste('Temperature input has membership values for both "normal" & "high" fuzzy sets.' ), paste('Temperature input belongs to "high" fuzzy set.')) ))) })
  output$input_head = renderText({ ifelse(userInfo$headache <= 3,  paste('Headache input belongs to "mild" fuzzy set.'),  ifelse(userInfo$headache <= 4 ,  paste('Headache input has membership values for both "mild" & "moderate" fuzzy sets.'), ifelse(userInfo$headache <=5 , paste('Headache input belongs to "Moderate" fuzzy set.'), ifelse(userInfo$headache <= 6, paste('Headache input has membership values for both "moderate" & "severe" fuzzy sets.' ),paste('Headache input belongs to "severe" fuzzy set.')) ))) })
  output$input_age  = renderText({ ifelse(userInfo$age <= 2,  paste('Age input belongs to "infant" fuzzy set.'), 
                                          ifelse(userInfo$age <= 16 ,  paste('Age input belongs to "child" fuzzy set.'),       
                                                 ifelse(userInfo$age <= 64, paste('Age input belongs to "adult" fuzzy set.'),        
                                                        paste('Age input belongs to "old" fuzzy set.')))) })
  
  
}
