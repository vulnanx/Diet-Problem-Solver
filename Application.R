# PROGRAM BY: Christel C. Mirano
# COURSE AND SECTION: CMSC 150 B1L
# PROJECT IN CMSC 150
# DECEMBER 10, 2024
# PROGRAM DESCRIPTION: This program uses Simplex Method to
#                     maximize and minimize the given systems
#                     of linear equations. 
#-----------------------------------------------------------------------------------------------------------
# if not yet installed
# install.packages("shiny")
# install.packages("shinyjs")
# install.packages("shinythemes")

library(shiny)
library(shinyjs)
library(shinythemes)
options(max.print = .Machine$integer.max) # since my initial program stops with error : "reached getOption("max.print") -- omitted 59 rows (reference: https://stackoverflow.com/questions/46526299/max-print-option-in-r)

# DATA INPUTS ================================================================================================
  
  # create a data frame for the nutrients and vector of nutrients of each food
  foodSelection = data.frame(
    Nutrient = c("Calories", "Cholesterol", "Total fat", "Sodium", 
                 "Carbohydrates", "Dietary Fiber", "Protein", "Vitamin A", 
                 "Vitamin C", "Calcium", "Iron"),
    NutrientsMin=c(1900,0,0,0,0,25,50,5000,50,800,10),
    NutrientsMax=c(2250,300,65,2400,300,100,100,50000,20000,1600,30),
    "Frozen Broccoli"=c(73.8,0,0.8,68.2,13.6,8.5,8,5867.4,160.2,159,2.3),
    "Carrots,Raw"=c(23.7,0,0.1,19.2,5.6,1.6,0.6,15471,5.1,14.9,0.3),
    "Celery,Raw"=c(6.4,0,0.1,34.8,1.5,0.7,0.3,53.6,2.8,16,0.2),
    "Frozen Corn"=c(72.2,0,0.6,2.5,17.1,2,2.5,106.6,5.2,3.3,0.3),
    "Iceberg Lettuce,Raw"=c(2.6,0,0,1.8,0.4,0.3,0.2,66,0.8,3.8,0.1),
    "Peppers, Sweet, Raw"=c(20,0,0.1,1.5,4.8,1.3,0.7,467.7,66.1,6.7,0.3),
    "Potatoes, Baked"=c(171.5,0,0.2,15.2,39.9,3.2,3.7,0,15.6,22.7,4.3),
    "Tofu"=c(88.2,0,5.5,8.1,2.2,1.4,9.4,98.6,0.1,121.8,6.2),
    "Roasted Chicken"=c(277.4,129.9,10.8,125.6,0,0,42.2,77.4,0,21.9,1.8),
    "Spaghetti W/ Sauce"=c(358.2,0,12.3,1237.1,58.3,11.6,8.2,3055.2,27.9,80.2,2.3),
    "Tomato,Red,Ripe,Raw"=c(25.8,0,0.4,11.1,5.7,1.4,1,766.3,23.5,6.2,0.6),
    "Apple,Raw,W/ Skin"=c(81.4,0,0.5,0,21,3.7,0.3,73.1,7.9,9.7,0.2),
    "Banana"=c(104.9,0,0.5,1.1,26.7,2.7,1.2,92.3,10.4,6.8,0.4),
    "Grapes"=c(15.1,0,0.1,0.5,4.1,0.2,0.2,24,1,3.4,0.1),
    "Kiwifruit,Raw, Fresh"=c(46.4,0,0.3,3.8,11.3,2.6,0.8,133,74.5,19.8,0.3),
    "Oranges"=c(61.6,0,0.2,0,15.4,3.1,1.2,268.6,69.7,52.4,0.1),
    "Bagels"=c(78,0,0.5,151.4,15.1,0.6,3,0,0,21,1),
    "Wheat Bread"=c(65,0,1,134.5,12.4,1.3,2.2,0,0,10.8,0.7),
    "White Bread"=c(65,0,1,132.5,11.8,1.1,2.3,0,0,26.2,0.8),
    "Oatmeal Cookies"=c(81,0,3.3,68.9,12.4,0.6,1.1,2.9,0.1,6.7,0.5),
    "Apple Pie"=c(67.2,0,3.1,75.4,9.6,0.5,0.5,35.2,0.9,3.1,0.1),
    "Chocolate Chip Cookies"=c(78.1,5.1,4.5,57.8,9.3,0,0.9,101.8,0,6.2,0.4),
    "Butter,Regular"=c(35.8,10.9,4.1,41.3,0,0,0,152.9,0,1.2,0),
    "Cheddar Cheese"=c(112.7,29.4,9.3,173.7,0.4,0,7,296.5,0,202,0.2),
    "3.3% Fat,Whole Milk"=c(149.9,33.2,8.1,119.6,11.4,0,8,307.4,2.3,291.3,0.1),
    "2% Lowfat Milk"=c(121.2,18.3,4.7,121.8,11.7,0,8.1,500.2,2.3,296.7,0.1),
    "Skim Milk"=c(85.5,4.4,0.4,126.2,11.9,0,8.4,499.8,2.4,302.3,0.1),
    "Poached Eggs"=c(74.5,211.5,5,140,0.6,0,6.2,316,0,24.5,0.7),
    "Scrambled Eggs"=c(99.6,211.2,7.3,168,1.3,0,6.7,409.2,0.1,42.6,0.7),
    "Bologna,Turkey"=c(56.4,28.1,4.3,248.9,0.3,0,3.9,0,0,23.8,0.4),
    "Frankfurter, Beef"=c(141.8,27.4,12.8,461.7,0.8,0,5.4,0,10.8,9,0.6),
    "Ham,Sliced,Extra lean"=c(37.1,13.3,1.4,405.1,0.3,0,5.5,0,7.4,2,0.2),
    "Kielbasa,Prk"=c(80.6,17.4,7.1,279.8,0.6,0,3.4,0,5.5,11.4,0.4),
    "Cap'N Crunch"=c(119.6,0,2.6,213.3,23,0.5,1.4,40.6,0,4.8,7.5),
    "Cheerios"=c(111,0,1.8,307.6,19.6,2,4.3,1252.2,15.1,48.6,4.5),
    "Corn Flks, Kellogg'S"=c(110.5,0,0.1,290.5,24.5,0.7,2.3,1252.2,15.1,0.9,1.8),
    "Raisin Brn, Kellg'S"=c(115.1,0,0.7,204.4,27.9,4,4,1250.2,0,12.9,16.8),
    "Rice Krispies"=c(112.2,0,0.2,340.8,24.8,0.4,1.9,1252.2,15.1,4,1.8),
    "Special K"=c(110.8,0,0.1,265.5,21.3,0.7,5.6,1252.2,15.1,8.2,4.5),
    "Oatmeal"=c(145.1,0,2.3,2.3,25.3,4,6.1,37.4,0,18.7,1.6),
    "Malt-O- Meal,Choc"=c(607.2,0,1.5,16.5,128.2,0,17.3,0,0,23.1,47.2),
    "Pizza W/Pepperoni"=c(181,14.2,7,267,19.9,0,10.1,281.9,1.6,64.6,0.9),
    "Taco"=c(369.4,56.4,20.6,802,26.7,0,20.7,855,2.2,220.6,2.4),
    "Hamburger W/Toppings"=c(275,42.8,10.2,563.9,32.7,0,13.6,126.3,2.6,51.4,2.5),
    "Hotdog, Plain"=c(242.1,44.1,14.5,670.3,18,0,10.4,0,0.1,23.5,2.3),
    "Couscous"=c(100.8,0,0.1,4.5,20.9,1.3,3.4,0,0,7.2,0.3),
    "White Rice"=c(102.7,0,0.2,0.8,22.3,0.3,2.1,0,0,7.9,0.9),
    "Macaroni,Ckd"=c(98.7,0,0.5,0.7,19.8,0.9,3.3,0,0,4.9,1),
    "Peanut Butter"=c(188.5,0,16,155.5,6.9,2.1,7.7,0,0,13.1,0.6),
    "Pork"=c(710.8,105.1,72.2,38.4,0,0,13.8,14.7,0,59.9,0.4),
    "Sardines in Oil"=c(49.9,34.1,2.7,121.2,0,0,5.9,53.8,0,91.7,0.7),
    "White Tuna in Water"=c(115.6,35.7,2.1,333.2,0,0,22.7,68,0,3.4,0.5),
    "Popcorn,Air- Popped"=c(108.3,0,1.2,1.1,22.1,4.3,3.4,55.6,0,2.8,0.8),
    "Potato Chips,Bbqflvr"=c(139.2,0,9.2,212.6,15,1.2,2.2,61.5,9.6,14.2,0.5),
    "Pretzels"=c(108,0,1,486.2,22.5,0.9,2.6,0,0,10.2,1.2),
    "Tortilla Chip"=c(142,0,7.4,149.7,17.8,1.8,2,55.6,0,43.7,0.4),
    "Chicknoodl Soup"=c(150.1,12.3,4.6,1862.2,18.7,1.5,7.9,1308.7,0,27.1,1.5),
    "Splt Pea&Hamsoup"=c(184.8,7.2,4,964.8,26.8,4.1,11.1,4872,7,33.6,2.1),
    "Vegetbeef Soup"=c(158.1,10,3.8,1915.1,20.4,4,11.2,3785.1,4.8,32.6,2.2),
    "Neweng Clamchwd"=c(175.7,10,5,1864.9,21.8,1.5,10.9,20.1,4.8,82.8,2.8),
    "Tomato Soup"=c(170.7,0,3.8,1744.4,33.2,1,4.1,1393,133,27.6,3.5),
    "New EClamchwd,W/ Mlk"=c(163.7,22.3,6.6,992,16.6,1.5,9.5,163.7,3.5,186,1.5),
    "Crm Mshrm Soup,W/Mlk"=c(203.4,19.8,13.6,1076.3,15,0.5,6.1,153.8,2.2,178.6,0.6),
    "Beanbacn Soup,W/Watr"=c(172,2.5,5.9,951.3,22.8,8.6,7.9,888,1.5,81,2),
    check.names = FALSE  # reference: https://stat.ethz.ch/R-manual/R-devel/library/base/html/data.frame.html
  )
  
  Cost = c(0.16,0.07,0.04,0.18,0.02,0.53,0.06,0.31,0.84,0.78,0.27,0.24,
           0.15,0.32,0.49,0.15,0.16,0.05,0.06,0.09,0.16,0.03,0.05,0.25,
           0.16,0.23,0.13,0.08,0.11,0.15,0.27,0.33,0.15,0.31,0.28,0.28,
           0.34,0.32,0.38,0.82,0.52,0.44,0.59,0.83,0.31,0.39,0.08,0.17,
           0.07,0.81,0.45,0.69,0.04,0.22,0.12,0.19,0.39,0.67,0.71,0.75,
           0.39,0.99,0.65,0.67)

# SIMPLEX METHOD ---------
  stopIteration = function(objectiveRow) { 
    stop = TRUE
    for (element in objectiveRow) {
      if (element < 0) {
        stop = FALSE
        break
      }
    }
    return(stop)
  }
  
  # finds the lowest value (different conditions for getting pivol col and pivot row has already been initialized)
  findSmallest = function(objectiveRow) { 
    pivotCol = 1
    for (i in 1:length(objectiveRow)) {
      if ((objectiveRow[i] < objectiveRow[pivotCol]) && objectiveRow[i] != Inf) {
        pivotCol = i
      }
    }
    return(pivotCol)
  }
  
  # performs the simplex method of Gauss-Jordan elimination 
  pivot = function(tableau, pivotRow, pivotCol) {
    
    # check if pivot element is zero to prevent division by zero
    if (tableau[pivotRow, pivotCol] == 0) {
      stop("pivot element is zero")
    }
    
    # normalize pivot row
    tableau[pivotRow, ] = tableau[pivotRow, ] / tableau[pivotRow,pivotCol]
    
    # solving for values of elements not in pivot row
    for (i in 1:nrow(tableau)) {
      if (i != pivotRow) { 
        tableau[i, ] = tableau[i, ] - (tableau[i, pivotCol] * tableau[pivotRow, ])
      }
    }
    return(tableau)
  }
  
  Simplex = function(tableau, isMax)  {
    iterations = 0
    tablist = list()
    while (TRUE) {
      
      # maximum iterations at 1000
      if (iterations == 1000) {
        return(list(tableau = tableau, iterations = iterations, infeasible = TRUE, why = "max"))
      }
      
      # takes the last row as the objective row
      objectiveRow = tableau[nrow(tableau), ] 
      
      # check stopping criterion
      shouldStop = stopIteration(objectiveRow)
      if (shouldStop) {
        break
      }
      
      # if iteration continues, 
      iterations = iterations+1
      
      # get pivot column
      pivotColIndex = findSmallest(objectiveRow)
      solColumn = tableau[, ncol(tableau)]
      pivotCol = tableau[, pivotColIndex]
      
      # get pivot row (using lowest positive test ratio) 
      testRatios = solColumn / pivotCol
      
      # normalize invalid test ratios to Inf (always greater than any number)
      for (i in 1:length(testRatios)) {
        if (is.nan(testRatios[i]) || testRatios[i] <= 0 || testRatios[i] == -Inf) { 
          testRatios[i] = Inf
        }
      }
      
      # check if there is a valid test ratio
      valid = FALSE
      for (i in 1:length(testRatios)) {
        if (testRatios[i] != Inf) {
          valid = TRUE
          break
        }
      }
      
      # if none is valid, stop
      if (!valid) {
        return(list(tableau = tableau, iterations = iterations, infeasible = TRUE, why = "TR"))
      }
      
      # find smallest positive
      pivotRowIndex = findSmallest(testRatios)
      
      # Pivot the tableau
      tableau = pivot(tableau, pivotRowIndex, pivotColIndex)
      if (is.null(tableau)) {
        return(list(tableau = tableau, iterations = iterations, infeasible = TRUE, why = "zero"))
      }
      
      # save the tableau in a list
      tablist = append(tablist, list(tableau)) # reference: https://stackoverflow.com/questions/33549748/how-to-extract-matrix-object-from-list-without-being-a-list-anymore 
    }
    
    # Get the basic solution (remove second to the last column/row)
    if (isMax) {
      basicSolution = tableau[1:(nrow(tableau)-1), ncol(tableau)]  
    } else {
      basicSolution = tableau[nrow(tableau), -(ncol(tableau)-1)]
    }
    
    # get Z value
    Z = basicSolution[length(basicSolution)]
    
    return(list(tableau = tableau, Z = Z , basicSolution = basicSolution, iterations = iterations, tablist = tablist, infeasible = FALSE)) 
  }
    
# UI -------------------------------------------------------------------------------------------------------

ui = fluidPage(theme = shinytheme("lumen"), # https://github.com/rstudio/shiny-examples/tree/main 117-shinythemes
  # add color to tabs reference: https://stackoverflow.com/questions/35025145/background-color-of-tabs-in-shiny-tabpanel
  tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: #347928; color: white; border-color: transparent; font-weight: bold}
    .tabbable > .nav > li[class=active]    > a {background-color: #C0EBA6; color: black; border-color: transparent; font-weight: bold}
  ")),
  titlePanel(
    h1(htmlOutput("header"))
  ),
  hr(),
  
  tabsetPanel(  # reference: https://github.com/rstudio/shiny-examples/tree/main 006 tabsets
    id = "tabs",
    tabPanel("User Input",  
      HTML("<br>"),
      sidebarLayout(
        sidebarPanel(
          
          # exclude the nutrients column, nutrients min, and nutrients max as choices
          checkboxGroupInput("foodSelectionCol", "Food Selections", # reference: https://github.com/rstudio/shiny-examples/tree/main 069-widget-check-group
                             choices = names(foodSelection)[!names(foodSelection) %in% c("Nutrient", "NutrientsMin","NutrientsMax")]), # reference: https://stackoverflow.com/questions/5234117/how-to-drop-columns-by-name-in-a-data-frame
          
          actionButton("allReset", "Reset Options", style = "font-size: 12px; background-color: #347928; color: white; border-width: 1px"), # styling buttons reference: https://www.geeksforgeeks.org/change-the-color-of-action-button-in-shiny-using-r/
          actionButton("allSelect", "Select All", style = "font-size: 12px; background-color: #347928; color: white; border-width: 1px"), 
          helpText("Note: Choose at least 1 food"),
          shinyjs::useShinyjs(), # to be enabled once condition is satisfied (reference: https://forum.posit.co/t/disable-actionbutton-until-file-upload-is-complete/39026/3)
          actionButton("solve", "Start Solving", style = "font-size: 18px; color: #347928; background-color: #C0EBA6; border-width: 1px") 
        ),
        mainPanel(
          h2(textOutput("ObjFxn")), 
          verbatimTextOutput("theObjFxn"), # display the objective function (reference in verbatimOutput: https://www.appsilon.com/post/r-shiny-reactivity)
          h2(textOutput("Const")),
          verbatimTextOutput("theConst"), 
          h2(textOutput("tabletitle")), 
          tableOutput("table") 
        )
      )
    ),
    tabPanel("Solving",
      h3(textOutput("noSolve")),
      h3(textOutput("IntTabTitle")),
      tableOutput("IntTab"), 
      h3(textOutput("IntSolTitle")),
      tableOutput("IntSol"),
      HTML("<br><br>"),
      h3(htmlOutput("NxtIter")), 
      tableOutput("NxtIterTableau"),
      tableOutput("NxtIterSol"),
      fluidRow( # centers the buttons reference: https://stackoverflow.com/questions/37304599/align-actionbutton-in-shiny-app and https://www.javatpoint.com/how-to-center-a-button-in-css
        column(
          width = 12,
          div(
            hidden(
              actionButton("previous", "Previous", style = "font-size: 12px; background-color: #347928; color: white; border-width: 1px"),
              actionButton("nxt", "Next", style = "font-size: 12px; background-color: #347928; color: white; border-width: 1px")
            ),
            HTML("<br><br><br><br>"),
            hidden(
              actionButton("showRes", "See results", style = "font-size: 18px; color: #347928; background-color: #C0EBA6; border-width: 1px")
            ),
            HTML("<br><br>"),
            style = "text-align: center" 
          )
        )
      ),
      
      
    ),
    tabPanel("Results",
           # reference: https://www.codementor.io/@packt/producing-layout-in-rows-and-columns-with-shiny-qcjj8szq0
          HTML("<br>"),
          h4(htmlOutput("displayName")), # since line break (\n) does not seem to work on my end reference: https://www.geeksforgeeks.org/how-to-insert-new-line-in-r-shiny-string/
          verbatimTextOutput("foodItemName"),

      column(6,
        wellPanel(
          HTML("<br>"),
          h4(htmlOutput("text1")),
          tableOutput("finalTab"),
          h4(htmlOutput("text2")),
        )
      )
    )
  )
)

# Server ---------------------------------------------------------------------------------------------------
server = function(input, output, session) {
# (FOR USER TAB) ===========================================================================
    output$header = renderText({paste0(
      '<div style="text-align: center;">',
      '<span style="font-weight: bold; color: #347928;">NutriMax</span><br>',
      '<span style="font-size: 20px; color: black;"><i>a diet problem solver</span></i>',
      '</div>'
      )})
  
    # activate the reset button (reference:https://shiny.posit.co/r/articles/build/action-buttons/ and https://shiny.posit.co/r/reference/shiny/0.14/updateselectinput.html)
    observeEvent(input$allReset, {
      updateSelectInput(session, "foodSelectionCol", selected = character(0))
    })
    
    # activate the select all button (reference: https://shiny.posit.co/r/articles/build/action-buttons/ and https://www.youtube.com/watch?v=PNzNDq1_uKQ)
    observeEvent(input$allSelect, {
      updateSelectInput(session, "foodSelectionCol", selected = colnames(foodSelection)[-3])
    })
    
    # disable solve button if checked <= 1
    observe({ # reference: https://forum.posit.co/t/disable-actionbutton-until-file-upload-is-complete/39026/3)
      checked = length(input$foodSelectionCol)
      if (checked > 0 ) {
        shinyjs::enable("solve")
      } else {
        shinyjs::disable("solve")
      }
    })
    
    #show objective function, constraints, and initial tableau (reference: https://stackoverflow.com/questions/34408283/show-text-in-main-panel-of-shiny-app-after-click-of-a-button)
    output$ObjFxn = renderText({"Objective Function"})
    output$Const = renderText({ "Constraints"})
    output$tabletitle = renderText({"Selected Foods"})
    
    # formats the printing of the objective function and constraints :"D
    observe({
      if (length(input$foodSelectionCol) < 1) {
        output$theObjFxn = renderPrint({"Select at least 1 food"})
        output$theConst = renderPrint({"Select at least 1 food"})
      } else { 
        output$theObjFxn = renderPrint({
          index = match(input$foodSelectionCol, colnames(foodSelection)[4:ncol(foodSelection)]) # get indices for selected foods excluding the first 3 columns (reference: https://stackoverflow.com/questions/65086390/position-of-elements-from-one-vector-in-another-vector-with-r)
          price = Cost[index] # use the vector of indices to get the vector of prices matching the index 
          paste("Minimize C =", paste(price, "x_", 1:length(price), sep = "", collapse = " + "))
        })
        output$theConst = renderPrint({
          
          # prints the constraints for the minimium nutrients required
          for (i in 1:11) { 
            nutriMinConst = paste(foodSelection[i,input$foodSelectionCol], "x_", 1:length(input$foodSelectionCol), sep = "", collapse = " + ")
            slackVar = paste(">= ", foodSelection$NutrientsMin[i], sep = "")
            combined = paste(nutriMinConst,slackVar, sep = " - ")
            cat(combined, "\n")
          }
          
          # prints the constraints for the maximum nutrients required
          for (i in 1:11) { 
            nutriMaxConst = paste(" -",(foodSelection[i,input$foodSelectionCol]), "x_", 1:length(input$foodSelectionCol), sep = "", collapse = "")
            slackVar = paste(">= -", foodSelection$NutrientsMax[i], sep = "") 
            combined = paste(nutriMaxConst,slackVar, sep = " - ")
            cat(combined, "\n")
          }
          
          # prints constraints for servings (must be less than 10)
          for (i in 1:length(input$foodSelectionCol)) { 
            servingsConst = paste( "-x_",i," >= -10", sep = "", collapse = " + ")
            cat(servingsConst, "\n")
          }
          
          # prints constraints for servings (must be more than 0)
          for (i in 1:length(input$foodSelectionCol)) { 
            servingsConst = paste( "x_",i," >= 0", sep = "", collapse = " + ")
            cat(servingsConst, "\n")
          }
        })
      }
    })
    
    # displays the selected food items and nutritional values
    output$table = renderTable ({ #(reference to tabulate: main reference in 012 - datatables and https://shiny.posit.co/r/getstarted/shiny-basics/lesson4/)
      req(input$foodSelectionCol)  # displays a table when at least 1 is checked
      rownames(foodSelection) = foodSelection$Nutrient # takes the nutrient names vector as rownames
      foodSelectedCol = foodSelection[, input$foodSelectionCol, drop = FALSE]  # displays the vector of food as column in table
      foodSelectedCol # display the column
    }, rownames = TRUE, striped = TRUE, bordered = TRUE,  hover = TRUE, spacing = 'xs', style = "background-color: red")
    
    # set this into default first
    output$noSolve = renderText("Nothing to solve.")
    output$displayName = renderText("No Results to Show.")

# (ONCE "SOLVE" BUTTON CLICKED) ==============================================================
    observeEvent(input$solve, {
      # switch tab
      updateTabsetPanel(session, "tabs", selected = "Solving")
     
# SHOW INITIAL TABLEAU >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      
      # once solve button is clicked, remove the "No Solutions/Results to Show" and show the title
      output$noSolve = renderText("")
      output$IntTabTitle = renderText({"Initial Tableau"})
      output$IntSol = renderText({"Initial Solution"})
      
      # save the inputs in one variable to avoid dynamically changing tables
      foodSelectionCol = input$foodSelectionCol
      
      # creates the matrix to tranpose
      foodNutrients = foodSelection[, !names(foodSelection) %in% c("Nutrient", "NutrientsMin", "NutrientsMax")] # recreate a new data frame that exludes the first 3 columns in foodSelection
      beforeTableau = matrix(0,ncol = length(foodSelectionCol) + 1, nrow = 22 + length(foodSelectionCol) + 1)
      
      # for the nutrients constraints rows minus RHS
      for (i in 1:11) {
        for (j in 1:(length(foodSelectionCol))) {
          col = foodSelectionCol[j] # access the jth element in the list of selected foods and gives the index
          beforeTableau[i*2-1,j] = foodNutrients[i,col]
          beforeTableau[i*2,j] = -(foodNutrients[i,col])
        }
      }
      # for the servings constraints rows minus RHS
      for (i in 23:(nrow(beforeTableau)-1)) {
        for (j in 1:(length(foodSelectionCol))) {
          if (i-22 == j) {
            beforeTableau[i,j] = -1
          } else {
            beforeTableau[i,j] = 0
          }
        }
      }
      # for the objective fxn rows minus RHS
      for (j in 1:length(foodSelectionCol)) {
        col = foodSelectionCol[j]
        colIndex = which(names(foodNutrients) == col) # gets the index of the column in a data frame (reference: https://www.geeksforgeeks.org/get-column-index-in-data-frame-by-variable-name-in-r/)
        beforeTableau[nrow(beforeTableau),j] = Cost[colIndex]
      }
      #  for the RHS (food nutrients constraints)
      for (i in 1:11) {
        beforeTableau[i*2-1,ncol(beforeTableau)] = foodSelection$NutrientsMin[i]
        beforeTableau[i*2,ncol(beforeTableau)] = -(foodSelection$NutrientsMax[i])
      }
      # for the RHS (serving constraints)
      for (i in 23:nrow(beforeTableau)) {
        if (i == nrow(beforeTableau)) {
          beforeTableau[i,ncol(beforeTableau)] = 0 # this is an advance step since after adding an identity matrix for slacks later, the value of Z in the solution column will be 0
        } else {
          beforeTableau[i,ncol(beforeTableau)] = -10 
        }
      }
      
      # transpose the matrix
      initialTableau = t(beforeTableau)
      
      # multiply the last row to -1 (simulating the tranposing of the Z = objective fxn values)
      initialTableau[nrow(initialTableau),] = -(initialTableau[nrow(initialTableau),])
      
      # add a new matrix of slack variables + Z and cbind it into the initialTableau
      identityMat = matrix(0, nrow = length(foodSelectionCol)+1 , ncol = length(foodSelectionCol)+1)
      for (row in 1:nrow(identityMat)) {
        for (col in 1:ncol(identityMat)) {
          if (row == col) {
            identityMat[row,col] = 1
          }
        }
      }
      
      #inserts identity matrix
      initialTableau = cbind( 
        initialTableau[,1:(ncol(initialTableau)-1)],
        identityMat,
        initialTableau[,ncol(initialTableau)]
      )
      
      # display the initial tableau
      output$IntTab = renderTable ({ #(reference to tabulate: main reference in 012 - datatables and https://shiny.posit.co/r/getstarted/shiny-basics/lesson4/)
        req(foodSelectionCol)  # displays a table when at least 1 is checked
        # create the column names
        colNames = c(paste0("S_", 1:(22+length(foodSelectionCol))),paste0("X_", 1:length(foodSelectionCol)),"Z","Solution")
        colnames(initialTableau) = colNames
        initialTableau
      }, striped = TRUE, bordered = TRUE,  hover = TRUE, spacing = 'xs') # reference in rendering tables: 109-render-table
      
      # show initial basic solution and z
      output$IntSolTitle = renderText("Initial Basic Solution and Cost Value")
      output$IntSol = renderTable({
        intSolTab = initialTableau[nrow(initialTableau), ]
        intSolTab = intSolTab[-(ncol(initialTableau) - 1)] # exclude the 2nd to the last column of the last
        solution = t(intSolTab)
        colNames = c(paste0("S_", 1:(22 + length(foodSelectionCol))), paste0("X_", 1:length(foodSelectionCol)), "Z")
        colnames(solution) = colNames
        solution
      },striped = TRUE, bordered = TRUE,  hover = TRUE, spacing = 'xs')
      
# SHOW NEXT TABLEAUS (THRU NEXT PAGES) >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      
      # function to print the next tableau
      showTabSol = function(output, results, page) {
        if (length(results$tablist) == 0) {
          output$NxtIter = renderText({""})
          output$NxtIterTableau = renderTable({})
          output$NxtIterSol = renderTable({})
          return()
        } else {
          # display the next tableau and solution
          output$NxtIter = renderText({paste0("Next Iterations...<br><i>iteration ", values$page ,"</i>")})
          output$NxtIterTableau = renderTable({
            colNames = c(paste0("S_", 1:(22+length(foodSelectionCol))),paste0("X_", 1:length(foodSelectionCol)),"Z","Solution")
            colnames(results$tablist[[page]]) = colNames
            results$tablist[[page]]
          },striped = TRUE, bordered = TRUE,  hover = TRUE, spacing = 'xs')
          output$NxtIterSol = renderTable({
            getSol = (results$tablist)[[page]]
            solution = getSol[nrow(getSol),-(ncol(getSol)-1)]
            solution = t(solution)
            colNames = c(paste0("S_", 1:(22 + length(foodSelectionCol))), paste0("X_", 1:length(foodSelectionCol)), "Z")
            colnames(solution) = colNames
            solution
          },striped = TRUE, bordered = TRUE,  hover = TRUE, spacing = 'xs')
        }
      }
      
      # function call to simplex method
       resultsList = Simplex(initialTableau, FALSE) 
      
       values = reactiveValues(page = 1) # use reactive values to be accessed by different "observe" for pages (reference: https://shiny.posit.co/r/reference/shiny/latest/reactivevalues.html and https://www.youtube.com/watch?v=VF9s7_YY9TQ)
       
       showTabSol(output,resultsList,values$page)
       shinyjs::show("showRes")
       
       if (length(resultsList$tablist) == 0) {
         # hide the buttons
         shinyjs::hide("nxt")
         shinyjs::hide("previous")
       } else {
         # hide the buttons
         shinyjs::show("nxt")
         shinyjs::show("previous")
       }
       
       # enable and disable the next and previous buttons
       observe({
         if (values$page == 1) {
           shinyjs::enable("nxt")
           shinyjs::disable("previous")
         } else if (values$page == resultsList$iterations) {
           shinyjs::disable("nxt")
           shinyjs::enable("previous")
         } else {
           shinyjs::enable("nxt")
           shinyjs::enable("previous")
         }
       })
       
        # switching pages
        observeEvent(input$nxt, {
          values$page = (values$page)+1
          output$NxtIter = renderText({paste0("Next Iterations...<br><i>iteration ", values$page ,"</i>")})
          showTabSol(output,resultsList,values$page)
        })
        observeEvent(input$previous, {
          values$page = (values$page)-1
          output$NxtIter = renderText({paste0("Next Iterations...<br><i>iteration ", values$page ,"</i>")})
          showTabSol(output,resultsList,values$page)
        })
       
    # (RESULTS) =======================================
        # switch tab
        observeEvent(input$showRes, {
          updateTabsetPanel(session, "tabs", selected = "Results")
        })
       
      HTML("<br><br>")
      output$displayName = renderUI({
        HTML("<br>Your Input: <br><br> You selected <b>", length(foodSelectionCol), " foods </b> to consider for your diet")
        })
      output$foodItemName = renderPrint({foodSelectionCol})
        
        if (!resultsList$infeasible) {
          output$text1 = renderText({paste0("<br><i>The Optimized Menu</i> <br> The cost of the <b>optimal</b> diet is ", sprintf("%.2f", resultsList$Z) ,"$ per day.<br><br> <i>The Solution and Cost Breakdown by Food</i>")})
          
          output$finalTab = renderTable({
            
            # get the solution for the servings of each selected food
            servSelected = resultsList$basicSolution[(22+length(foodSelectionCol)+1):(length(resultsList$basicSolution)-1)]
            
            # gets the number of nonzero servings 
            numRow = sum(servSelected!=0)
            
            # initialize final table output
            finaltab = matrix(0, nrow=numRow, ncol = 3)
            colnames(finaltab) = c("Food", "Servings", "Cost")
            
            row = 1
            for (i in 1:length(servSelected)) {
              if (servSelected[i] != 0) {
                finaltab[row, 1] = (foodSelectionCol)[i]
                finaltab[row, 2] = sprintf("%.2f",servSelected[i])
                
                # get the index to access its value in the cost vector 
                index = match(foodSelectionCol[i], colnames(foodNutrients)) 
                price = Cost[index] # use the vector of indices to get the vector of prices matching the index 
                finaltab[row, 3] = sprintf("%.2f", servSelected[i] * price)
                
                row = row+1
              }
            }
            
            finaltab
          }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = 'xs')
          output$text2 = renderText({paste0("<i> iterations: ", resultsList$iterations, "</i>")})
          
        } else {
          reason = switch(resultsList$why,
            "zero" = "division by zero occurred in the pivot element.",
            "max"  = "the program reached its maximum number of allowable iterations (n = 1000).",
            "TR"   = "the test ratios for finding the pivot row are all invalid."
          )
          output$text1 = renderText({paste0(
            "<br><b>The problem is infeasible.</b><br>",
            "It is not possible to meet the nutritional constraints with the foods you have selected.<br>",
            "<i>The possible reason is that ", reason, "</i>"
          )})
          output$text2 = renderText({})
          output$finalTab = renderTable({})
        }
    })   
}

# Run the application
shinyApp(ui = ui, server = server)


  

