shinyUI(pageWithSidebar(
        headerPanel('"GET 24!" The Game'),
        sidebarPanel(
                # select a card value from the dropdown list
                # default value is "none"
                selectInput("spade", 
                        label = "Choose your spade card:",
                        choices = c("A = 1", "2", "3", "4", "5", "6", "7", "8", 
                                "9", "10", "J = 11", "Q = 12", "K = 13", "none"),
                        selected = "none"),
                selectInput("heart", 
                        label = "Choose your heart card:",
                        choices = c("A = 1", "2", "3", "4", "5", "6", "7", "8", 
                                "9", "10", "J = 11", "Q = 12", "K = 13", "none"),
                        selected = "none"),
                selectInput("club", 
                        label = "Choose your club card:",
                        choices = c("A = 1", "2", "3", "4", "5", "6", "7", "8", 
                                "9", "10", "J = 11", "Q = 12", "K = 13", "none"),
                        selected = "none"),
                selectInput("diamond", 
                        label = "Choose your diamond card:",
                        choices = c("A = 1", "2", "3", "4", "5", "6", "7", "8", 
                                "9", "10", "J = 11", "Q = 12", "K = 13", "none"),
                        selected = "none"),
                p("04-25-2015"),
                p("by Ariel")
        ),
        mainPanel(
                h3('Your "GET 24!" equation:'),
                p("After after each new selection, you may need to wait a few seconds 
                        for the game to respond properly."),
                # when a result of 24 is reached, game will print a full expression 
                # of equation
                verbatimTextOutput("t_main"),
                h4("Game Instructions:"),
                p("Select a card from the dropdown list for each of the four suits 
                        (spade, heart, club, diamond). 
                        Every suit has thirteen different cards including", 
                        code("A"), code("2"), code("3"), code("4"), code("5"), 
                        code("6"), code("7"), code("8"), code("9"), code("10"), 
                        code("J"), code("Q"), code("K"), ". 
                        Here A is mapped A to 1, J to 11, Q to 12, and K to 13, 
                        so each suit can represent integers from 1 to 13. "),
                p('After the player selects a card from each suit, "GET 24!" will 
                        calculate whether the numbers represented by all four 
                        selected cards can undergo arithmetic operations to give 
                        a result of 24. Operators allowed in this game are', 
                        code("+"), code("-"), code("*"), code("/"), '. Pairs of 
                        brackets are also allowed and are used to clarify 
                        operator precedence. '),
                p('If "GET 24!" finds any arithmetic equation which gives 24, 
                        it will print one complete example above. Otherwise, 
                        "GET 24!" will ask the player to modify card selection. '),
                p("Explore and enjoy.")
        )
))