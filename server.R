op <- function(operand1, operator, operand2) {
        result <- numeric()
        sign <- c("+", "-", "*", "/")
        
        # make sure that a numeric value, not a string, is used in calculations
        if (length(operand1) == 1) oper1 <- operand1
        if (length(operand1) == 2) oper1 <- as.numeric(operand1[1])
        
        if (length(operand2) == 1) oper2 <- operand2
        if (length(operand2) == 2) oper2 <- as.numeric(operand2[1])
        
        # mimic arithmetic operations and store outcome in result[1]
        if (operator == 1) result[1] <- oper1 + oper2
        if (operator == 2) result[1] <- oper1 - oper2
        if (operator == 3) result[1] <- oper1 * oper2
        if (operator == 4) result[1] <- oper1 / oper2
        
        # paste operands and operator into arithmetic expression and store in result[2]
        if (length(operand1) == 2 && length(operand2) == 1) {
                result[2] <- paste("(", operand1[2], sign[operator], operand2[1], ")")
        } else if (length(operand1) == 1 && length(operand2) == 2) {
                result[2] <- paste("(", operand1[1], sign[operator], operand2[2], ")")
        } else if (length(operand1) == 2 && length(operand2) == 2) {
                result[2] <- paste("(", operand1[2], sign[operator], operand2[2], ")")
        } else if (length(operand1) == 1 && length(operand2) == 1) {
                result[2] <- paste("(", operand1[1], sign[operator], operand2[1], ")")
        }
        
        # return "result" of two parts: string of numeric outcome & string expression of 
        # arithmetic operation
        return(result)
}

get24 <- function(card1, card2, card3, card4) {
        sign <- c("+", "-", "*", "/")
        where24 <- 0
        select24 <- paste("You cannot get 24 using these numbers.", 
                "Please modify your choices.")
        card <- c("A = 1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J = 11", 
                "Q = 12", "K = 13", "none")
        
        # find the only correct match of each input string
        int1 <- grep(card1, card)[1]
        int2 <- grep(card2, card)[1]
        int3 <- grep(card3, card)[1]
        int4 <- grep(card4, card)[1]
        
        # begin calculations when all four input values are given
        if (int1 == 14 | int2 == 14 | int3 == 14 | int4 == 14) {
                select24 <- 'Please select a card from each suit and click "Calculate".'
        } else {
                num <- c(int1, int2, int3, int4)
                
                mat_operand <- matrix(data = 00, nrow = 24, ncol = 4)
                mat_operator <- matrix(data = 0, nrow = 64, ncol = 3)
                mat_record <- matrix(data = "", nrow = 24 * 64 * 5, ncol = 2)
                mat_24 <- matrix(nrow = 3, ncol = 2)
                
                # store all possible combinations of operands  in a 24 * 4 matrix 
                # called mat_operand
                i <- 1
                for (n1 in 1:4) {
                        for (n2 in (1:4)[-n1]) {
                                for (n3 in (1:4)[-c(n1, n2)]) {
                                        for (n4 in (1:4)[-c(n1, n2, n3)]) {
                                                mat_operand[i,] <- c(n1, n2, n3, n4)
                                                i <- i + 1
                                        }
                                }
                        }
                }
                
                # store all possible combinations of operators in a 64 * 3 matrix 
                # called mat_operator
                j <- 1
                for (o1 in 1:4) {
                        for (o2 in 1:4) {
                                for (o3 in 1:4) {
                                        mat_operator[j, ] <- c(o1, o2, o3)
                                        j <- j + 1
                                }
                        }
                }
                
                i <- 1; j <- 1; k <- 1
                # operation steps: left operator -> center operator -> right operator
                # store all possible equations following the steps above in mat_record, 
                # row 1:(24*64)
                for (i in seq_along(1:24)) {
                        for (j in seq_along(1:64)) {
                                n1 <- mat_operand[i, 1]
                                n2 <- mat_operand[i, 2]
                                n3 <- mat_operand[i, 3]
                                n4 <- mat_operand[i, 4]
                                o1 <- mat_operator[j, 1]
                                o2 <- mat_operator[j, 2]
                                o3 <- mat_operator[j, 3]
                                left <- op(num[n1], o1, num[n2])
                                center <- op(left, o2, num[n3])
                                mat_record[k,] <- op(center, o3, num[n4])
                                k <- k + 1
                        }
                }
                
                # check if "24" appears in the first column of mat_record
                # if "24" is available, end the loop and store the full expression 
                # that gives 24
                where24 <- which(mat_record == "24")
                if (sum(where24) > 0) {
                        all24 <- mat_record[where24, 2]
                        select24 <- paste(all24[1], "= 24")
                } else {
                        i <- 1; j <- 1; k <- 24 * 64 + 1
                        # mat_record, row (24*64+1):(2*24*64)
                        # steps: left -> right -> center
                        for (i in seq_along(1:24)) {
                                for (j in seq_along(1:64)) {
                                        n1 <- mat_operand[i, 1]
                                        n2 <- mat_operand[i, 2]
                                        n3 <- mat_operand[i, 3]
                                        n4 <- mat_operand[i, 4]
                                        o1 <- mat_operator[j, 1]
                                        o2 <- mat_operator[j, 2]
                                        o3 <- mat_operator[j, 3]
                                        left <- op(num[n1], o1, num[n2])
                                        right <- op(num[n3], o3, num[n4])
                                        mat_record[k,] <- op(left, o2, right)
                                        k <- k + 1
                                }
                        }
                        
                        where24 <- which(mat_record == "24")
                        if (sum(where24) > 0) {
                                all24 <- mat_record[where24, 2]
                                select24 <- paste(all24[1], "= 24")
                        } else {
                                i <- 1; j <- 1; k <- 2 * 24 * 64 + 1
                                # mat_record, row (2*24*64+1):(3*24*64), 
                                # steps: center -> left -> right
                                for (i in seq_along(1:24)) {
                                        for (j in seq_along(1:64)) {
                                                n1 <- mat_operand[i, 1]
                                                n2 <- mat_operand[i, 2]
                                                n3 <- mat_operand[i, 3]
                                                n4 <- mat_operand[i, 4]
                                                o1 <- mat_operator[j, 1]
                                                o2 <- mat_operator[j, 2]
                                                o3 <- mat_operator[j, 3]
                                                center <- op(num[n2], o2, num[n3])
                                                left <- op(num[n1], o1, center)
                                                mat_record[k,] <- op(left, o3, num[n4])
                                                k <- k + 1
                                        }
                                }
                                
                                where24 <- which(mat_record == "24")
                                if (sum(where24) > 0) {
                                        all24 <- mat_record[where24, 2]
                                        select24 <- paste(all24[1], "= 24")
                                } else {
                                        i <- 1; j <- 1; k <- 3 * 24 * 64 + 1
                                        # mat_record, row (3*24*64+1):(4*24*64), 
                                        # steps: center -> right -> left
                                        for (i in seq_along(1:24)) {
                                                for (j in seq_along(1:64)) {
                                                        n1 <- mat_operand[i, 1]
                                                        n2 <- mat_operand[i, 2]
                                                        n3 <- mat_operand[i, 3]
                                                        n4 <- mat_operand[i, 4]
                                                        o1 <- mat_operator[j, 1]
                                                        o2 <- mat_operator[j, 2]
                                                        o3 <- mat_operator[j, 3]
                                                        center <- op(num[n2], o2, num[n3])
                                                        right <- op(center, o3, num[n4])
                                                        mat_record[k,] <- 
                                                                op(num[n1], o1, right)
                                                        k <- k + 1
                                                }
                                        }
                                        
                                        where24 <- which(mat_record == "24")
                                        if (sum(where24) > 0) {
                                                all24 <- mat_record[where24, 2]
                                                select24 <- paste(all24[1], "= 24")
                                        } else {
                                                i <- 1; j <- 1; k <- 4 * 24 * 64 + 1
                                                # mat_record, row (4*24*64+1):(5*24*64), 
                                                # steps: right -> center -> left
                                                for (i in seq_along(1:24)) {
                                                        for (j in seq_along(1:64)) {
                                                                n1 <- mat_operand[i, 1]
                                                                n2 <- mat_operand[i, 2]
                                                                n3 <- mat_operand[i, 3]
                                                                n4 <- mat_operand[i, 4]
                                                                o1 <- mat_operator[j, 1]
                                                                o2 <- mat_operator[j, 2]
                                                                o3 <- mat_operator[j, 3]
                                                                right <- op(num[n3], o3, num[n4])
                                                                center <- op(num[n2], o2, right)
                                                                mat_record[k,] <- 
                                                                        op(num[n1], o1, center)
                                                                k <- k + 1
                                                        }
                                                }
                                                
                                                where24 <- which(mat_record == "24")
                                                if (sum(where24) > 0) {
                                                        all24 <- mat_record[where24, 2]
                                                        select24 <- paste(all24[1], "= 24")
                                                }
                                        }
                                }
                        }
                }
        }
        
        return(select24)
}

shinyServer(
        function(input, output) {
                # output of a full expression of equation with a result of 24
                output$t_main <- renderText({
                        # result will only show when button is clicked
                        input$goButton
                        isolate(get24(input$spade, input$heart, input$club, input$diamond))
                })
        }
)