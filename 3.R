merge_data_frames <- function() {
  n1 <- as.integer(readline(prompt = "Enter the number of rows for the first data frame: "))
  df1 <- data.frame(id = integer(n1), cars = character(n1), stringsAsFactors = FALSE)
  
  for (i in 1:n1) {
    df1$id[i] <- as.integer(readline(prompt = paste("Enter id for row", i, ": ")))
    df1$cars[i] <- readline(prompt = paste("Enter car for row", i, ": "))
  }
  
  n2 <- as.integer(readline(prompt = "Enter the number of rows for the second data frame: "))
  df2 <- data.frame(id = integer(n2), bikes = character(n2), stringsAsFactors = FALSE)
  
  for (i in 1:n2) {
    df2$id[i] <- as.integer(readline(prompt = paste("Enter id for row", i, ": ")))
    df2$bikes[i] <- readline(prompt = paste("Enter bike for row", i, ": "))
  }
  
  total <- merge(df1, df2, by = 'id')
  print("Merged Data Frame:")
  print(total)
}

check_palindrome <- function() {
  n <- as.integer(readline(prompt = "Enter the number of rows for the matrix: "))
  matrix_data <- matrix(nrow = n, ncol = 3)
  
  for (i in 1:n) {
    for (j in 1:3) {
      matrix_data[i, j] <- readline(prompt = paste("Enter value for row", i, "column", j, ": "))
    }
  }
  
  print(matrix_data)
  is_palindrome <- function(row) {
    return(identical(row, rev(row)))
  }
  
  results <- apply(matrix_data, 1, is_palindrome)
  print("Palindrome Check Results (TRUE = Palindrome, FALSE = Not a Palindrome):")
  print(results)
}

find_max_min <- function() {
  n <- as.integer(readline(prompt = "Enter the number of rows for the matrix: "))
  m <- as.integer(readline(prompt = "Enter the number of columns for the matrix: "))
  
  ogm <- matrix(nrow = n, ncol = m)
  
  for (i in 1:n) {
    for (j in 1:m) {
      ogm[i, j] <- as.numeric(readline(prompt = paste("Enter value for row", i, "column", j, ": ")))
    }
  }
  
  rmax <- apply(ogm, 1, max)
  cmax <- apply(ogm, 2, max)
  
  print("Row Maximums:")
  print(rmax)
  print("Column Maximums:")
  print(cmax)
}

fibonacci_factorial <- function() {
  # Prompt user for a vector of numbers
  user_input <- readline(prompt = "Enter a vector of numbers separated by spaces: ")
  numbers <- as.numeric(unlist(strsplit(user_input, " ")))  # Convert input to numeric vector
  
  # Generate Fibonacci series up to a certain limit
  max_fib <- max(numbers)  # We only need Fibonacci numbers up to the maximum number in the input
  fib <- function(n) {
    a <- 0
    b <- 1
    f <- numeric(n)
    f[1] <- a
    if (n > 1) {
      f[2] <- b
    }
    for (i in 3:n) {
      num <- a + b
      f[i] <- num
      a <- b
      b <- num
    }
    return(f[f <= max_fib])  # Return only Fibonacci numbers less than or equal to max_fib
  }
  
  # Generate Fibonacci numbers
  fib_series <- fib(100)  # Generate a large enough Fibonacci series
  cat("Fibonacci series up to", max_fib, ":", fib_series, "\n")
  
  # Check for each number in the input vector
  for (num in numbers) {
    if (num %in% fib_series) {
      fact_result <- factorial(num)
      cat("Number", num, "is in the Fibonacci series. Its factorial is:", fact_result, "\n")
    }
  }
}

# Main menu loop
while (TRUE) {
  cat("\nMenu:\n")
  cat("1. Merge Data Frames\n")
  cat("2. Check for Palindromes\n")
  cat("3. Find Max and Min Values\n")
  cat("4. Calculate Fibonacci and its Factorial\n")
  cat("5. Exit\n")
  
  choice <- as.integer(readline(prompt = "Enter your choice (1-5): "))
  
  switch(choice,
         `1` = merge_data_frames(),
         `2` = check_palindrome(),
         `3` = find_max_min(),
         `4` = fibonacci_factorial(),
         `5` = {
           cat("Exiting the program.\n")
           break
         },
         cat("Invalid choice. Please try again.\n")
  )
}