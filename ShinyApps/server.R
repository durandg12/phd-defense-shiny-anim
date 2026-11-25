function(input, output, session) {
  re <- reactive({
    moy <- input$delta
    s <- input$s
    nb_reg_signal <- input$nb_reg_signal
    K <- m / s
    reg_signal <- sample(K, nb_reg_signal)
    signal <- as.numeric(sapply(reg_signal, function(x) {
      (1:s) + (x - 1) * s
    }))
    theta <- rep(0, m)
    theta[signal] <- 1
    muvector <- theta * moy
    X <- muvector + xi
    pvalues <- pnorm(X, lower.tail = FALSE)
    list(pvalues = pvalues, reg_signal = reg_signal, K = K)
  })
  
  output$plot <- renderPlotly({
    res <- re()
    pvalues <- res$pvalues
    
    data <- data.frame(key = 1:m, SNP = 1:m, mlogp = -log(pvalues))
    plot_ly(data,
            x = ~SNP,
            y = ~mlogp,
            key = ~key,
            width = 800,
            height = 400) |>
      layout(dragmode = "select") |>
      layout(xaxis = list(title = ""), yaxis = list(title = ""))
  })
  
  output$bound <- renderText({
    s <- input$s
    alpha <- input$alpha
    res <- re()
    pvalues <- res$pvalues
    reg_signal <- res$reg_signal
    K <- res$K
    
    d <- event_data("plotly_selected")
    if (is.null(d)) {
      collec <- c(reg_signal)
      R <- unique(as.numeric(sapply(collec, function(x) {
        (1:s) + (x - 1) * s
      })))
    } else {
      R <- as.numeric(d$key)
    }
    pval <- pvalues[R]
    
    V <- min(sapply(
      1:m,
      function(k) {
        min(
          sum(pval >= alpha * k / m) + k - 1,
          length(pval)
        )
      }
    ))
    
    truc2 <- function(k) {
      R_K <- ((k - 1) * s + 1):(k * s)
      ualpha <- sqrt(log(K / alpha) / 2)
      zetak <- sapply(pvalues, function(t) {
        (
          sqrt(sum(pvalues[R_K] > t) / (1 - t) +
                 (ualpha / (2 * (1 - t)))^2) +
            ualpha / (2 * (1 - t))
        )^2
      })
      return(min(zetak, length(intersect(R_K, R))))
    }
    VDKW <- floor(sum(sapply(1:K, truc2)))
    
    msg <- sprintf(
      "True nulls smaller than %s (Simes)
                   or %s (part) among %s selected pixels",
      V,
      VDKW,
      length(R)
    )
    # }
    msg
  })
}