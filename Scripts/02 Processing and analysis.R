
# Set input ---------------------------------------------------------------

  ### CONFIRM RAW DATA IS IN THE INPUT FOLDER ###
  input_file <- INPUT("iowa_liquor_sales_2018_2021.csv")
  DT <- fread(input_file, data.table = TRUE)


# Cleaning and processing data -----------------------------------------------------------

  # Confirm dates are not missing and follow the MM/DD/YYYY format
  DT[, Valid := grepl("^\\d{2}/\\d{2}/\\d{4}$", Date)]
  stopifnot(all(DT[, Valid]))
  stopifnot(!anyNA(DT[, Date]))

  # Generate cleaned date, month, year, and month year variables 
  DT[, `:=`(Date = as.Date(Date, format = "%m/%d/%Y"))]
  DT[, `:=`(Month = month(Date), Year = year(Date))]
  DT[, `:=`(`Month Year` = sprintf("%02d/%02d", Month, Year %% 100))]
            
  # Generate absolute and percentage markup 
  DT[, `:=`(`Absolute Markup` = `State Bottle Retail` - `State Bottle Cost`,
            `Percentage Markup` = (`State Bottle Retail`- `State Bottle Cost`) / `State Bottle Cost` * 100L)]

  # Clean and standardize store names
  # All apostrophes, exclamation marks, question marks, backslashes followed by anything, hash tags followed by anything, slashes followed by anything, and numbers followed by four or more characters are removed 
  DT[, `Cleaned Store Name` := toupper(str_trim(str_squish(gsub("\\'|\\!\\?|\\\\.*$|#.*$|/.*$|\\d{4}.*", "", `Store Name`))))]
  DT <- DT %>%
    mutate(`Cleaned Store Name` = case_when(
      str_detect(`Cleaned Store Name`, 'HY-VEE') ~ 'HY-VEE',
      str_detect(`Cleaned Store Name`, 'FAREWAY') ~ 'FAREWAY',
      str_detect(`Cleaned Store Name`, 'CASEYS') ~ 'CASEYS',
      str_detect(`Cleaned Store Name`, 'HOMETOWN FOODS') ~ 'HOMETOWN FOODS',
      str_detect(`Cleaned Store Name`, 'KWIK STOP') ~ 'KWIK STOP',
      str_detect(`Cleaned Store Name`, 'LIQUOR BARN') ~ 'LIQUOR BARN',
      str_detect(`Cleaned Store Name`, 'CVS') ~ 'CVS',
      str_detect(`Cleaned Store Name`, 'TARGET') ~ 'TARGET',
      str_detect(`Cleaned Store Name`, 'CENTRAL CITY 2') ~ 'CENTRAL CITY LIQUOR',
      str_detect(`Cleaned Store Name`, 'CENTRAL CITY LIQUOR, INC.') ~ 'CENTRAL CITY LIQUOR',
      TRUE ~ `Cleaned Store Name`  # If no condition matches, retain the original value
    ))

  # Generate standardized liquor category
  DT <- DT %>%
    mutate(`Liquor Category` = case_when(
      `Category Name` == "" ~ "UNKNOWN",
      str_detect(`Category Name`, "WHISKIES|WHISKY|BOURBON|SCOTCH") ~ "WHISKY",
      str_detect(`Category Name`, "RUM") ~ "RUM",
      str_detect(`Category Name`, "VODKA") ~ "VODKA",
      str_detect(`Category Name`, "BRANDIES") ~ "BRANDIES",
      str_detect(`Category Name`, "GIN") ~ "GIN",
      str_detect(`Category Name`, "LIQUEUR|TRIPLE SEC") ~ "LIQUEUR",
      str_detect(`Category Name`, "SCHNAPPS") ~ "SCHNAPPS",
      str_detect(`Category Name`, "TEQUILA") ~ "TEQUILA",
      str_detect(`Category Name`, "SPIRITS|DISTILLERIES") ~ "DISTILLED SPIRITS",
      str_detect(`Category Name`, "SPECIAL ORDER|TEMPORARY") ~ "OTHER",
      TRUE ~ `Category Name`  # If no condition matches, retain the original value
    ))
  
  # Generate translation tables for both store names and liquor categories 
  store_name_tt <- unique(DT[, .(`Store Name`, `Cleaned Store Name`)])
  setorder(store_name_tt, `Store Name`)
  
  liquor_category_tt <- unique(DT[, .(`Liquor Category`, `Category Name`)])
  setorder(liquor_category_tt, `Liquor Category`)
  
  # Save out the raw tables
  out_file <- OUTPUT("Translation tables.xlsx")
  wb <- if (file.exists(out_file)) {
    openxlsx::loadWorkbook(out_file)
  } else {
    openxlsx::createWorkbook()
  }
  worksheets <- list(
    "r.store names tt" = store_name_tt,
    "r.liquor category tt" = liquor_category_tt
  )
  for (wkst in names(worksheets)) {
    if (wkst %in% names(wb)) {
      openxlsx::removeWorksheet(wb, wkst)
    }
    openxlsx::addWorksheet(wb, wkst)
    openxlsx::writeData(wb, wkst, worksheets[[wkst]])
  }
  openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)
  
  # Save total sales data for R Shiny tool
  sale_data <- DT[, .(`Total Sales` = sum(`Sale (Dollars)`)), 
                  by = .(Year, County)]
  saveRDS(sale_data, OUTPUT("Sale Data.rds"))
  

  # Analysis ----------------------------------------------------------------
  
# 1. What impact did COVID have on the overall liquor market in Iowa?
  # a. What trends evolved over the next 3-18 months?
  trend_table <- DT[, .(`Average Sales` = mean(`Sale (Dollars)`),
                                   `Total Sales` = sum(`Sale (Dollars)`),
                                   Volume = sum(`Volume Sold (Liters)`),
                                   `Average Percentage Markup` = mean(`Percentage Markup`)), 
                               by = .(Year, Month)][
                                 order(Year, Month)
                               ]
  
  trend_table[, `MA Total Sales` := zoo::rollmean(`Total Sales`, k = 12L, fill = NA, align = "right")]
  trend_table[, `MA Volume` := zoo::rollmean(Volume, k = 12L, fill = NA, align = "right")]
  
  # Scale total sales and MA total sales (per $ million)
  trend_table[, `Total Sales` := `Total Sales` / 1000000L]
  trend_table[, `MA Total Sales` := `MA Total Sales` /1000000L]
  
  # Scale volume and MA volume (per million L)
  trend_table[, Volume := Volume / 1000000L]
  trend_table[, `MA Volume` := `MA Volume` /1000000L]
  
  # Plot sales trend
  sales_trends_plot <- ggplot(trend_table, aes(x = as.Date(paste(Year, Month, "01", sep = "-")), group = 1)) + 
    geom_line(aes(y = `Total Sales`, color = "Total Sales"), linewidth = 1) + 
    geom_line(aes(y = `MA Total Sales`, color = "12-Month Moving Average"), linewidth = 1, linetype = "dashed") + 
    labs(x = "Date",
      y = "Sales ($ Millions)",
      color = "Legend"
    ) +
    theme_minimal() + 
    scale_color_manual(values = c("Total Sales" = "blue", "12-Month Moving Average" = "red")) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_blank()
    )

  # Plot volume trend
  volumes_trends_plot <- ggplot(trend_table, aes(x = as.Date(paste(Year, Month, "01", sep = "-")), group = 1)) + 
    geom_line(aes(y = Volume, color = "Volume Sold"), linewidth = 1) + 
    geom_line(aes(y = `MA Volume`, color = "12-Month Moving Average"), linewidth = 1, linetype = "dashed") + 
    labs(x = "Date",
      y = "Volume (Millions of Liters)",
      color = "Legend"
    ) +
    theme_minimal() + 
    scale_color_manual(values = c("Volume Sold" = "blue", "12-Month Moving Average" = "red")) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_blank()
    )
  
  # Save out plots
    ggsave(OUTPUT("Total Sales Trends.jpg"), plot = sales_trends_plot, device = "jpeg", width = 10, height = 8)
    ggsave(OUTPUT("Total Volume Trends.jpg"), plot = volumes_trends_plot, device = "jpeg", width = 10, height = 8)
    
    
  # b. Was there a notable shift in the types of products purchased in terms of pack size?
  
    # Generate count of liquor categories by pack over year
    unique_categories_pack <- DT[, .(`Unique categories` = n_distinct(`Liquor Category`)), by = .(Year, Pack)]
    
    # Generate market share by pack and month, year
    market_share_by_year_pack <- DT %>%
      group_by(Year, Month, Pack) %>%
      summarise(`Total sales` = sum(`Sale (Dollars)`)) %>%  
      ungroup() %>%
      group_by(Year, Month) %>%
      mutate(`Year month total sales` = sum(`Total sales`), 
             `Market share` = `Total sales` / `Year month total sales` * 100L) %>% 
      ungroup()

    # Filter to the top 2 common pack size (6 and 12 pack)
    filtered_data <- market_share_by_year_pack %>%
      filter(Pack %in% c(6L, 12L))
    filtered_data$Pack <- as.factor(filtered_data$Pack)
    
    # Plot market share for 6 and 12 pack
    market_share_6_12_pack <- ggplot(filtered_data, aes(as.Date(paste(Year, Month, "01", sep = "-")), y = `Market share`, color = `Pack`)) +
      geom_line() +
      labs(x = "Date",
           y = "Market Share (%)",
           color = "Pack Size") +
      theme_minimal()
  
    # Save out liquor categories table and market share plot
    out_file <- OUTPUT("Count of categories by pack.xlsx")
        wb <- if (file.exists(out_file)) {
          openxlsx::loadWorkbook(out_file)
        } else {
          openxlsx::createWorkbook()
        }
        wkst <- "r.count of categories tt"
        if (wkst %in% names(wb)) {
          openxlsx::removeWorksheet(wb, wkst)
        }
        openxlsx::addWorksheet(wb, wkst)
        openxlsx::writeData(wb, wkst, unique_categories_pack)
        openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)
    
    ggsave(OUTPUT("Market share (6 & 12 pack).jpg"), plot = market_share_6_12_pack, device = "jpeg", width = 10, height = 8)

  
# 2. Which are the fastest growing types of liquor (e.g., vodka, tequila, rum, etc.)? How has market share changed over time?
  #  a. Write a function that takes a list of liquor types as an input and visualizes the market share over time for each of those.
    calculate_market_share <- function(data, liquor) {
      # Ensure the data is a data.table
      setDT(data)
      
      # Calculate total sales by month and year
      total_sales <- data[, .(`Total sales` = sum(`Sale (Dollars)`)), by = .(Year, Month)]
      
      # Calculate sales by liquor category
      sales_by_liquor <- data[, .(`Sale by category` = sum(`Sale (Dollars)`)), by = .(Year, Month, `Liquor Category`)]
      
      # Filter to liquor category parameter
      sales_by_liquor <- sales_by_liquor[`Liquor Category` %in% liquor]
      
      # Generate market share
      merged_data <- merge(total_sales, sales_by_liquor, by = c("Year", "Month")) 
      merged_data[, `:=` (`Market share` = (`Sale by category`/`Total sales`) * 100L)]
      
      # Generate plot
      plot <- ggplot(merged_data, aes(x = as.Date(paste(Year, Month, "01", sep = "-")), y = `Market share`, color = `Liquor Category`)) +
        geom_line() +
        labs(x = "Date", y = "Moving Average Market Share (%)") +
        theme_minimal()
      
      return(plot)
    }
  
    # Calculate CAGR for each liquor type
    cagr_data <- DT[, .(`Total sales` = sum(`Sale (Dollars)`)), by = .(Year, `Liquor Category`)]
    cagr_date <- cagr_data[Year != 2017L]
    cagr_results <- cagr_data[, .(CAGR = (last(`Total sales`) / first(`Total sales`))^(1 / (max(Year) - min(Year))) - 1), by = `Liquor Category`]
    setorder(cagr_results, -CAGR)
    
    # Save out CAGR table
    out_file <- OUTPUT("CAGR by category.xlsx")
      wb <- if (file.exists(out_file)) {
        openxlsx::loadWorkbook(out_file)
      } else {
        openxlsx::createWorkbook()
      }
      wkst <- "r.cagr"
      if (wkst %in% names(wb)) {
        openxlsx::removeWorksheet(wb, wkst)
      }
      openxlsx::addWorksheet(wb, wkst)
      openxlsx::writeData(wb, wkst, cagr_results)
      openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)

    
  # b. What is driving the growth in tequila sales (price, volume sold, distribution, etc.)?
    # Capture totals sales and volume 
    tequila_data <- DT[,  .(`Total sales` = sum(`Sale (Dollars)`), `Total consumption` = sum(`Volume Sold (Liters)`)), by = .(Year, Month, `Liquor Category`)]
    tequila_data <- tequila_data[`Liquor Category` == "TEQUILA"]
    tequila_data <- tequila_data[order(Year, Month)]
    tequila_data <- tequila_data[,`:=` (Price = `Total sales`/`Total consumption`)]
    tequila_data <- tequila_data[, `MA Price` := zoo::rollmean(Price, k = 12L, fill = NA, align = "right")]
    tequila_data <- tequila_data[, `MA Consumption` := zoo::rollmean(`Total consumption`, k = 12L, fill = NA, align = "right")]

    # Filter out rows where MA Price is NA
    tequila_data <- tequila_data[!is.na(`MA Price`) | !is.na(`MA Consumption`)]
    
    # Generate plot
    plot_tequila_price <- ggplot(tequila_data, aes(x = as.Date(paste(Year, Month, "01", sep = "-")), y = `MA Price`)) +
      geom_line(color = "red") +
      labs(x = "Date", y = "Price ($/L)") +
      theme_minimal()

    plot_tequila_consumption <- ggplot(tequila_data, aes(x = as.Date(paste(Year, Month, "01", sep = "-")), y = `MA Consumption`, color = `Liquor Category`)) +
      geom_line() +
      labs(x = "Date", y = "Consumption (L)") +
      theme_minimal()
    
    # Save out tequila trends graphs
    ggsave(OUTPUT("Tequila Price Trend.jpg"), plot = plot_tequila_price, device = "jpeg", width = 10, height = 8)
    ggsave(OUTPUT("Tequila Consumption Trend.jpg"), plot = plot_tequila_consumption, device = "jpeg", width = 10, height = 8)
    

  
# 3. In late 2019 Heaven Hill Brands bought a portfolio of liquor brands from Constellation Brands. What impact did this have on Heaven Hill’s growth?
  # a. What percentage of their growth in 2020 can be attributed to the acquisition?
    
    # Generate translation table portfolio of liquor brands
    vendor_combinations <- DT[`Vendor Name` == "CONSTELLATION BRANDS INC" | `Vendor Name` == "HEAVEN HILL BRANDS"]
    vendor_combinations <- vendor_combinations[, .N, by = .(`Item Description`, `Vendor Name`)][, .N, by = `Item Description`]

    # Merge based on a common column (e.g., 'Item Description')
    merged_data <- merge(DT, vendor_combinations, by = "Item Description")
    merged_data <- merged_data[, .(`Total sales` = sum(`Sale (Dollars)`)), by = .(Year, `Vendor Name`, N)]
    total <- DT[, .(`Total sales` = sum(`Sale (Dollars)`)), by = .(Year, `Vendor Name`)]
    
    # Create results table
    my_table <- data.table(
        "2019 Sales" = c(merged_data[`Vendor Name` == "HEAVEN HILL BRANDS" & N == 1L & Year == 2019L, `Total sales`], 
                         merged_data[`Vendor Name` == "HEAVEN HILL BRANDS" & N == 2L & Year == 2019L, `Total sales`], 
                         total[`Vendor Name` == "HEAVEN HILL BRANDS" & Year == 2019L, `Total sales`] 
                         ),
        "2020 Sales" = c(merged_data[`Vendor Name` == "HEAVEN HILL BRANDS" & N == 1L & Year == 2020L, `Total sales`],
                         merged_data[`Vendor Name` == "HEAVEN HILL BRANDS" & N == 2L & Year == 2020L, `Total sales`], 
                         total[`Vendor Name` == "HEAVEN HILL BRANDS" & Year == 2020L, `Total sales`]  
                        )
                         
                        )

    out_file <- OUTPUT("Heaven Hills translation table.xlsx")
      wb <- if (file.exists(out_file)) {
        openxlsx::loadWorkbook(out_file)
      } else {
        openxlsx::createWorkbook()
      }
      wkst <- "r.heaven hills tt"
      if (wkst %in% names(wb)) {
        openxlsx::removeWorksheet(wb, wkst)
      }
      openxlsx::addWorksheet(wb, wkst)
      openxlsx::writeData(wb, wkst, vendor_combinations[N==2L])
      openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)


    out_file <- OUTPUT("Heaven Hills sales.xlsx")
      wb <- if (file.exists(out_file)) {
        openxlsx::loadWorkbook(out_file)
      } else {
        openxlsx::createWorkbook()
      }
      wkst <- "r.heaven hills sales"
      if (wkst %in% names(wb)) {
        openxlsx::removeWorksheet(wb, wkst)
      }
      openxlsx::addWorksheet(wb, wkst)
      openxlsx::writeData(wb, wkst, my_table)
      openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)


  
# 4. Grouping individual store brands together (e.g., all of Walmart, Liquor Barn, Hy-Vee, etc.), who are the top 10 retailers by year?
  # Generate total volume and sales amount by year
    top_ten_retailers_sales <- DT[, .(`Total Sales` = sum(`Sale (Dollars)`)), by = .(Year, `Cleaned Store Name`)][
      order(-`Total Sales`), head(.SD, 10L), by = Year
    ]
    top_ten_retailers_volume <- DT[, .(`Total Volume` = sum(`Volume Sold (Liters)`)), by = .(Year, `Cleaned Store Name`)][
      order(-`Total Volume`), head(.SD, 10L), by = Year
    ]
    
    # Sort top ten retailers by total sale's amount 
    outwide_sales <- data.table() 
    for (year in unique(top_ten_retailers_sales[, Year])) {
      current_year_data <- top_ten_retailers_sales[Year == year]
      current_year_data[, Year := year][
        order(Year, -`Total Sales`)
      ]
      outwide_sales <- cbind(outwide_sales, current_year_data, fill = TRUE)
    }
    outwide_sales <- outwide_sales[, .SD, .SDcols = patterns("Cleaned Store Name")]

    # Sort top ten retailers by total volume amount 
    outwide_volume <- data.table() 
    for (year in unique(top_ten_retailers_volume[, Year])) {
      current_year_data <- top_ten_retailers_volume[Year == year]
      current_year_data[, Year := year][
        order(Year, -`Total Volume`)
      ]
      outwide_volume <- cbind(outwide_volume, current_year_data, fill = TRUE)
    }
    outwide_volume <- outwide_volume[, .SD, .SDcols = patterns("Cleaned Store Name")]
    
    # Save sorted top ten retailers
    out_file <- OUTPUT("Top 10 retailers (sales).xlsx")
    wb <- if (file.exists(out_file)) {
      openxlsx::loadWorkbook(out_file)
    } else {
      openxlsx::createWorkbook()
    }
    wkst <- "r.top ten retailers (sales)"
    if (wkst %in% names(wb)) {
      openxlsx::removeWorksheet(wb, wkst)
    }
    openxlsx::addWorksheet(wb, wkst)
    openxlsx::writeData(wb, wkst, outwide_sales)
    openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)

    out_file <- OUTPUT("Top 10 retailers (volume).xlsx")
    wb <- if (file.exists(out_file)) {
      openxlsx::loadWorkbook(out_file)
    } else {
      openxlsx::createWorkbook()
    }
    wkst <- "r.top ten retailers (volume)"
    if (wkst %in% names(wb)) {
      openxlsx::removeWorksheet(wb, wkst)
    }
    openxlsx::addWorksheet(wb, wkst)
    openxlsx::writeData(wb, wkst, outwide_volume)
    openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)
    
   