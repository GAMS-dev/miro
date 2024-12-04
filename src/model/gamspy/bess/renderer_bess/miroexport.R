miroexport_Markdown <- function(data, path = NULL, views = NULL, attachments = NULL, metadata = NULL, customRendererDir = NULL, ...) {
    # First, extract the values you want to display.
    total_cost <- data[["_scalars_out"]] %>%
        filter(scalar == "total_cost") %>%
        pull(value) %>%
        as.numeric() %>%
        round(2)

    battery_delivery_rate <- data[["_scalarsve_out"]] %>%
        filter(scalar == "battery_delivery_rate") %>%
        pull(level)

    battery_storage <- data[["_scalarsve_out"]] %>%
        filter(scalar == "battery_storage") %>%
        pull(level)

    output_string <- paste(
        "Our final total cost is: ", total_cost,
        "$\n\nWith a battery power (delivery) rate of ", battery_delivery_rate,
        "kW and a battery energy (storage) rate of ", battery_storage, "kWh."
    )

    # Open a connection to the output file
    file_conn <- file(path, "w")

    # Then write them to the output file.
    writeLines(output_string, file_conn)

    writeLines("\n\n", file_conn)

    # Let's add the generator specifications used
    writeLines("With the following generator specifications:\n\n", file_conn)

    # Extract the table
    table <- data[["generator_specifications"]]

    # Convert the table to a Markdown-style string
    # Create the header
    headers <- paste(names(table), collapse = " | ")
    separator <- paste(rep("---", length(table)), collapse = " | ")
    rows <- apply(table, 1, function(row) paste(row, collapse = " | "))
    
    # Write the table to the file
    writeLines(paste(headers, separator, paste(rows, collapse = "\n"), sep = "\n"), file_conn)

    # Close the file connection
    close(file_conn)

    # If you also want to save the data to a database,
    # you can do that here as well, similar to the import function.
}
