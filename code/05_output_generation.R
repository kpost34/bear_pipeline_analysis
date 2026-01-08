# Initialize Filesystem and Export Plots, Summary Table, Model, and Suitcase Functions


# Set Up Function===================================================================================
# Verifies and creates the project directory structure (plots, tables, models) to ensure safe file 
  #export
initialize_filesystem <- function(){
  
  # Create folder dirs
  fp_plot <- here("output", "plots")
  fp_table <- here("output", "tables")
  fp_model <- here("output", "models")
  
  # Create folders if they do not exist
  if(!dir.exists(fp_plot)){
    dir.create(fp_plot, recursive=TRUE)
  }
  
  if(!dir.exists(fp_table)){
    dir.create(fp_table, recursive=TRUE)
  }
  
  if(!dir.exists(fp_model)){
    dir.create(fp_model, recursive=TRUE)
  }
  
  # Print message
  message("File system initialized: Output directories verified.")
}


# Image Export======================================================================================
# Iteratively exports audit and final curve plots as high-resolution PNG and vector-based PDF files
export_plots <- function(suitcase){
  
  # Guard: If no audit plot exists, skip everything
  if(is.null(suitcase$audit_plot)){
    message("Skip: No plots available for export.")
    return(suitcase)
  }
  
  # Extract suitcase objs
  name <- suitcase$dataset_name
  plot_audit <- suitcase$audit_plot
  plot_final <- suitcase$modeling$plot
  
  # Create fp and filenames
  fp_plot <- here("output", "plots")
  
  nm_audit <- "audit_plot"
  nm_final <- "final_curve"
  
  fp_audit_png <- here(fp_plot, paste0(name, "_", nm_audit, ".png"))
  fp_audit_pdf <- here(fp_plot, paste0(name, "_", nm_audit, ".pdf"))
  
  fp_final_png <- here(fp_plot, paste0(name, "_", nm_final, ".png"))
  fp_final_pdf <- here(fp_plot, paste0(name, "_", nm_final, ".pdf"))
  
  # Determine which plots exist to prevent walk2 failure
  if(is.null(plot_final)){
    fps <- c(fp_audit_png, fp_audit_pdf)
    plots <- list(plot_audit, plot_audit)
    msg_count <- "2"
  } else{
    fps <- c(fp_audit_png, fp_audit_pdf, fp_final_png, fp_final_pdf) 
    plots <- rep(list(plot_audit, plot_final), each=2)
    msg_count <- "4"
  }
  
  # Save plots
  purrr::walk2(fps, plots, function(x, y){
    ggsave(filename=x,
           plot=y,
           width=7,
           height=5,
           units="in",
           dpi=300)
  })
  
  # Print message & return suitcase
  plots_msg <- paste("Success:", msg_count, "plot files (PNG/PDF) exported for", 
                     name, "to output/plots/")
  message(plots_msg)
  
  return(suitcase)
  
}



# Export Summary Table==============================================================================
# Formats statistical results with high precision and exports a comprehensive CSV summary to the 
  #tables directory
export_summary <- function(suitcase){
  
  # Guard: Skip if modeling results are missing (Structural or Biological failure)
  if(is.null(suitcase$modeling$results)){
    message("Skip: No summary results available to export.")
    return(suitcase)
  }
  
  # Extract suitcase info
  name <- suitcase$dataset_name
  df_results0 <- suitcase$modeling$results
  
  # Clean up results data
  df_results <- df_results0 %>%
    mutate(across(where(is.numeric), ~round(.x, 4))) %>%
    set_names(
      c("ED50 Estimate", "ED50 Std. Error", "ED50 95% CI (Lower)", "ED50 95% CI (Upper)", 
        "Slope (b)", "Lower Limit (c)", "Upper Limit (d)", "Asymmetry (f)", "AIC", 
        "Pseudo R-Squared", "Slope P-Value", "Residual Std. Error", "Model Type", "Fit Status", 
        "Decision Rationale")
    )
  
  # Save to csv
  fp_table <- here("output", "tables", paste0(name, "_summary_stats.csv"))
  write_csv(x=df_results, file=fp_table)
    
  # Print message & return suitcase
  table_msg <- paste("Success: Summary table exported for",
                     name,
                     "to output/tables/")
  message(table_msg)
  
  return(suitcase)
  
}



# Save Model========================================================================================
# Archives the winning 'drc' or 'lm' model object as an RDS file for future reloading or meta-analysis
save_model_object <- function(suitcase){
  
  # Guard: Skip if no winning model was selected
  if(is.null(suitcase$modeling$winner)){
    message("Skip: No model object available to save.")
    return(suitcase)
  }
  
  # Extract objs from suitcase
  name <- suitcase$dataset_name
  mod <- suitcase$modeling$winner
  
  # Save to rds
  fp_model <- here("output", "models", paste0(name, "_model.rds"))
  saveRDS(object=mod, file=fp_model)
  
  # Print message & return suitcase
  model_msg <- paste("Success: Model object for",
                     name,
                     "saved to output/models/")
  message(model_msg)
  
  return(suitcase)
  
}



# Save Suitcase=====================================================================================
# Serializes the entire analysis state—including data, metadata, and plots—into a single 
  #'full_suitcase' RDS archive
save_suitcase <- function(suitcase){
  
  # Extract obj from suitcase
  name <- suitcase$dataset_name
  
  # Define path
  fp_models <- here("output", "models")
  fp_suitcase <- here(fp_models, paste0(name, "_full_suitcase.rds"))
  
  # Save suitcase
  saveRDS(object=suitcase, file=fp_suitcase)
  
  # Print message & return suitcase
  message("Analysis state has been archived.")
  
  return(suitcase)
                      
}



# ARCHIVE===========================================================================================
# Image Export--------------------------------------------------------------------------------------
# fname_plot_png <- "plot_ryegrass_rootlength.png"
# fp_out_plot_png <- here("output", "plots", fname_plot_png)
# 
# ggsave(filename=fp_out_plot,
#        plot=plot_draft,
#        width=6,
#        height=4,
#        units="in",
#        dpi=300)
# 
# 
# fname_plot_pdf <- "plot_ryegrass_rootlength.pdf"
# fp_out_plot_pdf <- here("output", "plots", fname_plot_pdf)
# 
# 
# ggsave(filename=fp_out_plot_pdf,
#        plot=plot_draft,
#        width=6,
#        height=4,
#        units="in",
#        dpi=300)
# 
# 
# 
# # Export Summary------------------------------------------------------------------------------------
# fname_table <- "table_ryegrass_rootlength.png"
# fp_out_table <- here("output", "tables", fname_table)
# 
# write_csv(df_final_summary, fp_out_table) #after creating df_final_summary
# 
# 
# 
# # Save Model----------------------------------------------------------------------------------------
# fname_model <- "model_ryegrass_rootlength.rds"
# fp_out_model <- here("output", "models", fname_model)
# 
# saveRDS(mod_final, fp_out_model)









