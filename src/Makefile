# Define variables
runr = R CMD BATCH --vanilla
runj = julia --project=..

# Define file paths
s_d = src_data
s_e = src_est_cal
s_m = src_model

all: clean_all_output run_data run_est_cal run_model

clean_all_output: clean_output_files clean_figures_tables clean_run_data clean_run_est_cal clean_run_model
	@echo "---Deleted all outputs of the code---"

clean_output_files:
	@echo "---Deleting outputs (cleaned data, estimation/inversion result, model simulation result)---"
	@cd ../output && find ./ -type f -delete

clean_figures_tables:
	@echo "---Deleting figures and tables---"
	@cd ../doc/figures && find ./ -type f -delete
	@cd ../doc/tables && find ./ -type f -delete	
	@cd ../doc/nums && find ./ -type f -delete	

run_data:
	@echo "*** For each code, if it takes more than 5 minutes on the author's local machine (M2 Max Processor with 12-core CPU and 64GB RAM), then I denote the runtime of the code. ***"
	@echo "---Data part---"

	@echo "* Running 'get_country_industry_names.R'"
	@cd ./$(s_d) && $(runr) get_country_industry_names.R get_country_industry_names.out 

	@echo "* Running 'motivation_two_services.R'"
	@cd ./$(s_d) && $(runr) motivation_two_services.R motivation_two_services.out

	@echo "* Running 'condense_icio_remove_tax.R' (This takes approximately 12 minutes on the author's local machine.)"
	@cd ./$(s_d) && $(runr) condense_icio_remove_tax.R condense_icio_remove_tax.out

	@echo "* Running 'extract_pwt.R'"
	@cd ./$(s_d) && $(runr) extract_pwt.R extract_pwt.out

	@echo "* Running 'get_price_deflators.R' (This takes approximately 13 minutes on the author's local machine.)"
	@cd ./$(s_d) && $(runr) get_price_deflators.R get_price_deflators.out

	@echo "* Running 'get_price_level.R'"	
	@cd ./$(s_d) && $(runr) get_price_level.R get_price_level.out

	@echo "* Running 'get_data_cons_prod_trade.R'"	
	@cd ./$(s_d) && $(runr) get_data_cons_prod_trade.R get_data_cons_prod_trade.out

	@echo "* Running 'motivation_trade.R'"	
	@cd ./$(s_d) && $(runr) motivation_trade.R motivation_trade.out

	@echo "* Running 'motivation_str_tran.R'"	
	@cd ./$(s_d) && $(runr) motivation_str_tran.R motivation_str_tran.out

	@echo "* Running 'get_bea_prod.R'"	
	@cd ./$(s_d) && $(runr) get_bea_prod.R get_bea_prod.out

clean_run_data:
	@echo "---Deleting printed outputs of the data-part codes---"
	@cd ./$(s_d) && rm -f *.out

run_est_cal:
	@echo "---Estimation and model inversion part---"

	@echo "* Running 'preference.R' (This takes approximately 9 minutes on the author's local machine.)"
	@cd ./$(s_e) && $(runr) preference.R preference.out

	@echo "* Running 'production.R'"
	@cd ./$(s_e) && $(runr) production.R production.out

	@echo "* Running 'cal_prd_tau.R' (This takes approximately 8 minutes on the author's local machine.)"
	@cd ./$(s_e) && $(runr) cal_prd_tau.R cal_prd_tau.out

	@echo "* Running 'cal_tau_head_ries.R'"
	@cd ./$(s_e) && $(runr) cal_tau_head_ries.R cal_tau_head_ries.out

	@echo "* Running 'analyze_tau.R'"
	@cd ./$(s_e) && $(runr) analyze_tau.R analyze_tau.out

	@echo "* Running 'get_two_country.R'"
	@cd ./$(s_e) && $(runr) get_two_country.R get_two_country.out

clean_run_est_cal:
	@echo "---Deleting printed outputs of the estimation and model inversion codes---"
	@cd ./$(s_e) && rm -f *.out

run_model:
	@echo "---Model simulations---"

	@echo "* Running 'inputs.jl'"
	@cd ./$(s_m) && $(runj) inputs.jl > inputs.out 2>&1

	@echo "* Running 'run_models.jl' (This takes approximately 10 minutes on the author's local machine. This code runs baseline and most of the other model specifications.)"
	@cd ./$(s_m) && $(runj) run_models.jl > run_models.out 2>&1

	@echo "* Running 'clean_result.jl'"
	@cd ./$(s_m) && $(runj) clean_result.jl > clean_result.out 2>&1

	@echo "* Running 'run_usa_row.jl'"
	@cd ./$(s_m) && $(runj) run_usa_row.jl > run_usa_row.out 2>&1

	@echo "* Running 'plot_res_fit.R'"
	@cd ./$(s_m) && $(runr) plot_res_fit.R plot_res_fit.out

	@echo "* Running 'plot_two.R'"
	@cd ./$(s_m) && $(runr) plot_two.R plot_two.out

	@echo "* Running 'analyze_prd.R'"
	@cd ./$(s_m) && $(runr) analyze_prd.R analyze_prd.out


clean_run_model:
	@echo "---Deleting printed outputs of the model simulation codes---"
	@cd ./$(s_m) && rm -f *.out


