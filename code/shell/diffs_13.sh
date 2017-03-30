Rscript subset_differences.R -y '13' -f '../output/results_output/age1_univariate_13.Rdata,../output/results_output/age2_univariate_13.Rdata,../output/results_output/age3_univariate_13.Rdata' -k 'age1,age2,age3' -o age_diffs_13.Rdata
Rscript subset_differences.R -y '13' -f '../output/results_output/urban_univariate_13.Rdata,../output/results_output/rural_univariate_13.Rdata' -k 'urban,rural' -o location_diffs_13.Rdata
Rscript subset_differences.R -y '13' -f '../output/results_output/never_tested_univariate_13.Rdata,../output/results_output/tested_univariate_13.Rdata' -k 'nevertested,tested' -o hiv_test_diffs_13.Rdata
Rscript subset_differences.R -y '13' -f '../output/results_output/poor_univariate_13.Rdata,../output/results_output/rich_univariate_13.Rdata' -k 'poor,rich' -o poor_rich_diffs_13.Rdata
Rscript subset_differences.R -y '13' -f '../output/results_output/wealth1_univariate_13.Rdata,../output/results_output/wealth2_univariate_13.Rdata,../output/results_output/wealth3_univariate_13.Rdata,../output/results_output/wealth4_univariate_13.Rdata,../output/results_output/wealth5_univariate_13.Rdata' -k 'wealth1,wealth2,wealth3,wealth4,wealth5' -o wealth_diffs_13.Rdata

