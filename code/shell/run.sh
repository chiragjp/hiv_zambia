Rscript xwas_logistic_v6.R -n 'main univariate analysis 2007' -o 'main_univariate_07.Rdata' -f '../data/hiv xwas Zambia07 dataset v7.csv' -s 'univariate_07'
Rscript xwas_logistic_v6.R -n 'main univariate analysis 2013' -o 'main_univariate_13.Rdata' -f '../data/hiv xwas Zambia13 dataset v7.csv' -s 'univariate_13'
Rscript xwas_logistic_v6.R -n 'apriori multivariate 2007' -o 'apriori_adj_07.Rdata' -f '../data/hiv xwas Zambia07 dataset v7.csv' -s 'apriori_adj_07' -a apriori_adjustments.R
Rscript xwas_logistic_v6.R -n 'apriori multivariate 2013' -o 'apriori_adj_13.Rdata' -f '../data/hiv xwas Zambia13 dataset v7.csv' -s 'aprior_adj_13' -a apriori_adjustments.R
Rscript xwas_logistic_v6.R -n 'super multivariate 2013' -o 'super_13.Rdata' -f '../data/hiv xwas Zambia13 dataset v7.csv' -s 'super_13' -a super_adjustments_13.R
Rscript xwas_logistic_v6.R -n 'super multivariate 2007' -o 'super_07.Rdata' -f '../data/hiv xwas Zambia07 dataset v7.csv' -s 'super_07' -a super_adjustments_07.R
