## makefile for DPH covid case and hospitalization analyses

download: download_ma download_ca

download_ma:
	Rscript code/download-data.R
	Rscript code/convert-data.R

download_ca:
	Rscript code/create_CA_data_links.R
	Rscript code/download-data-ca.R

