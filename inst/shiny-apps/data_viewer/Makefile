# use 'app' to launch the app, 'dev' to launch it in development mode
# (from the sources rather than the installed package), and 'refresh'
# to force refreshing the sources when working in development mode
base_dir 		?= .

dev:
	Rscript -e "isorunN2O:::run_data_viewer_dev(base_dir = '$(base_dir)', app_dir = '.', launch.browser = T)"

app:
	Rscript -e "isorunN2O:::run_data_viewer(base_dir = '$(base_dir)', launch.browser = T)"

refresh:
	touch server.R && touch ui.R
