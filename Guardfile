# Ruby guard file
# To make sure all the gems are installed, run 'bundle install' once in your browser
# Then you can use the Makefile target 'make gui_dev' to start the GUI in development mode
# For browser livereload to work, need the browser extension: http://livereload.com/extensions/#installing-sections
# If the delay is too short for relaunching the app, increase the grace_period

dir = 'inst/shiny-apps/data_viewer'
data = 'raw_data'
port = 5000

guard 'process', name: 'Shiny', command: ['R', '-e', "isorunN2O:::run_data_viewer_dev(base_dir = '#{data}', app_dir = '#{dir}', port = #{port})"] do
  watch(%r{#{dir}/.+\.R$})
end

guard 'livereload', grace_period: 5 do
  watch(%r{#{dir}/.+\.R$})
end
