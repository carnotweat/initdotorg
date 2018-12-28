# file: get-work-done.sh 
# run: sh get-work-done.sh

# Uses emacs to extract the DONE items from work.org, generating a work-done.csv
emacs -batch -l ~/.emacs.d/init.el -eval '(org-batch-agenda-csv "+TODO=\"DONE\"" org-agenda-files (quote ("/../org-pack/work.org")))' > work-done.csv

# Applies desired report formatting to the exported work-done.csv, generating work.csv
ruby format-report.rb work-done.csv

# Clean up the originally exported file
rm work-done.csv

# Opens the final file in the default .csv handler, typically Excel.
open work.csv
