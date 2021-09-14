# Use mdbtools to export all tables in csv
# sudo apt-get install mdbtools

# Read schema
mdb-schema data-raw/NIHHCWserol.accdb |
# Find table names within that schema
grep -oP "CREATE TABLE \[\K.+(?=\])" |
# Export each table into its own file under NIGGCWserol folder
while read line ; do mdb-export data-raw/NIHHCWserol.accdb "$line" > "data-raw/$line.csv" ; done
