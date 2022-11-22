# Use mdbtools to export all tables in csv
# sudo apt-get install mdbtools

# Read schema
mdb-schema data-raw/serology/NIHHCWserol.accdb |
# Find table names within that schema
grep -oP "CREATE TABLE \[\K.+(?=\])" |
# Export each table into its own file under NIGGCWserol folder
while read line ; do mdb-export data-raw/serology/NIHHCWserol.accdb "$line" > "data-raw/serology/$line.csv" ; done

# Read schema
mdb-schema data-raw/serology-covid/NIHHCW_COVID_Serol.accdb |
# Find table names within that schema
grep -oP "CREATE TABLE \[\K.+(?=\])" |
# Export each table into its own file under NIGGCWserol folder
while read line ; do mdb-export data-raw/serology-covid/NIHHCW_COVID_Serol.accdb "$line" > "data-raw/serology-covid/$line.csv" ; done
