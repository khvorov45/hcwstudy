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

# Read schema
mdb-schema "data-raw/serology-landscapes/NIHHCW FLU Landscapes.accdb" |
# Find table names within that schema
grep -oP "CREATE TABLE \[\K.+(?=\])" |
# Export each table into its own file under NIGGCWserol folder
while read line ; do mdb-export "data-raw/serology-landscapes/NIHHCW FLU Landscapes.accdb" "$line" > "data-raw/serology-landscapes/$line.csv" ; done

cp "/home/khvorova/vidrlwhoflu/Group/AF_LC_RT_share/NIH Aust HCW Study/2022 Y3 Serology/InDevr Data/V72 H1 egg/Y3_2023_H1_egg_all.xlsx" \
    "data-raw/serology/Y3_2023_H1_egg_all.xlsx"

cp "/home/khvorova/vidrlwhoflu/Group/AF_LC_RT_share/NIH Aust HCW Study/2022 Y3 Serology/InDevr Data/V76 BVic Egg/BVic_egg_2022_all.xlsx" \
    "data-raw/serology/BVic_egg_2022_all.xlsx"
