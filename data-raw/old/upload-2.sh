DEVDB=~/Projects/hcwstudyapp/backend-rust/hsf-db-dev/current
cp export-new-serology.json $DEVDB/Serology.json
cp export-new-virus.json $DEVDB/Virus.json

DB=ubuntu@unicloud:/home/ubuntu/hcwstudyapp-2/data/current

scp export-new-serology.json $DB/Serology.json
scp export-new-virus.json $DB/Virus.json
