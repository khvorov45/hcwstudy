source ./pull-redcap/vars.sh

curl -X POST \
  -o ./pull-redcap/redcap2020.csv \
  -d token=$TOKEN2020 \
  -d content=record \
  -d format=csv \
  -d exportDataAccessGroups=true \
  -d events=$EVENTS \
  https://biredcap.mh.org.au/api/
