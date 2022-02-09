source ./pull-redcap/vars.sh

curl -X POST \
  -o ./pull-redcap/redcap2022.csv \
  -d token=$TOKEN2022 \
  -d content=record \
  -d format=csv \
  -d exportDataAccessGroups=true \
  -d events=$EVENTS \
  https://biredcap.mh.org.au/api/
