rsync -avz -e 'ssh' ./pull-redcap/*.json unicloud:/home/ubuntu/reports-app/pull-redcap
rsync -avz -e 'ssh' ./backend/*.json unicloud:/home/ubuntu/reports-app/backend
