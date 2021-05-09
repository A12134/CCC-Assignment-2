. ./unimelb-comp90024-2021-grp-60-openrc.sh; ansible-playbook --ask-become-pass nectar.yaml


ansible-playbook -i inventory.ini -u ubuntu --key-file=./Group60   docker.yaml



# setup couchdb cluster on database servers
ansible-playbook -i inventory.ini -u ubuntu --key-file=./Group60  couchdb.yaml

ansible-playbook -i inventory.ini -u ubuntu --key-file=./Group60  harvaster.yaml