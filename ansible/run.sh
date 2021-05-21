
. ./unimelb-comp90024-2021-grp-60-openrc.sh;
# install docker on the remote servers


# ansible-playbook -i inventory.ini -u ubuntu --key-file=./Group60   docker.yaml



# # setup couchdb cluster on database servers
# ansible-playbook -i inventory.ini -u ubuntu --key-file=./Group60  couchdb.yaml

#ansible-playbook -i inventory.ini -u ubuntu --key-file=./Group60  run-harvaster.yaml



ansible-playbook -i inventory.ini -u ubuntu --key-file=./Group60  frontend.yaml