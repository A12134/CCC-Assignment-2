
. ./unimelb-comp90024-2021-grp-60-openrc.sh;

# reset the enviroment of the remote servers
# ansible-playbook -i inventory.ini -u ubuntu --key-file=./Group58 -v environment-reset.yml

# setup couchdb cluster on database servers
ansible-playbook -i inventory.ini -u ubuntu --key-file=./Group60  couchdb.yaml