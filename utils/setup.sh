#!/bin/bash
# Jose Santos Martins Pereira jsmpereira@gmail.com
#
# Script for setting up CHOPIN-ROUTING.
#
# - Update clock with NTP
# - Fetch routing protocol source
# - Generate .config file for protocol
# - Build shared libs (Netlink)
# - Setup iptable rules for topology building
#   - For file based functionality, the script expects a file
#     named 'mac_whitelist' with one MAC address per line.
#   - Otherwise interactive input of MAC address is supplied.
# - Wipe iptable rules

PROJECT_REPO=https://github.com/CHOPIN-ISR-Coimbra/MANET.git

function usage(){
    echo "Usage: $0 <iface>"
    exit 1;
}

function ip_address() {
    ifconfig $IFACE | grep "inet addr" | awk -F: '{print $2}' | awk '{print $1}' # IP address
}
function broadcast_address() {
    ifconfig $IFACE | grep "inet addr" | awk -F: '{print $3}' | awk '{print $1}' # Bcast address
}
function network_address() {
    ip route | grep $IFACE | awk {'print $1'} | sed -n 2p
}

function filter_by_mac(){
    echo "---> Setting up iptables rule ...$2 $1"
    [[ $2 == "-D" ]] && idx='' || idx=1
    sudo iptables -t mangle $2 PREROUTING $idx -m mac --mac-source $1 -j ACCEPT;
}

function add_mac(){
    echo "Enter MAC Address:"
    read MAC
    filter_by_mac $MAC -I
    echo "Add another?"
    select yn in "Yes" "No"; do
    case $yn in
        Yes ) add_mac; break;;
        No ) break;;
    esac
    done
}

function clean_iptables() {
    echo "---> Removing iptables rules"
    sudo iptables -t mangle -F
}

function drop_from_all_iptables() {
    echo "---> Dropping packets from everyone else on '$IFACE' ..."
    sudo iptables -t mangle -A PREROUTING -i $IFACE -j DROP;
}

function setup_iptables() {
    echo "Enabling IP FORWARD ..."
    sudo sysctl -w net.ipv4.ip_forward=1
    echo "---> Setup iptables rules"
    echo "Select Topology:"
    select top in "1" "2" "3.1" "3.2" "3.3" "4" "5" "Back"; do
	case $top in
	    Back ) break;;
	    "3.2" | "3.3" ) update_test_topology $top;;
	    *) read_test_file $top;;
	esac
    done
}

function update_test_topology () {
    file="mac_whitelist_$1";
    out=$(awk -v ip=$(ip_address) '{ RS= ""; OFS="\n"} $0 ~ ip' $file | awk 'NR>1 {print}')
    mac1=$(echo $out |awk '{print $1}')
    mac2=$(echo $out |awk '{print $2}')
    if [ "$mac1" != "NIL" ]; then
       filter_by_mac $mac1 -I;
    fi
    if [ "$mac2" != "NIL" ]; then
       filter_by_mac $mac2 -D;
    fi
}

function read_test_file() {
    file="mac_whitelist_$1";
    if [ -e $file ]; then
	echo "---> Reading from file for $ip"
	awk -v ip=$(ip_address) '{ RS= ""; OFS="\n"} $0 ~ ip' $file | awk 'NR>1 {print}' \
	| while read -e line; do
	    filter_by_mac $line -I;
	done
    else
	add_mac
    fi
    drop_from_all_iptables; 
}

function update_source(){
    echo "---> Fecthing updated source from Github ..."
    git pull $PROJECT_REPO
}

function clock_sync(){
    echo "---> Halting NTP server ..."
    sudo service ntp stop
    echo "---> Syncing clock ..."
    sudo ntpdate ntp.ubuntu.com
    echo "---> Restarting NTP server ..."
    sudo service ntp start
}

function build_config_template() {
    echo "---> Retrieving Inet and Bcast Address on '$IFACE' ..."
    inet_addr=$(ip_address)
    bcast_addr=$(broadcast_address)
    echo " ... inet: $inet_addr | bcast: $bcast_addr"
    echo "Generating .config file ..."
    echo "(:interface \"$IFACE\"
 :host-address \"$inet_addr\"
 :broadcast-address \"$bcast_addr\"
 :port 269
 :hop-limit 255
 :refresh-interval 2
 :dup-hold-time 30
 :neighb-hold-time 6 ; 3 * refresh-interval
 :timer-repeat-interval 10)" > ../.config
    echo "Done."
}

function build_shared_libs(){
    echo "---> Building shared libraries ... "
    cd ../c && make clean && make
    cd ../utils
}

# begin
[[ $# -eq 0 ]] && usage

IFACE=$1

select option in "NTP clock sync" "Update Source" "Build .config file" "Build Shared Libs" "Setup Topology" "Wipe Topology" "Quit"; do
case $option in
    "NTP clock sync" ) clock_sync;;
    "Update Source" ) update_source;;
    "Build .config file" ) build_config_template;;
    "Build Shared Libs" ) build_shared_libs;;
    "Setup Topology" ) setup_iptables;;
    "Wipe Topology" ) clean_iptables;;
    "Quit" ) break;;
    *) echo "Invalid option.";;
esac
done

echo "All done."
