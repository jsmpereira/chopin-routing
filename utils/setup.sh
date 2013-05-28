#!/bin/bash
# Jose Santos Martins Pereira jsmpereira@gmail.com
#
# Script for setting up CHOPIN-ROUTING.
#
# - Fetch routing protocol source
# - Generate .config file for protocol
# - Build shared libs (Netlink)
# - Setup iptable rules for topology building

PROJECT_REPO=https://github.com/jsmpereira/chopin-routing.git

function usage(){
    echo "Usage: $0 <iface>"
    exit 1;
}

function add_mac(){
    echo "Enter MAC Address:"
    read MAC
    echo "---> Setting up iptables rule ..."
    sudo iptables -t mangle -A PREROUTING -m mac --mac-source $MAC -j ACCEPT;
    echo "Add another?"
    select yn in "Yes" "No"; do
    case $yn in
        Yes ) add_mac; break;;
        No ) break;;
    esac
    done
}

function setup_iptables() {
    echo "Enabling IP FORWARD ..."
    sudo sysctl -w net.ipv4.ip_forward=1
    echo "---> Setup iptables rules"
    echo "White list MAC addresses"
    add_mac
    echo "---> Dropping packets from everyone else on '$IFACE' ..."
    sudo iptables -t mangle -A PREROUTING -i $IFACE -j DROP;
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

function ip_address() {
    ifconfig $IFACE | grep "inet addr" | awk -F: '{print $2}' | awk '{print $1}' # IP address
}
function broadcast_address() {
    ifconfig $IFACE | grep "inet addr" | awk -F: '{print $3}' | awk '{print $1}' # Bcast address
}
function network_address() {
    ip route | grep $IFACE | awk {'print $1'} | sed -n 2p
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
 :refresh-interval 5
 :dup-hold-time 30
 :neighb-hold-time 3
 :timer-repeat-interval 10)" > ../.config
    echo "Done."
}

function build_shared_libs(){
    echo "---> Building shared libraries ... "
    cd ../c && make clean && make
    cd ../
}

# begin
[[ $# -eq 0 ]] && usage

IFACE=$1

select option in "NTP clock sync" "Update Source" "Build .config file" "Build Shared Libs" "Setup Topology" "Quit"; do
case $option in
    "NTP clock sync" ) clock_sync;;
    "Update Source" ) update_source;;
    "Build .config file" ) build_config_template;;
    "Build Shared Libs" ) build_shared_libs;;
    "Setup Topology" ) setup_iptables;;
    "Quit" ) break;;
    *) echo "Invalid option.";;
esac
done

echo "All done."
