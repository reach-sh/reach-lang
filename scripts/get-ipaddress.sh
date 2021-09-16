#!/bin/sh
#echo $(/usr/sbin/ip a ls $(/usr/sbin/ip r | grep default | awk '{print $NF}') | grep inet\ | awk '{print $2}' | awk -F \/ '{print $1}')
echo /usr/sbin/ifconfig $(route  | grep default | awk '{print $NF}') | grep inet\ | awk '{print $2}' | awk -F \/ '{print $1}'