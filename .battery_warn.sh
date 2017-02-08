#!/bin/bash

# Lock the screen and warn if power cable is plugged out
# Helps if the laptop has an almost dead battery
status=$(cat /sys/class/power_supply/BAT0/status)

if [ $status != "Full" ] && [ $status != "Charging" ]; && [ $status != "Unknown"] then
    /usr/bin/i3lock -i /home/ebs/Pictures/battery.png
fi
