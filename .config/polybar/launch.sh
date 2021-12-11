#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar

# Launch bar1 and bar2
echo "---" | tee -a /tmp/polybar-top.log /tmp/polybar-bottom.log
polybar top >>/tmp/polybar-top.log 2>&1 & disown
polybar bottom >>/tmp/polybar-bottom.log 2>&1 & disown

echo "Bars launched..."
