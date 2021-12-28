#!/usr/bin/env bash

# Kill existing bars
killall polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launching polybars
echo "---" | tee -a /tmp/polybar-top.log /tmp/polybar-bottom.log
for m in $(polybar --list-monitors | cut -d":" -f1); do
    MONITOR=$m polybar top >>/tmp/polybar-top.log 2>&1 & disown
    MONITOR=$m polybar bottom >>/tmp/polybar-bottom.log 2>&1 & disown
done

echo "Bars launched..."
