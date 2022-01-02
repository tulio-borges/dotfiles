#!/bin/bash

# The name of polybar bar which houses the main spotify module and the control modules.
PARENT_BAR="top"
PARENT_BAR_PID=$(pgrep -a "polybar" | grep "$PARENT_BAR" | cut -d" " -f1)

# Set the source audio player here.
# Players supporting the MPRIS spec are supported.
# Examples: spotify, vlc, chrome, mpv and others.
# Use `playerctld` to always detect the latest player.
# See more here: https://github.com/altdesktop/playerctl/#selecting-players-to-control
PLAYER="spotify"

# Format of the information displayed
# Eg. {{ artist }} - {{ album }} - {{ title }}
# See more attributes here: https://github.com/altdesktop/playerctl/#printing-properties-and-metadata
FORMAT="{{ artist }} - {{ title }}"

# Sends $2 as message to all polybar PIDs that are part of $1
update_hooks() {
    while IFS= read -r id
    do
        polybar-msg -p "$id" hook spotify-play-pause $2 1>/dev/null 2>&1
    done < <(echo "$1")
}

PLAYERCTL_STATUS=$(playerctl --player=$PLAYER status 2>/dev/null)
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    STATUS=$PLAYERCTL_STATUS
else
    STATUS="No player is running"
fi

if [ "$1" == "--status" ]; then
    echo "$STATUS"
else
    if [ "$STATUS" = "Stopped" ]; then
        MSG="No music is playing"
    elif [ "$STATUS" = "No player is running"  ]; then
        MSG=$STATUS
    else
        HOOK=$(if [ "$STATUS" = "Paused"  ];then echo 2; else echo 1; fi)
        update_hooks "$PARENT_BAR_PID" $HOOK
        MSG=$(playerctl --player=$PLAYER metadata --format "$FORMAT")
    fi
    BYTES=$(echo $MSG | wc -c)
    CHARS=$(echo $MSG | wc -m)
    MIN_LENGTH=$((30+BYTES-CHARS))
    printf "%-${MIN_LENGTH}s" "$MSG"
fi
