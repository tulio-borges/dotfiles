#!/bin/sh

selection=$(hacksaw -f "-i %i -g %g")
sleep 1
shotgun $selection - | xclip -t 'image/png' -selection clipboard
