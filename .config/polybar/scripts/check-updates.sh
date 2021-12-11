#!/bin/sh
updates=$(checkupdates 2>/dev/null | wc -l)
if [[ $updates = "0" ]]; then
	exit 0
fi
echo "${updates} updates available"
