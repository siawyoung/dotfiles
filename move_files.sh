#!/bin/bash

SOURCE_DIR="/Users/sy/Downloads/obsidian"
DEST_DIR="$HOME/Documents/Obsidian Vault/links"

while true; do
  find "$SOURCE_DIR" -type f -not -path '*/\.*' -exec bash -c 'mkdir -p "$3/$(dirname "${1#$2}")" && mv "$1" "$3/$(dirname "${1#$2}")"' _ {} "$SOURCE_DIR" "$DEST_DIR" \;
  sleep 5
done
