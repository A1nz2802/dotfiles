#!/bin/bash

VERSION="v3.4.0"

# Installation directory (Local user scope to avoid system pollution)
FONT_DIR="/usr/local/share/fonts/NerdFonts"
TEMP_DIR="/tmp/nerdfonts_downloads"

# Colors for pretty output
GREEN="\e[32m"
BLUE="\e[34m"
RED="\e[31m"
RESET="\e[0m"

# --- START ---
echo -e "${BLUE}=== Mass Nerd Fonts Installer (${VERSION}) ===${RESET}"
echo -e "${BLUE}Target directory: ${FONT_DIR}${RESET}"

# 1. Create directories
mkdir -p "$FONT_DIR"
mkdir -p "$TEMP_DIR"

# 2. Check dependencies
if ! command -v curl &> /dev/null || ! command -v jq &> /dev/null; then
    echo -e "${RED}[!] Error: You need 'curl' and 'jq' installed.${RESET}"
    echo "Run: sudo apt install curl jq"
    exit 1
fi

# 3. Fetch download list from GitHub API
echo -e "${BLUE}[*] Fetching font list for version ${VERSION}...${RESET}"
API_URL="https://api.github.com/repos/ryanoasis/nerd-fonts/releases/tags/${VERSION}"

# Filter for .tar.xz files (lighter than zips) excluding source code/checksums
URLS=$(curl -s "$API_URL" | jq -r '.assets[] | select(.name | endswith(".tar.xz")) | .browser_download_url')

if [ -z "$URLS" ]; then
    echo -e "${RED}[!] No fonts found. Check the version number or your internet connection.${RESET}"
    exit 1
fi

COUNT=$(echo "$URLS" | wc -l)
echo -e "${GREEN}[+] Found ${COUNT} font families.${RESET}"

# 4. Confirmation (Crucial as this involves downloading many files)
read -p "You are about to download and install ${COUNT} font families. Continue? (y/n): " confirm
if [[ $confirm != "y" && $confirm != "Y" ]]; then
    echo "Aborted."
    exit 0
fi

# 5. Download and Install Loop
cd "$TEMP_DIR"
i=0
for url in $URLS; do
    ((i++))
    filename=$(basename "$url")
    echo -e "${BLUE}[${i}/${COUNT}] Downloading: ${filename}...${RESET}"
    
    # Download
    curl -L -O "$url" --progress-bar
    
    # Extract directly to the fonts directory
    echo -e "    -> Extracting to ${FONT_DIR}..."
    tar -xf "$filename" -C "$FONT_DIR"
    
    # Remove the downloaded archive to save space in /tmp
    rm "$filename"
done

# 6. Final Cleanup and Cache Update
echo -e "${BLUE}[*] Cleaning up temporary files and regenerating font cache...${RESET}"
rm -rf "$TEMP_DIR"
fc-cache -fv

echo -e "${GREEN}=== INSTALLATION COMPLETE! ===${RESET}"
