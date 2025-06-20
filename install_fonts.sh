#!/bin/bash

# -----------------------------------------------------------------------------
# Script to install all Nerd Fonts from the official Arch Linux repositories.
#
# WARNING: This script uses 'sudo' and the '--noconfirm' flag, which means
# it will install all listed packages without asking for confirmation.
# Use with caution.
# -----------------------------------------------------------------------------

# Array containing the list of all Nerd Font packages to be installed
nerd_fonts=(
  "ttf-0xproto-nerd"
  "ttf-3270-nerd"
  "ttf-agave-nerd"
  "ttf-anonymouspro-nerd"
  "ttf-arimo-nerd"
  "ttf-bigblueterminal-nerd"
  "ttf-bitstream-vera-mono-nerd"
  "ttf-cascadia-code-nerd"
  "ttf-cascadia-mono-nerd"
  "ttf-cousine-nerd"
  "ttf-d2coding-nerd"
  "ttf-daddytime-mono-nerd"
  "ttf-dejavu-nerd"
  "ttf-envycoder-nerd"
  "ttf-fantasque-nerd"
  "ttf-firacode-nerd"
  "ttf-go-nerd"
  "ttf-gohu-nerd"
  "ttf-hack-nerd"
  "ttf-heavydata-nerd"
  "ttf-iawriter-nerd"
  "ttf-ibmplex-mono-nerd"
  "ttf-inconsolata-go-nerd"
  "ttf-inconsolata-lgc-nerd"
  "ttf-inconsolata-nerd"
  "ttf-intone-nerd"
  "ttf-iosevka-nerd"
  "ttf-iosevkaterm-nerd"
  "ttf-iosevkatermslab-nerd"
  "ttf-jetbrains-mono-nerd"
  "ttf-lekton-nerd"
  "ttf-liberation-mono-nerd"
  "ttf-lilex-nerd"
  "ttf-martian-mono-nerd"
  "ttf-meslo-nerd"
  "ttf-monofur-nerd"
  "ttf-monoid-nerd"
  "ttf-mononoki-nerd"
  "ttf-mplus-nerd"
  "ttf-nerd-fonts-symbols"
  "ttf-nerd-fonts-symbols-mono"
  "ttf-noto-nerd"
  "ttf-profont-nerd"
  "ttf-proggyclean-nerd"
  "ttf-recursive-nerd"
  "ttf-roboto-mono-nerd"
  "ttf-sharetech-mono-nerd"
  "ttf-sourcecodepro-nerd"
  "ttf-space-mono-nerd"
  "ttf-terminus-nerd"
  "ttf-tinos-nerd"
  "ttf-ubuntu-mono-nerd"
  "ttf-ubuntu-nerd"
  "ttf-victor-mono-nerd"
  "ttf-zed-mono-nerd"
)

# Starting message
echo "Starting the installation of all Nerd Fonts..."
echo "A total of ${#nerd_fonts[@]} font packages will be installed."

# Installation command with pacman
# -S: Sync/Install packages
# --noconfirm: Do not ask for confirmation before installation
# "${nerd_fonts[@]}": Expands the array to pass all package names
sudo pacman -S --noconfirm "${nerd_fonts[@]}"

# Check if the installation was successful
if [ $? -eq 0 ]; then
  echo ""
  echo "Installation completed successfully!"
  echo "Refreshing the system's font cache..."
  # Update the font cache to make the new fonts available immediately
  fc-cache -fv
  echo "Process finished."
else
  echo ""
  echo "An error occurred during the pacman installation." >&2
  echo "Please check the pacman output for more details." >&2
  exit 1
fi

exit 0

sudo pacman -S locate
update db
locate somefile