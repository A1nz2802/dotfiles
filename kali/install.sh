#!/bin/bash

# update system

# install dependencies: sudo apt install git kitty zsh curl rofi trayer xmonad xmobar

# clone repo: https://github.com/A1nz2802/dotfiles.git

# install nerdfonts: execute font install script in kali/install_fonts.sh from my dotfiles repo

# copy my apps config to .config directory

# install starship prompt, use this: curl -sS https://starship.rs/install.sh | sh

# --- 1. Safety & Error Handling ---
# Exit immediately if a command exits with a non-zero status
set -e

# Set up colors
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Error Trap: Catch failures and print the line number
trap 'echo -e "\n${RED}[CRITICAL ERROR]${NC} The script failed on line $LINENO. Exiting."; exit 1' ERR

# --- 2. Helper Functions ---

# Function to print step headers (acts as progress indicator)
print_step() {
    echo -e "\n${BLUE}======================================${NC}"
    echo -e "${YELLOW} PROGRESS: $1 ${NC}"
    echo -e "${BLUE}======================================${NC}"
}

# Function for success messages
success() {
    echo -e "${GREEN}[OK]${NC} $1"
}

# Function for info messages
info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

# --- 3. Paths & Variables ---
# Assuming script is run from inside the repo (e.g., ~/dotfiles/kali/install.sh)
# We resolve the parent directories to find the root.
KALI_DIR="kali"
DOTFILES_DIR="$(dirname "$KALI_DIR")"
COMMON_DIR="$DOTFILES_DIR/common"
CONFIG_DIR="$HOME/.config"

# --- 4. Main Installation Steps ---

step_1_update_system() {
    print_step "Step 1/5: Updating System Packages"
    info "Running apt update and upgrade. This might take a while..."
    # Using DEBIAN_FRONTEND=noninteractive to avoid some prompts during upgrade
    sudo DEBIAN_FRONTEND=noninteractive apt update -y
    sudo DEBIAN_FRONTEND=noninteractive apt upgrade -y
    success "System updated successfully."
}

step_2_install_dependencies() {
    print_step "Step 2/5: Installing Core Dependencies"
    
    # List taken from your image (image_2.png line 3)
    DEPENDENCIES=(
        "git"
        "kitty"
        "zsh"
        "curl"
        "rofi"
        "trayer"
        "xmonad"
        "xmobar"
        "build-essential" # Added for general compilation needs
    )

    info "Installing packages: ${DEPENDENCIES[*]}"
    sudo DEBIAN_FRONTEND=noninteractive apt install -y "${DEPENDENCIES[@]}"
    success "Dependencies installed."
}

step_3_install_fonts() {
    print_step "Step 3/5: Installing NerdFonts"
    
    FONT_SCRIPT="$KALI_DIR/install_fonts.sh"

    if [ -f "$FONT_SCRIPT" ]; then
        info "Found font script. Executing..."
        chmod +x "$FONT_SCRIPT"
        # Executing the external script. If it fails, 'set -e' will trigger here.
        "$FONT_SCRIPT"
        success "NerdFonts installation complete."
    else
        echo -e "${RED}[ERROR]${NC} Font installation script not found at: $FONT_SCRIPT"
        # Manually exiting 1 just in case, though set -e usually catches missing files too.
        exit 1
    fi
}

step_4_link_configs() {
    print_step "Step 4/5: Linking Configurations (Common)"
    
    mkdir -p "$CONFIG_DIR"
    info "Linking configs from $COMMON_DIR to $CONFIG_DIR..."

    # 1. Link Directories shown in image_3.png
    # Using an array for easy additions later
    DIRS_TO_LINK=("alacritty" "kitty" "rofi")

    for dir in "${DIRS_TO_LINK[@]}"; do
        SOURCE="$COMMON_DIR/$dir"
        TARGET="$CONFIG_DIR/$dir"
        
        if [ -d "$SOURCE" ]; then
            # ln -sfn: symlink, force (overwrite existing links), no-dereference
            ln -sfn "$SOURCE" "$TARGET"
            info "Linked directory: $dir"
        else
            echo -e "${YELLOW}[WARN]${NC} Source directory not found, skipping: $SOURCE"
        fi
    done

    # 2. Link standalone files (like starship.toml)
    if [ -f "$COMMON_DIR/starship.toml" ]; then
        ln -sf "$COMMON_DIR/starship.toml" "$CONFIG_DIR/starship.toml"
        info "Linked file: starship.toml"
    fi

    # 3. Link Kali-specific ZSH setup (Optional phase for later, but good to have ready)
    if [ -f "$KALI_DIR/.zshrc" ]; then
         # Backup existing zshrc if it's a real file
         [ -f "$HOME/.zshrc" ] && [ ! -L "$HOME/.zshrc" ] && mv "$HOME/.zshrc" "$HOME/.zshrc.bak"
         ln -sf "$KALI_DIR/.zshrc" "$HOME/.zshrc"
         info "Linked Kali .zshrc"
    fi

    success "Configurations linked."
}

step_5_install_starship() {
    print_step "Step 5/5: Installing Starship Prompt"
    
    if command -v starship &> /dev/null; then
        info "Starship is already installed. Skipping."
    else
        info "Downloading and running Starship installer script..."
        # Using sudo to install globally to /usr/local/bin, requiring less path config
        curl -sS https://starship.rs/install.sh | sudo sh -s -- --yes
        success "Starship installed."
    fi
}

# --- 5. Execution Flow ---

main() {
    # Ensure sudo permissions upfront
    sudo -v

    step_1_update_system
    step_2_install_dependencies
    step_3_install_fonts
    step_4_link_configs
    step_5_install_starship

    echo -e "\n${GREEN}======================================${NC}"
    echo -e "${GREEN}  INSTALLATION COMPLETE!  ${NC}"
    echo -e "${GREEN}======================================${NC}"
    echo -e "Please restart your session or reboot to apply all changes."
    echo -e "If you installed zsh for the first time, you might need to change default shell: chsh -s \$(which zsh)"
}

# Run main function
main
