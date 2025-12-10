#!/bin/bash

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
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
KALI_DIR="$SCRIPT_DIR"
DOTFILES_DIR="$(dirname "$KALI_DIR")"
COMMON_DIR="$DOTFILES_DIR/common"
CONFIG_DIR="$HOME/.config"

# --- 4. Main Installation Steps ---

step_1_update_system() {
    print_step "Step 1/6: Updating System Packages"
    info "Running apt update and upgrade. This might take a while..."
    sudo DEBIAN_FRONTEND=noninteractive apt update -y
    sudo DEBIAN_FRONTEND=noninteractive apt upgrade -y
    success "System updated successfully."
}

step_2_install_dependencies() {
    print_step "Step 2/6: Installing Core Dependencies"
    
    DEPENDENCIES=(
        "git"
        "kitty"
        "zsh"
        "curl"
        "jq"
        "rofi"
        "trayer"
        "xmonad"
        "xmobar"
        "build-essential"
        "nodejs"
        "npm"
    )

    info "Installing packages: ${DEPENDENCIES[*]}"
    sudo DEBIAN_FRONTEND=noninteractive apt install -y "${DEPENDENCIES[@]}"
    success "Dependencies installed."
}

step_3_install_fonts() {
    print_step "Step 3/6: Installing NerdFonts"
    
    FONT_SCRIPT="$KALI_DIR/install_fonts.sh"

    if [ -f "$FONT_SCRIPT" ]; then
        info "Found font script. Executing..."
        chmod +x "$FONT_SCRIPT"
        "$FONT_SCRIPT"
        success "NerdFonts installation complete."
    else
        echo -e "${YELLOW}[WARN]${NC} Font script not found at $FONT_SCRIPT. Skipping."
    fi
}

step_4_link_configs() {
    print_step "Step 4/6: Linking Configurations"
    
    mkdir -p "$CONFIG_DIR"
    info "Linking configs from $COMMON_DIR to $CONFIG_DIR..."

    # 1. Link Common Directories (alacritty, kitty, rofi)
    DIRS_TO_LINK=("alacritty" "kitty" "rofi")

    for dir in "${DIRS_TO_LINK[@]}"; do
        SOURCE="$COMMON_DIR/$dir"
        TARGET="$CONFIG_DIR/$dir"
        
        if [ -d "$SOURCE" ]; then
            ln -sfn "$SOURCE" "$TARGET"
            info "Linked directory: $dir"
        else
            echo -e "${YELLOW}[WARN]${NC} Source directory not found, skipping: $SOURCE"
        fi
    done

    # 2. Link Common Files (starship.toml)
    if [ -f "$COMMON_DIR/starship.toml" ]; then
        ln -sf "$COMMON_DIR/starship.toml" "$CONFIG_DIR/starship.toml"
        info "Linked file: starship.toml"
    fi

    # 3. Link HOME files (.zshrc and .xprofile)
    info "Linking Home Directory files (.zshrc, .xprofile)..."
    
    HOME_FILES=(".zshrc" ".xprofile")

    for file in "${HOME_FILES[@]}"; do
        SOURCE_FILE="$KALI_DIR/$file"
        TARGET_FILE="$HOME/$file"

        if [ -f "$SOURCE_FILE" ]; then
            if [ -f "$TARGET_FILE" ] && [ ! -L "$TARGET_FILE" ]; then
                mv "$TARGET_FILE" "${TARGET_FILE}.bak"
                info "Backed up existing $file to ${file}.bak"
            fi
            
            ln -sf "$SOURCE_FILE" "$TARGET_FILE"
            success "Linked $file -> $HOME/$file"
        else
             echo -e "${YELLOW}[WARN]${NC} $file not found in $KALI_DIR. Skipping."
        fi
    done
}

step_5_install_starship() {
    print_step "Step 5/6: Installing Starship Prompt"
    
    if command -v starship &> /dev/null; then
        info "Starship is already installed. Skipping."
    else
        info "Downloading and running Starship installer script..."
        curl -sS https://starship.rs/install.sh | sudo sh -s -- --yes
        success "Starship installed."
    fi
}

step_6_setup_neovim() {
    print_step "Step 6/6: Setting up Neovim Configuration"
    
    NVIM_CONFIG_DIR="$HOME/.config/nvim"
    REPO_URL="https://github.com/A1nz2802/nvim.git"

    # Backup existing nvim config if present
    if [ -d "$NVIM_CONFIG_DIR" ]; then
        info "Existing Neovim config found. Backing up to ${NVIM_CONFIG_DIR}.bak"
        # Removing old backup if exists to avoid conflicts, or use timestamp
        rm -rf "${NVIM_CONFIG_DIR}.bak"
        mv "$NVIM_CONFIG_DIR" "${NVIM_CONFIG_DIR}.bak"
    fi

    info "Cloning Neovim config from $REPO_URL..."
    git clone "$REPO_URL" "$NVIM_CONFIG_DIR"
    
    success "Neovim configuration installed from GitHub."
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
    step_6_setup_neovim

    echo -e "\n${GREEN}======================================${NC}"
    echo -e "${GREEN}  INSTALLATION COMPLETE!  ${NC}"
    echo -e "${GREEN}======================================${NC}"
    echo -e "Please restart your session or reboot to apply all changes."
    echo -e "Note: Open 'nvim' and run :PackerSync or your plugin manager install command."
}

main
