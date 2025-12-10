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
    print_step "Step 1/7: Updating System Packages"
    info "Running apt update and upgrade. This might take a while..."
    sudo DEBIAN_FRONTEND=noninteractive apt update -y
    sudo DEBIAN_FRONTEND=noninteractive apt upgrade -y
    success "System updated successfully."
}

step_2_install_dependencies() {
    print_step "Step 2/7: Installing Core Dependencies & Node.js"
    
    info "Adding NodeSource repository for Node.js 22.x..."
    curl -fsSL https://deb.nodesource.com/setup_22.x | sudo -E bash -
    
    DEPENDENCIES=(
        "git"
        "kitty"
        "neovim"
        "spice-vdagent"
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
        "ly"   # [AGREGADO] Display Manager
    )

    info "Installing packages: ${DEPENDENCIES[*]}"
    sudo DEBIAN_FRONTEND=noninteractive apt install -y "${DEPENDENCIES[@]}"
    success "Dependencies installed."
}

step_3_install_fonts() {
    print_step "Step 3/7: Installing NerdFonts"
    
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
    print_step "Step 4/7: Linking Configurations"
    
    mkdir -p "$CONFIG_DIR"
    
    # --- 4.1 Link Common Directories ---
    info "Linking configs from COMMON to .config..."
    DIRS_TO_LINK=("alacritty" "kitty" "rofi")

    for dir in "${DIRS_TO_LINK[@]}"; do
        SOURCE="$COMMON_DIR/$dir"
        TARGET="$CONFIG_DIR/$dir"
        
        if [ -d "$SOURCE" ]; then
            ln -sfn "$SOURCE" "$TARGET"
            info "Linked directory (Common): $dir"
        else
            echo -e "${RED}[ERROR]${NC} Critical directory missing: $SOURCE"
            exit 1
        fi
    done

    # --- 4.2 Link Kali Specific Configs (xmonad, xmobar) --
    info "Linking configs from KALI to .config..."
    KALI_CONFIGS=("xmonad" "xmobar")

    for dir in "${KALI_CONFIGS[@]}"; do
        SOURCE="$KALI_DIR/$dir"
        TARGET="$CONFIG_DIR/$dir"
        
        if [ -d "$SOURCE" ]; then
            ln -sfn "$SOURCE" "$TARGET"
            info "Linked directory (Kali): $dir"
        else
            echo -e "${YELLOW}[WARN]${NC} Kali config directory missing: $SOURCE. Skipping."
        fi
    done

    # --- 4.3 Link Common Files ---
    if [ -f "$COMMON_DIR/starship.toml" ]; then
        ln -sf "$COMMON_DIR/starship.toml" "$CONFIG_DIR/starship.toml"
        info "Linked file: starship.toml"
    fi

    # --- 4.4 Link HOME files (.zshrc, .xprofile) ---
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
             echo -e "${RED}[ERROR]${NC} Critical config file missing: $SOURCE_FILE"
             exit 1
        fi
    done
}

step_5_install_starship() {
    print_step "Step 5/7: Installing Starship Prompt"
    
    if command -v starship &> /dev/null; then
        info "Starship is already installed. Skipping."
    else
        info "Downloading and running Starship installer script..."
        curl -sS https://starship.rs/install.sh | sudo sh -s -- --yes
        success "Starship installed."
    fi
}

step_6_setup_neovim() {
    print_step "Step 6/7: Setting up Neovim Configuration"
    
    NVIM_CONFIG_DIR="$HOME/.config/nvim"
    REPO_URL="https://github.com/A1nz2802/nvim.git"

    if [ -d "$NVIM_CONFIG_DIR" ]; then
        info "Existing Neovim config found. Backing up..."
        rm -rf "${NVIM_CONFIG_DIR}.bak"
        mv "$NVIM_CONFIG_DIR" "${NVIM_CONFIG_DIR}.bak"
    fi

    info "Cloning Neovim config from $REPO_URL..."
    git clone "$REPO_URL" "$NVIM_CONFIG_DIR"
    success "Neovim configuration installed."
}

step_7_enable_services() {
    print_step "Step 7/7: Enabling System Services"
    
    if systemctl list-unit-files | grep -q ly.service; then
        info "Enabling Ly Display Manager..."
        sudo systemctl enable ly
        success "Ly enabled."
    else
        echo -e "${YELLOW}[WARN]${NC} Ly service not found. Did it install correctly?"
    fi
}

# --- 5. Execution Flow ---
main() {
    sudo -v
    step_1_update_system
    step_2_install_dependencies
    step_3_install_fonts
    step_4_link_configs
    step_5_install_starship
    step_6_setup_neovim
    step_7_enable_services

    echo -e "\n${GREEN}======================================${NC}"
    echo -e "${GREEN}  INSTALLATION COMPLETE!  ${NC}"
    echo -e "${GREEN}======================================${NC}"
    echo -e "Please reboot your system to start Ly and XMonad."
}

main
