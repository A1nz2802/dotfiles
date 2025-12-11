#!/bin/bash

# ================================================
# KALI LINUX AUTOMATED SETUP SCRIPT
# ================================================

# --- 1. Safety & Error Handling ---
set -e
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' 

# Trap errors
trap 'echo -e "\n${RED}[CRITICAL ERROR]${NC} The script failed on line $LINENO. Exiting."; exit 1' ERR

# --- 2. Helper Functions ---
print_step() {
    echo -e "\n${BLUE}======================================${NC}"
    echo -e "${YELLOW} PROGRESS: $1 ${NC}"
    echo -e "${BLUE}======================================${NC}"
}
success() { echo -e "${GREEN}[OK]${NC} $1"; }
info() { echo -e "${BLUE}[INFO]${NC} $1"; }

# --- 3. Paths & Variables ---
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
KALI_DIR="$SCRIPT_DIR"
DOTFILES_DIR="$(dirname "$KALI_DIR")"
COMMON_DIR="$DOTFILES_DIR/common"
CONFIG_DIR="$HOME/.config"

# --- 4. Main Installation Steps ---

step_1_update_system() {
    print_step "Step 1/7: Updating System Packages"
    info "Running apt update..."
    sudo DEBIAN_FRONTEND=noninteractive apt update -y
    success "System updated."
}

step_2_install_dependencies() {
    print_step "Step 2/7: Installing Runtime Dependencies"
    
    # Node.js 22.x Repo (Checks if already added)
    if ! command -v node &> /dev/null; then
        info "Adding NodeSource repo..."
        curl -fsSL https://deb.nodesource.com/setup_22.x | sudo -E bash -
    fi
    
    # List of PACKAGES TO KEEP (Runtime)
    DEPENDENCIES=(
        "git" "kitty" "neovim" "spice-vdagent" "zsh" "curl" "wget" "jq"
        "rofi" "trayer" "xmonad" "xmobar" "build-essential" "nodejs"
        "brightnessctl" 
        # Note: We removed the -dev packages from here to install/remove them later
    )

    info "Installing: ${DEPENDENCIES[*]}"
    sudo DEBIAN_FRONTEND=noninteractive apt install -y "${DEPENDENCIES[@]}"
    success "Dependencies installed."
}

step_3_install_fonts() {
    print_step "Step 3/7: Installing Fonts"
    FONT_SCRIPT="$KALI_DIR/install_fonts.sh"
    if [ -f "$FONT_SCRIPT" ]; then
        chmod +x "$FONT_SCRIPT"
        "$FONT_SCRIPT"
        success "Fonts installed."
    else
        echo -e "${YELLOW}[WARN]${NC} Font script not found. Skipping."
    fi
}

step_4_link_configs() {
    print_step "Step 4/7: Linking Configurations"
    mkdir -p "$CONFIG_DIR"

    # 1. Common Directories
    DIRS_TO_LINK=("alacritty" "kitty" "rofi")
    for dir in "${DIRS_TO_LINK[@]}"; do
        SOURCE="$COMMON_DIR/$dir"
        TARGET="$CONFIG_DIR/$dir"
        if [ -d "$SOURCE" ]; then
            ln -sfn "$SOURCE" "$TARGET"
            info "Linked: $dir"
        else
            echo -e "${RED}[ERROR]${NC} Missing common dir: $SOURCE"; exit 1
        fi
    done

    # 2. Kali Specific (xmonad/xmobar)
    KALI_APPS=("xmonad" "xmobar")
    for dir in "${KALI_APPS[@]}"; do
        SOURCE="$KALI_DIR/$dir"
        TARGET="$CONFIG_DIR/$dir"
        if [ -d "$SOURCE" ]; then
            ln -sfn "$SOURCE" "$TARGET"
            info "Linked: $dir"
        else
            echo -e "${YELLOW}[WARN]${NC} Missing $dir in kali folder. Skipping."
        fi
    done

    # 3. Files to HOME (.zshrc, .xprofile)
    HOME_FILES=(".zshrc" ".xprofile")
    for file in "${HOME_FILES[@]}"; do
        SOURCE="$KALI_DIR/$file"
        TARGET="$HOME/$file"
        if [ -f "$SOURCE" ]; then
            # Backup if exists and is not a symlink
            [ -f "$TARGET" ] && [ ! -L "$TARGET" ] && mv "$TARGET" "${TARGET}.bak"
            ln -sf "$SOURCE" "$TARGET"
            success "Linked $file"
        else
            echo -e "${RED}[ERROR]${NC} Missing file: $SOURCE"; exit 1
        fi
    done
    
    # 4. Starship config
    [ -f "$COMMON_DIR/starship.toml" ] && ln -sf "$COMMON_DIR/starship.toml" "$CONFIG_DIR/starship.toml"
}

step_5_starship_nvim() {
    print_step "Step 5/7: Starship & Neovim"
    
    # Starship
    if ! command -v starship &> /dev/null; then
        curl -sS https://starship.rs/install.sh | sudo sh -s -- --yes
    fi

    # Neovim Config
    NVIM_DIR="$HOME/.config/nvim"
    if [ -d "$NVIM_DIR" ]; then
        rm -rf "${NVIM_DIR}.bak"
        mv "$NVIM_DIR" "${NVIM_DIR}.bak"
    fi
    git clone "https://github.com/A1nz2802/nvim.git" "$NVIM_DIR"
    success "Neovim & Starship setup complete."
}

step_6_install_ly_source() {
    print_step "Step 6/7: Compiling & Installing Ly (Zig 0.15.2)"
    
    # --- A. Install Build Dependencies ---
    info "Installing temporary build dependencies..."
    sudo DEBIAN_FRONTEND=noninteractive apt install -y libpam0g-dev libxcb-xkb-dev

    # --- B. Install Zig 0.15.2 (Stable) ---
    ZIG_VER="0.15.2"
    # Note: Confirmed working URL
    ZIG_URL="https://ziglang.org/download/${ZIG_VER}/zig-x86_64-linux-${ZIG_VER}.tar.xz"
    # Note: Folder name inside tarball uses 'x86_64-linux' order
    ZIG_EXTRACTED_NAME="zig-x86_64-linux-${ZIG_VER}"
    
    INSTALL_DIR="$HOME/.local/bin"
    ZIG_PATH="$HOME/.local/zig-${ZIG_VER}"

    # Check if exact version is already installed
    if ! command -v zig &> /dev/null || [[ "$(zig version)" != "${ZIG_VER}"* ]]; then
        info "Downloading Zig ${ZIG_VER}..."
        mkdir -p "$HOME/.local" "$INSTALL_DIR"
        
        if ! wget -qO /tmp/zig.tar.xz "$ZIG_URL"; then
             echo -e "${RED}[ERROR]${NC} Failed to download Zig. Check URL."; exit 1
        fi
        
        # Cleanup and extraction
        rm -rf "$ZIG_PATH" 
        rm -rf "$HOME/.local/$ZIG_EXTRACTED_NAME"
        rm -f "$INSTALL_DIR/zig"

        tar -xf /tmp/zig.tar.xz -C "$HOME/.local"
        
        if [ -d "$HOME/.local/$ZIG_EXTRACTED_NAME" ]; then
            mv "$HOME/.local/$ZIG_EXTRACTED_NAME" "$ZIG_PATH"
        else
            echo -e "${RED}[ERROR]${NC} Folder not found: $ZIG_EXTRACTED_NAME"; exit 1
        fi
        
        ln -sf "$ZIG_PATH/zig" "$INSTALL_DIR/zig"
        export PATH="$INSTALL_DIR:$PATH"
        success "Zig ${ZIG_VER} installed successfully."
    else
        info "Zig ${ZIG_VER} is already installed."
        export PATH="$INSTALL_DIR:$PATH"
    fi

    # --- C. Clone and Build Ly (Latest) ---
    BUILD_DIR="$KALI_DIR/ly_build"
    if [ -d "$BUILD_DIR" ]; then rm -rf "$BUILD_DIR"; fi
    
    info "Cloning Ly repo from Codeberg (Latest)..."
    git clone --recurse-submodules https://codeberg.org/fairyglade/ly "$BUILD_DIR"
    cd "$BUILD_DIR"

    info "Compiling and Installing Ly..."
    
    # Using 'installexe' with '-Dinit_system=systemd' 
    # This compiles AND installs the binary + systemd service automatically.
    # Sudo is required for writing to /usr and /etc.
    sudo "$INSTALL_DIR/zig" build installexe -Dinit_system=systemd -Doptimize=ReleaseSafe || \
    {
        echo -e "${RED}[ERROR]${NC} Compilation/Installation failed."; exit 1
    }
    
    cd "$SCRIPT_DIR"

    # --- D. CLEANUP ---
    print_step "Cleanup: Removing build tools"
    rm -rf "$BUILD_DIR"
    # Optional: Remove Zig to save space
    # rm -rf "$ZIG_PATH"
    # rm -f "$INSTALL_DIR/zig"
    
    sudo apt remove -y libpam0g-dev libxcb-xkb-dev
    sudo apt autoremove -y
    
    success "Ly installed successfully (Binary + Systemd Service)."
}

step_7_enable_services() {
    print_step "Step 7/7: Enabling Services"
    
    # 1. Disable other Display Managers
    info "Disabling current display managers..."
    sudo systemctl disable lightdm gdm3 sddm 2>/dev/null || true
    
    # 2. Enable Ly (Detecting if new 'ly@' or old 'ly' service exists)
    if [ -f "/usr/lib/systemd/system/ly@.service" ]; then
        info "Found new Ly template service (ly@.service)."
        
        # Enabling Ly specifically on TTY2
        # This is standard for TUI managers to avoid conflicts with boot logs on tty1
        sudo systemctl enable ly@tty2.service
        
        # Ensure it is the default display manager
        sudo ln -sf /usr/lib/systemd/system/ly@.service /etc/systemd/system/display-manager.service
        
        success "Ly service enabled on TTY2."
        
    elif [ -f "/etc/systemd/system/ly.service" ] || [ -f "/usr/lib/systemd/system/ly.service" ]; then
        # Fallback for legacy versions
        info "Found legacy Ly service."
        sudo systemctl enable ly
        success "Ly service enabled."
    else
        echo -e "${RED}[ERROR]${NC} Ly service file not found. Check compilation logs."
        exit 1
    fi
}

# --- 5. Execution ---
main() {
    sudo -v # Ask for password upfront
    step_1_update_system
    step_2_install_dependencies
    step_3_install_fonts
    step_4_link_configs
    step_5_starship_nvim
    step_6_install_ly_source
    step_7_enable_services
    
    echo -e "\n${GREEN}======================================${NC}"
    echo -e "${GREEN}  INSTALLATION COMPLETE! REBOOT NOW.  ${NC}"
    echo -e "${GREEN}======================================${NC}"
}

main
