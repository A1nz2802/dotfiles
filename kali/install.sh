#!/bin/bash

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
    print_step "Step 1/9: Updating System Packages"
    info "Running apt update..."
    sudo DEBIAN_FRONTEND=noninteractive apt update -y
    success "System updated."
}

step_2_install_dependencies() {
    print_step "Step 2/9: Installing Runtime Dependencies"
    
    if ! command -v node &> /dev/null; then
        info "Adding NodeSource repo..."
        curl -fsSL https://deb.nodesource.com/setup_22.x | sudo -E bash -
    fi
    
    DEPENDENCIES=(
        "git" "kitty" "neovim" "spice-vdagent" "zsh" "curl" "wget" "jq"
        "rofi" "trayer" "xmonad" "xmobar" "build-essential" "nodejs"
        "brightnessctl" 
    )

    info "Installing: ${DEPENDENCIES[*]}"
    sudo DEBIAN_FRONTEND=noninteractive apt install -y "${DEPENDENCIES[@]}"
    success "Dependencies installed."
}

step_3_install_fonts() {
    print_step "Step 3/9: Installing Fonts"
    FONT_SCRIPT="$KALI_DIR/install_fonts.sh"
    if [ -f "$FONT_SCRIPT" ]; then
        chmod +x "$FONT_SCRIPT"
        "$FONT_SCRIPT"
        success "Fonts installed."
    else
        echo -e "${YELLOW}[WARN]${NC} Font script not found. Skipping."
    fi
}

step_4_copy_configs() {
    print_step "Step 4/9: Copying Configurations"
    mkdir -p "$CONFIG_DIR"

    # 1. Common Directories (Using COPY instead of Symlinks)
    DIRS_TO_COPY=("alacritty" "kitty") 
    for dir in "${DIRS_TO_COPY[@]}"; do
        SOURCE="$COMMON_DIR/$dir"
        TARGET="$CONFIG_DIR/$dir"
        
        if [ -d "$SOURCE" ]; then
            # Remove existing directory to ensure a clean copy (backup if needed)
            if [ -d "$TARGET" ]; then
                info "Backing up existing $dir config..."
                rm -rf "${TARGET}.bak"
                mv "$TARGET" "${TARGET}.bak"
            fi
            
            cp -r "$SOURCE" "$TARGET"
            success "Copied directory: $dir"
        else
            echo -e "${RED}[ERROR]${NC} Missing common dir: $SOURCE"; exit 1
        fi
    done

    # 2. Kali Specific (xmonad/xmobar) - COPYING
    KALI_APPS=("xmonad" "xmobar")
    for dir in "${KALI_APPS[@]}"; do
        SOURCE="$KALI_DIR/$dir"
        TARGET="$CONFIG_DIR/$dir"
        if [ -d "$SOURCE" ]; then
            if [ -d "$TARGET" ]; then
                rm -rf "${TARGET}.bak"
                mv "$TARGET" "${TARGET}.bak"
            fi
            
            cp -r "$SOURCE" "$TARGET"
            success "Copied directory: $dir"
        else
            echo -e "${YELLOW}[WARN]${NC} Missing $dir in kali folder. Skipping."
        fi
    done

    # 3. Files to HOME (.zshrc, .xprofile) - COPYING
    HOME_FILES=(".zshrc" ".xprofile")
    for file in "${HOME_FILES[@]}"; do
        SOURCE="$KALI_DIR/$file"
        TARGET="$HOME/$file"
        if [ -f "$SOURCE" ]; then
            if [ -f "$TARGET" ]; then
                mv "$TARGET" "${TARGET}.bak"
            fi
            cp "$SOURCE" "$TARGET"
            success "Copied file: $file"
        else
            echo -e "${RED}[ERROR]${NC} Missing file: $SOURCE"; exit 1
        fi
    done
    
    # 4. Starship config - COPYING
    if [ -f "$COMMON_DIR/starship.toml" ]; then
        cp "$COMMON_DIR/starship.toml" "$CONFIG_DIR/starship.toml"
        success "Copied starship.toml"
    fi
}

step_5_install_starship() {
    print_step "Step 5/9: Installing Starship"
    
    if ! command -v starship &> /dev/null; then
        info "Downloading and installing Starship..."
        curl -sS https://starship.rs/install.sh | sudo sh -s -- --yes
        success "Starship installed."
    else
        info "Starship is already installed."
    fi
}

step_6_install_nvim() {
    print_step "Step 6/9: Configuring Neovim"
    
    NVIM_DIR="$HOME/.config/nvim"
    NVIM_REPO="https://github.com/A1nz2802/nvim.git"

    if [ -d "$NVIM_DIR" ]; then
        info "Backing up existing Neovim config..."
        rm -rf "${NVIM_DIR}.bak"
        mv "$NVIM_DIR" "${NVIM_DIR}.bak"
    fi
    
    info "Cloning Neovim configuration..."
    git clone "$NVIM_REPO" "$NVIM_DIR"
    success "Neovim setup complete."
}

step_7_configure_rofi() {
    print_step "Step 7/9: Configuring Rofi (Scripts, Themes & Config)"

    # --- A. Setup rofi-web-search script ---
    LOCAL_BIN="$HOME/.local/bin"
    SOURCE_SCRIPT="$COMMON_DIR/.local/bin/rofi-web-search"
    TARGET_SCRIPT="$LOCAL_BIN/rofi-web-search"

    mkdir -p "$LOCAL_BIN"

    if [ -f "$SOURCE_SCRIPT" ]; then
        info "Copying rofi-web-search script..."
        cp "$SOURCE_SCRIPT" "$TARGET_SCRIPT"
        chmod +x "$TARGET_SCRIPT"
        
        if [[ ":$PATH:" != *":$LOCAL_BIN:"* ]]; then
            export PATH="$LOCAL_BIN:$PATH"
        fi
        success "rofi-web-search installed and executable."
    else
        echo -e "${YELLOW}[WARN]${NC} Script 'rofi-web-search' not found in common/.local/bin."
    fi

    # --- B. Install Rofi Themes ---
    THEMES_REPO_URL="https://github.com/newmanls/rofi-themes-collection.git"
    TEMP_DIR="/tmp/rofi-themes"
    TARGET_THEMES_DIR="/usr/share/rofi/themes"

    info "Cloning Rofi themes collection..."
    if [ -d "$TEMP_DIR" ]; then rm -rf "$TEMP_DIR"; fi
    git clone --depth 1 "$THEMES_REPO_URL" "$TEMP_DIR"

    info "Installing themes to $TARGET_THEMES_DIR..."
    if [ -d "$TEMP_DIR/themes" ]; then
        sudo mkdir -p "$TARGET_THEMES_DIR"
        sudo cp -r "$TEMP_DIR/themes/"* "$TARGET_THEMES_DIR/"
        success "Rofi themes installed."
    else
        echo -e "${RED}[ERROR]${NC} Themes folder not found in cloned repo."; exit 1
    fi
    rm -rf "$TEMP_DIR"

    # --- C. Configure Config File (COPY, NO SYMLINK) ---
    ROFI_SOURCE_CONF="$COMMON_DIR/rofi/config.rasi"
    ROFI_CONFIG_DIR="$HOME/.config/rofi"
    ROFI_TARGET_CONF="$ROFI_CONFIG_DIR/config.rasi"
    
    info "Configuring Rofi config.rasi..."
    
    # 1. Ensure clean directory exists
    if [ ! -d "$ROFI_CONFIG_DIR" ]; then
        mkdir -p "$ROFI_CONFIG_DIR"
    fi

    # 2. Copy the config file directly
    if [ -f "$ROFI_SOURCE_CONF" ]; then       
        cp "$ROFI_SOURCE_CONF" "$ROFI_TARGET_CONF"
        success "Copied config.rasi to $ROFI_TARGET_CONF"
    else
         echo -e "${RED}[ERROR]${NC} config.rasi not found at: $ROFI_SOURCE_CONF"
         exit 1
    fi
}

step_8_install_ly_source() {
    print_step "Step 8/9: Compiling & Installing Ly (Zig 0.15.2)"
    
    # --- A. Install Build Dependencies (REQUIRES SUDO) ---
    info "Installing temporary build dependencies..."
    sudo DEBIAN_FRONTEND=noninteractive apt install -y libpam0g-dev libxcb-xkb-dev

    # --- B. Install Zig 0.15.2 (NO SUDO - User Space) ---
    ZIG_VER="0.15.2"
    ZIG_URL="https://ziglang.org/download/${ZIG_VER}/zig-x86_64-linux-${ZIG_VER}.tar.xz"
    ZIG_EXTRACTED_NAME="zig-x86_64-linux-${ZIG_VER}"
    
    INSTALL_DIR="$HOME/.local/bin"
    ZIG_PATH="$HOME/.local/zig-${ZIG_VER}"

    if ! command -v zig &> /dev/null || [[ "$(zig version)" != "${ZIG_VER}"* ]]; then
        info "Downloading Zig ${ZIG_VER}..."
        mkdir -p "$HOME/.local" "$INSTALL_DIR"
        
        if [ -f "/tmp/zig.tar.xz" ]; then
            sudo rm -f /tmp/zig.tar.xz
        fi
        
        if ! wget -qO /tmp/zig.tar.xz "$ZIG_URL"; then
             echo -e "${RED}[ERROR]${NC} Failed to download Zig. Check URL."; exit 1
        fi
        
        # Cleanup old versions
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

    # --- C. Clone and Build Ly ---
    BUILD_DIR="$KALI_DIR/ly_build"
    
    # Remove previous build dir
    if [ -d "$BUILD_DIR" ]; then sudo rm -rf "$BUILD_DIR"; fi
    
    info "Cloning Ly repo from Codeberg (Latest)..."
    git clone --recurse-submodules https://codeberg.org/fairyglade/ly "$BUILD_DIR"
    cd "$BUILD_DIR"

    info "Compiling and Installing Ly..."
    
    # Compile & Install (REQUIRES SUDO)
    sudo "$INSTALL_DIR/zig" build installexe -Dinit_system=systemd -Doptimize=ReleaseSafe || \
    {
        echo -e "${RED}[ERROR]${NC} Compilation/Installation failed."; exit 1
    }
    
    cd "$SCRIPT_DIR"

    # --- D. CLEANUP ---
    print_step "Cleanup: Removing build tools"
    sudo rm -rf "$BUILD_DIR"
    sudo apt remove -y libpam0g-dev libxcb-xkb-dev
    sudo apt autoremove -y
    success "Ly installed successfully (Binary + Systemd Service)."
}

step_9_enable_services() {
    print_step "Step 9/9: Enabling Services"
    
    info "Disabling current display managers..."
    sudo systemctl disable lightdm gdm3 sddm 2>/dev/null || true
    
    if [ -f "/usr/lib/systemd/system/ly@.service" ]; then
        info "Found new Ly template service (ly@.service)."
        sudo systemctl enable ly@tty2.service
        sudo ln -sf /usr/lib/systemd/system/ly@.service /etc/systemd/system/display-manager.service
        success "Ly service enabled on TTY2."
    elif [ -f "/etc/systemd/system/ly.service" ] || [ -f "/usr/lib/systemd/system/ly.service" ]; then
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
    sudo -v
    step_1_update_system
    step_2_install_dependencies
    step_3_install_fonts
    step_4_copy_configs
    step_5_install_starship
    step_6_install_nvim
    step_7_configure_rofi
    step_8_install_ly_source
    step_9_enable_services
    
    echo -e "\n${GREEN}======================================${NC}"
    echo -e "${GREEN}  INSTALLATION COMPLETE! REBOOT NOW.  ${NC}"
    echo -e "${GREEN}======================================${NC}"
}

main
