![xmonad](.screenshots/main.png)

### XMonad Keybindings

> [!NOTE]
> `M` refers to the `Mod` key (usually `Alt` or `Super`/`Windows` key), and `S` refers to `Shift`.

| Keybinding    | Description                                          |
| :------------ | :--------------------------------------------------- |++++++++++
| `M-S-q`       | Quit XMonad                                          |
| `M-w`         | Kill focused window                                  |
|               | **Switch to Workspace**                              |
| `M-[1-9]`     | Switch to workspace [1-9]                            |
| `M-S-[1-9]`   | Send to workspace [1-9]                              |
|               | **Window Navigation**                                |
| `M-Down`      | Move focus down                                      |
| `M-Up`        | Move focus up                                        |
| `M-Right`     | Swap focused window with next                        |
| `M-Left`      | Swap focused window with prev                        |
|               | **Rofi**                                             |
| `M-m`         | Menu (Rofi drun)                                     |
| `M-S-m`       | Window Navigator                                     |
| `M-S-.`       | Emojis                                               |
| `M-g`         | Google Search                                        |
|               | **Scrot (Screenshots)**                              |
| `M-S-s`       | Capture screenshot + clipboard                       |
| `M-s`         | Full screenshot + clipboard                          |
|               | **Favorite Programs**                                |
| `M-Return`    | Launch terminal                                      |
| `M-b`         | Launch web browser                                   |
| `M-e`         | Launch file manager                                  |
|               | **Monitors**                                         |
| `M-.`         | Switch focus to next monitor                         |
| `M-,`         | Switch focus to previous monitor                     |
|               | **Layouts**                                          |
| `M-Tab`       | Switch to next layout                                |
| `M-Space`     | Toggle noborders/full layout and toggle struts       |
|               | **Window Resizing**                                  |
| `M-h`         | Shrink window (horizontally)                         |
| `M-l`         | Expand window (horizontally)                         |
| `M-M1-j`      | Shrink window vertically (`M1` => `Alt`)             |
| `M-M1-k`      | Expand window vertically (`M1` => `Alt`)             |
|               | **Floating Windows**                                 |
| `M-f`         | Toggle float layout                                  |
| `M-t`         | Sink a floating window                               |
| `M-S-t`       | Sink all floated windows                             |
|               | **Master/Stack Pane Adjustment**                     |
| `M-S-Up`      | Increase clients in master pane                      |
| `M-S-Down`    | Decrease clients in master pane                      |

### XMonad Keybindings



### Dependencies

```bash
sudo pacman -S --noconfirm \
  # Window Managers & Core Utils
  qtile xmonad xmonad-contrib xmobar xdotool \
  # System & Utilities
  pacman-contrib trayer picom scrot nitrogen python-psutil \
  # Theming & Appearance
  papirus-icon-theme gnome-themes-extra breeze-gtk breeze gnome-tweaks lxappearance \
  # Terminal & Shell
  zsh zsh-autocomplete zsh-autosuggestions zsh-syntax-highlighting \
  # Clipboard & File Management (ranger)
  xclip copyq ueberzugpp ffmpegthumbnailer poppler \
  # Fonts & Emojis
  noto-fonts-emoji rofi-emoji \
  # Bluetooth
  bluez bluez-utils blueman \
  # Keyring
  gnome-keyring seahorse
```

```bash
yay -S --noconfirm pwvucontrol arc-gtk-theme
```

### SDDM Theme

Install the Astronaut theme following the automated configuration:

<https://github.com/Keyitdev/sddm-astronaut-theme>

Follow the automatic configuration.

### Cursor Theming

1- Cursor for SDDM

To set a custom cursor theme for SDDM:

- Navigate to `/usr/share/icons/default` and edit the `index.theme` file:

    ```conf
    [Icon Theme]
    Inherits=<your_theme>
    CursorTheme=True
    ```

- To change the cursor size, edit the SDDM configuration file located at `/etc/sddm.conf`:

    ```conf
    [Theme]
    Current=sddm-astronaut-theme

    [General]
    CursorTheme=<your_theme>
    CursorSize=64
    ```

> [!NOTE]
> Keep in mind that some cursor themes may only support specific sizes or include only one predefined size.

- Restart SDDM for the changes to take effect:

    ```bash
    sudo systemctl restart sddm
    ```

2- Cursor for All Applications (Browser, Terminal, etc.)

To apply the cursor theme system-wide:

- Edit (or create) the file `~/.gtkrc-2.0`:

    ```conf
    include "/home/a1nz/.gtkrc-2.0.mine"
    gtk-theme-name="Arc-Dark"
    gtk-icon-theme-name="Papirus-Dark"
    gtk-font-name="Cantarell 11"
    gtk-cursor-theme-name="Kafka"
    gtk-cursor-theme-size=64
    ```

- Alternatively, you can use `lxappearance` to configure cursor settings via a GUI:

    ```bash
    sudo pacman -S lxappearance
    ```

3- Cursor Settings for X with xsetroot

- Install the necessary packages:

    ```bash
    sudo pacman -S xorg-xinit xorg-xsetroot
    ```

- Create the file `~/.Xresources` with the following content:

    ```conf
    Xcursor.theme: Your-Theme
    Xcursor.size: 64
    ```

- Create or edit `~/.xprofile` and add:

    ```sh
        # Screens
        hdmi=$(xrandr | grep ' connected' | grep 'HDMI' | awk '{print $1}')

        if [ "$hdmi" = "HDMI-1" ]; then
            xrandr \
                --output DP-0 --primary --mode 3440x1440 \
                --output HDMI-1 --mode 3840x2160 &
        else
            xrandr \
                --output DP-0 --primary --mode 3440x1440 \
                --output HDMI-1 --off \
                --output eDP-1 --off &
        fi
    ```

### Font Installation

```bash
sh install_fonts.sh
```

To list all installed Nerd Font families recognized by your system:

```bash
fc-list | grep "Nerd Font" | cut -d: -f2 | sort -u
```

or you can use font manager

```bash
sudo pacman -S font-manager
```

Give execute permissions to local scripts:

```bash
chmod +x ~/.local/bin/*
chmod +x ~/xmobar/trayer-padding-icon.sh
```

### Zsh

1. Install Zsh and plugins:

```bash
sudo pacman -S zsh zsh-autosuggestions zsh-syntax-highlighting zsh-autocomplete
```

2. Change default shell for user and root:

```bash
sudo usermod --shell /usr/bin/zsh a1nz
sudo usermod --shell /usr/bin/zsh root
```

3. Link root's zshrc to user configuration:

```bash
sudo ln -sf /home/a1nz/.zshrc /root/.zshrc
```

### GRUB Theme

1. Install Elegant GRUB theme:

<https://github.com/vinceliuice/Elegant-grub2-themes>

2. Configuration details:
  - Theme path: `/boot/grub/themes/Elegant-forest-window-right-dark`
  - Configuration file: `/etc/default/grub`
  - Apply changes: 
    ```bash
    sudo grub-mkconfig -o /boot/grub/grub.cfg
    ```

Convert GRUB background for ultrawide displays:
```bash
magick \
  \( background.jpg -crop 1x1080+0+0 -resize 440x1440! \) \
  \( background.jpg -resize 2560x1440! \) \
  \( background.jpg -crop 1x1080+1919+0 -resize 440x1440! \) \
  +append extended_image.jpg
```

### VMware Tools Setup

```bash
sudo pacman -S open-vm-tools
sudo systemctl enable vmtoolsd.service
sudo systemctl enable vmware-vmblock-fuse.service
```

### Display Configuration for VM

Set up custom resolution for ultrawide displays:

```bash
sudo pacman -S xorg-xrandr

# Generate modeline for 3440x1440@60Hz
cvt 3440 1440 60 -r

# Add custom resolution
xrandr --newmode "3440x1440R" 319.75 3440 3488 3520 3600 1440 1443 1453 1481 +hsync -vsync
xrandr --addmode Virtual-1 "3440x1440R"
xrandr --output Virtual-1 --mode "3440x1440R"
```
