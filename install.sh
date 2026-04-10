#!/bin/sh
# Creates symlinks from ~/.config (and macOS-specific locations) into this repo.

set -e

REPO_DIR="$(cd "$(dirname "$0")" && pwd)"
CONFIG_DIR="$HOME/.config"

link() {
    src="$1"
    dst="$2"
    dst_parent="$(dirname "$dst")"

    if [ ! -d "$dst_parent" ]; then
        mkdir -p "$dst_parent"
    fi

    if [ -L "$dst" ]; then
        echo "Relinking: $dst -> $src"
        ln -sf "$src" "$dst"
    elif [ -e "$dst" ]; then
        echo "Skipping: $dst already exists and is not a symlink"
        return
    else
        echo "Linking: $dst -> $src"
        ln -s "$src" "$dst"
    fi
}

# ~/.config symlinks
link "$REPO_DIR/fish/fish"         "$CONFIG_DIR/fish"
link "$REPO_DIR/git/git"           "$CONFIG_DIR/git"
link "$REPO_DIR/nushell/nushell"   "$CONFIG_DIR/nushell"
link "$REPO_DIR/nvim/nvim"         "$CONFIG_DIR/nvim"
link "$REPO_DIR/starship/starship.toml" "$CONFIG_DIR/starship.toml"

# macOS: nushell also looks in ~/Library/Application Support/nushell
case "$(uname)" in
    Darwin)
        link "$CONFIG_DIR/nushell" "$HOME/Library/Application Support/nushell"
        ;;
esac

# Initialize extras.nu from starter if not already present
EXTRAS="$CONFIG_DIR/nushell/extras.nu"
if [ ! -e "$EXTRAS" ]; then
    echo "Creating: $EXTRAS"
    cp "$REPO_DIR/nushell/nushell/extras.nu.starter" "$EXTRAS"
fi

# Initialize git local config from template if not already present
GIT_LOCAL="$CONFIG_DIR/git/config.local"
if [ ! -e "$GIT_LOCAL" ]; then
    echo "Creating: $GIT_LOCAL (edit with your name, email, and paths)"
    cp "$REPO_DIR/git/git/config.local.template" "$GIT_LOCAL"
fi

echo "Done."
