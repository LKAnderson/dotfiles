# config.nu

$env.config.buffer_editor = "nvim"
$env.config.edit_mode = "vi"

$env.config.show_banner = false
$env.config.rm.always_trash = true

$env.config.footer_mode = "auto"

$env.ANDROID_HOME = $env.HOME + "/Library/Android/sdk"

# Setup PATH
$env.PATH = (
    (open /etc/paths | lines)
    | append [
        ~/.local/bin
        ~/.rvm/bin
        /opt/homebrew/bin
        ~/.config/emacs/bin
        ([$env.ANDROID_HOME "/platform-tools"] | str join)
        ([$env.ANDROID_HOME "/build-tools"] | str join)
        ([$env.ANDROID_HOME "/tools/bin"] | str join)
        ([$env.ANDROID_HOME "/emulator"] | str join)
        /Applications/Postgres.app/Contents/Versions/latest/bin
    ]
)

$env.HOMEBREW_NO_HINTS = 1
$env.JAVA_HOME = (/usr/libexec/java_home -v17)

$env.PATH = $env.PATH | uniq

#
# Aliases
#
alias ll = ls -l
alias appopen = /usr/bin/open

#
# Load custom functions
#
source functions.nu

#
# Load machine-specific extras
#
source extras.nu
