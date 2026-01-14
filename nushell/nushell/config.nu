# config.nu
#
# Installed by:
# version = "0.102.0"
#

$env.config.buffer_editor = $env.HOME + "/.local/bin/nvimg"
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
# Load env from things that set env (eg, nvm)
#
def --env "nvm use" [] {
  fish -c "
    set -x before (mktemp)
    env | sort > $before
    nvm use
    env | sort | comm -13 $before -
    rm $before
  " | lines | split column "=" key value | transpose -i -r -d | load-env
}
