# config.nu

$env.config.buffer_editor = "nvim"
$env.config.edit_mode = "vi"

$env.config.show_banner = false
$env.config.rm.always_trash = true

$env.config.footer_mode = "auto"

# Setup PATH
$env.PATH = (
    [
        ~/.local/bin
        /opt/homebrew/bin
        /Applications/Postgres.app/Contents/Versions/latest/bin
    ]
    | append (open /etc/paths | lines)
)

$env.HOMEBREW_NO_HINTS = 1
if ("/usr/libexec/java_home" | path exists) {
  try { $env.JAVA_HOME = (^/usr/libexec/java_home -v17 err> /dev/null) }
}

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
