eval (/opt/homebrew/bin/brew shellenv)
if status is-interactive
    # Commands to run in interactive sessions can go here
    set -x ANDROID_HOME "$HOME/Library/Android/sdk"

    set -x PATH \
        $PATH \
        $HOME/.local/bin \
        /Applications/Postgres.app/Contents/Versions/latest/bin \
        /opt/homebrew/Cellar/mariadb/11.7.2/bin \
        $HOME/.rvm/bin \
        $ANDROID_HOME/platform-tools \
        $ANDROID_HOME/build-tools \
        $ANDROID_HOME/tools/bin \
        $ANDROID_HOME/emulator \

    set -gx HOMEBREW_NO_HINTS 1

    set -gx JAVA_HOME (/usr/libexec/java_home -v17)

    rbenv init - fish | source
    fzf --fish | source
end
