
#
# Run claude from the first parent directory
# that has a .claude directory in it.  Avoids
# .claude turds everywhere.
#
def claude [...args: string] {
  let start = $env.PWD
  mut dir = $env.PWD
  loop {
    # Don't start a project from my home directory unless
    # I'm in my home directory when I run this function
    if ($dir | path join ".claude" | path exists) {
      if $dir == $env.HOME and $start != $env.HOME {
        break
      }
      cd $dir
      ^claude ...$args
      return
    }
    let parent = ($dir | path dirname)
    if $parent == $dir { break }
    $dir = $parent
  }
  ^claude ...$args
}


#
# Alias that helps fun nvm use
#
def --env "nvm use" [] {
  bash -c '
    before=$(mktemp)
    env | sort > "$before"
    export NVM_DIR="$HOME/.nvm"
    source "$NVM_DIR/nvm.sh"
    nvm use
    env | sort | comm -13 "$before" -
    rm "$before"
  ' | lines | parse "{key}={value}" | if ($in | is-not-empty) {
    let changes = $in
    $changes | transpose -i -r -d | load-env
    print ($changes | select key value | rename variable value)
  }
}

def nvimg [...args: string] {
  ^open -a neovide ...$args
}

