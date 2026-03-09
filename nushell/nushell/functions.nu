
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
  fish -c "
    set -x before (mktemp)
    env | sort > $before
    nvm use
    env | sort | comm -13 $before -
    rm $before
  " | lines | split column "=" key value | transpose -i -r -d | load-env
}

