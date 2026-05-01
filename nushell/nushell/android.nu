export-env {
    $env.ANDROID_HOME = $env.HOME + "/Library/Android/sdk"
    $env.PATH = $env.PATH | prepend [
        ($env.ANDROID_HOME | path join "platform-tools")
        ($env.ANDROID_HOME | path join "build-tools")
        ($env.ANDROID_HOME | path join "tools/bin")
        ($env.ANDROID_HOME | path join "emulator")
    ]
}
