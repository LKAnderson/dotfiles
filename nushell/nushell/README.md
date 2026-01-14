# Kent's Nushell Configuration

On macOS, do yourself a favor and symlink this directory from `~/Library/Application Support/nushell`.

First, clone this repository into `~/.config/nushell`.

Then symlink Nushell's data-dir to this directory:

```shell
cd ~/Library/Application\ Support/

# if nushell is there, remove it
rm -r nushell

ln -s ~/.config/nushell nushell
```
There is a debate online about whether Nushell should support the POSIX standard ~/.config directory.
I doubt it's going to change anytime soon, so we'll just work with their insistence on using
`~/Library/Application Support/nushell` on macOS. Symlinking FTW!

## Dependencies

### Starship

https://starship.rs/

Seems like a nice tool. Cross-shell, works with nushell.

