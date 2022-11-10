# dotfiles

This is a set of configuration files for various tools I use on a daily basis.
You can take a look at how I do some of the things and incorporate them into
your own system. :)

This is my new dotfiles repository. The old one was set up using `git bare`
repos, which was a hastle to get working on a new system. This uses `stow`.

Download the repo to your `$HOME` directory. This is important!
```
$ git clone git@github.com:byhowe/dotfiles.git
```

Stow whichever config you like.
```
$ stow alacritty
```

This will generate a symlink to the actual config directory on the system, and
it needs to be empty. Otherwise, it will not work.
