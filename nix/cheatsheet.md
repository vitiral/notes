# Important Links

- `nixos-help`: open manual in browser
- [nixos cheatsheet](https://github.com/knedlsepp/nix-cheatsheet)
- [good reference config](https://github.com/gilligan/nixos-config/blob/master/configuration.nix)


# Basic commands
| **CMD**                                       | **Description** |
| --------------------------------------------- | --------------- |
| `journalctl --user -xe`                       | Get user logs from services
| `systemctl restart wpa_supplicant.service`    | Restart wifi
| `nixos-option services.xserver.enable`        | print the value of an option
| `sudo nix-repl '<nixpkgs/nixos>'`             | open the repl with nixos
| `sudo nix-collect-garbage --delete-older-than 1d` | collect garbage and delete it

# nix-env

| **CMD** | **Description** |
| --------------------------------------------- | --------------- |
| `nix-env -q`                                  | List installed packages
| `nix-env -qaP --description`                  | List available packages
| `nix-env -iA nixpkgs.gitFull`                 | Install packages
| `nix-env -e git`                              | Remove packages (always copy/paste name)
| `nix-shell --packages X Y Z                   | Starting shell with specific packages
| `nix-shell`                                   | Starting shells with a `$CWD/shell.nix` file.
| `nix-shell --pure`                            | Nix-shell cleaned of all environment variables.
| `nix-shell -p atom --run "atom"`              | Run a command from within the environment.
| `nix-shell --add-root ??`                     | TODO(wut): Avoid a shell being garbage collected

# Updating packages

| **CMD** | **Description** |
| ------- | --------------- |
| `nix-channel --update`                        | (1.) update your channel
| `nix-env -qc`                                 | (2.) See what's available
| `nix-env -u --keep-going --leq`               | Update everything
| `nix-store --query --references $(nix-instantiate '<nixpkgs>' -A emacs)` | Show dependencies

# NixOS language / package

- How to switch to unstable and upgrade:
  - `nix-channel --list`: will show something like `nixos https://nixos.org/channels/nixos-16.09`
  - `nix-channel --add https://nixos.org/channels/nixos-unstable nixos`: removes the stable channel
  - `nixos-rebuild switch --upgrade`: upgrade the system
  - `nix-channel --add https://nixos.org/channels/nixpkgs-unstable`: unstable packages
  - `nix-channel --update`: recommended, not sure why?
  - `nix-env -u`: upgrade all packages
- `config` contains the _full final_ configuration which can be accessed
  anywhere since nix is lazy.
- options are merged if defined in multiple places, this includes lists.
- nixos modules are of the form
  `{config, pkgs, ...}: { imports = [...]; options = {...}; config = {...}; meta;}`
  - Anything in `imports` is just merged directly into your full configuration.
  - `config`: not completely sure what this is. I _think_ it sets the configuration
    on... other modules?
    - `warnings =`: set a list of warnings
    - `assertions =`: set a list of assertions
  - `options`: options which can be set for this module.
    `enable = mkOption { type = types.bool, ...}`
  - `meta`: maintainers, doc, etc
- `with NAME;` expressions are basically `from NAME import * ` _except_ that
  they do _not_ shadow variables. If `x` exists both in scope _and_ in `NAME.x`
  then the one in scope will be kept.


## Replacing Modules
You can replace modules using `disabledModules`

## Running Tests
Tests are written in a nix-expr, where the test script itself is written in `perl`.
You can run tests like:

```
$ nix-build '<nixpkgs/nixos/tests/login.nix>'
$ firefox result/log.html
```

