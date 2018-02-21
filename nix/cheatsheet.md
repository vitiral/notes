# Important Links

- `nixos-help`: open manual in browser
- [nixos cheatsheet](https://github.com/knedlsepp/nix-cheatsheet)
- [good reference config](https://github.com/gilligan/nixos-config/blob/master/configuration.nix)


# Basic commands
| **CMD** | **Description** |
| ------- | --------------- |
| `systemctl restart wpa_supplicant.service` | Restart wifi


# nix-env

| **CMD** | **Description** |
| ------- | --------------- |
| `nix-env -q` | List installed packages
| `nix-env -qaP --description` | List available packages
| `nix-env -iA nixpkgs.gitFull` | Instal packages
| `nix-env -e git` | Remov packages (always copy/paste name)
| `nix-shell --packages pythonPackages.ipython pythonPackages.numpy` | Starting shell with specific packages
| `nix-shell` | Starting shells with a `$CWD/shell.nix` file.
| `nix-shell --pure` | Nix-shell cleaned of all environment variables.
| `nix-shell -p atom --run "atom"` | Run a command from within the environment.
| `nix-shell --add-root ??` | Avoid a shell being garbage collected TODO: figure this out

# Updating packages

| **CMD** | **Description** |
| ------- | --------------- |
| `nix-channel --update` | (1.) update your channel
| `nix-env -qc` | (2.) See what's available
| `nix-env -u --keep-going --leq` | Update everything
| nix-store --query --references $(nix-instantiate '<nixpkgs>' -A emacs) | Show dependencies
