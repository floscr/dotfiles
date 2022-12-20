{ config, options, lib, pkgs, ... }:
with lib;
let cfg = config.modules.shell;
in
{
  options.modules.shell = { enable = my.mkBoolOpt false; };

  config = mkIf cfg.enable {
    # Miscellaneous userspace utilities I always want available on any platform.
    user.packages = with pkgs;
      [
        bc
        binutils
        coreutils
        curl
        dnsutils
        exa
        fd
        fzf
        gawk
        gnugrep
        gnumake
        htop
        jq
        killall
        lsof
        nvd # Nix version diff tool
        ripgrep
        tree
        unzip
        wget
        vim
        zip
      ] ++ (if config.targetSystem.isLinux then
      # Exclusive to Linux.
        [
        ] else
      # Exclusive to Darwin.
        [
        ]);

    # Miscellaneous aliases.
    modules.shell.zsh.aliases = {
      bc = "bc -lq";
      egrep = "egrep --color=auto";
      exa = "exa -h --group-directories-first --git";
      fgrep = "fgrep --color=auto";
      grep = "grep --color=auto";
      l = "exa -1a";
      ll = "exa -la";
      ls = "exa";
      lt = "exa -lm -s modified";
      mkdir = "mkdir -p";
      rg = "rg --hidden";
      tree = "tree -a -I '.git'";
      wget = "wget -c";
      tailf = "tail -f"; # util-linux habits.
    };

    # Shell fuzzer configuration.
    home.configFile = {
      "zsh/rc.d/rc.fzy.zsh".source = "${config.dotfiles.configDir}/fzy/rc.zsh";
      "zsh/rc.d/rc.fzf.zsh".source = "${config.dotfiles.configDir}/fzf/rc.zsh";
    };
  };
}
