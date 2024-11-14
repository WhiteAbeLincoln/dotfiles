{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.windows.environment;
  toPsHashmap = import ./makeHashmap.nix { lib = lib; };
  setEnvVarScript = pkgs.writeText "ps-set-env.ps1" ''
    param(
      [Parameter()]
      [switch]$DryRun,
      [Parameter()]
      [switch]$Clear,
      [Parameter(Mandatory, Position = 0)]
      [string]$XmlPath
    )

    Write-Verbose "Loading variables from $XmlPath"
    $hashMap = Import-Clixml -Path $XmlPath
    Write-Verbose ($hashMap | Format-Table | Out-String)
    $hashMap.GetEnumerator() | ForEach-Object {
      if ($Clear) {
        $_.Value = ""
      }
      Write-Verbose "Setting environment variable '$($_.Name)' to '$($_.Value)'"
      if ($DryRun) {
        Write-Output "[System.Environment]::SetEnvironmentVariable('$($_.Name)', '$($_.Value)', [System.EnvironmentVariableTarget]::User)"
      }
      else {
        [System.Environment]::SetEnvironmentVariable($_.Name, $_.Value, [System.EnvironmentVariableTarget]::User)
      }
    }
  '';
  flagMap = {
    path = "p";
    path-list = "l";
    for-wsl = "u";
    for-win = "w";
  };
  mkWslEnvFlags = flags: lib.concatStrings (lib.mapAttrsToList (key: flag: if flag then flagMap.${key} else "") flags);
  mkWslEnvVar = key: flags:
    let
      flagStr = mkWslEnvFlags flags;
      finalFlagStr = if flagStr == "" then "" else "/" + flagStr;
    in
      key + finalFlagStr;
  flagsType = types.submodule {
    options = {
      path = mkOption {
        default = false;
        type = types.bool;
        description = ''
          Translates the path between WSL/Linux style paths and Win32 paths.
        '';
      };
      path-list = mkOption {
        default = false;
        type = types.bool;
        description = ''
          Indicates the environment variable is a list of paths.
        '';
      };
      for-wsl = mkOption {
        default = false;
        type = types.bool;
        description = ''
          Indicates that this environment variable should only be included when running WSL from Win32.
        '';
      };
      for-win = mkOption {
        default = false;
        type = types.bool;
        description = ''
          Indicates that this environment variable should only be included when running Win32 from WSL.
        '';
      };
    };
  };
in
{
  options.windows.environment = {
    enable = mkEnableOption ''
      Allows managing windows environment variables.

      Will only manage the variables explicitly listed here, to avoid breaking
      changes made through Windows.
    '';

    variables = mkOption {
      type = types.attrsOf (types.oneOf [(types.listOf types.str) types.str]);
      default = {};
      example = {
        BASH_ENV = "~/.bash_env_noninteractive";
      };
      description = "List of variables to manage. Set to an empty list to ensure they don't exist.";
    };

    wslenv = mkOption {
      type = types.attrsOf flagsType;
      default = {};
      example = {
        GOPATH = { path-list = true; };
        USERPROFILE = { for-win = true; };
        SOMEVAR = { for-win = true; path = true; };
      };
    };
  };

  config =
    let
      exec = ''
        function runPsScript() {
          local dry_run_arg
          local verbose_arg
          if [[ -v VERBOSE ]]; then
            verbose_arg="-Verbose"
          else
            verbose_arg=""
          fi
          if [[ -v DRY_RUN ]]; then
            dry_run_arg="-DryRun"
          else
            dry_run_arg=""
          fi

          local ps_file
          ps_file="$(/usr/bin/wslpath -w ${escapeShellArg setEnvVarScript})"

          local xml_path
          xml_path="$(/usr/bin/wslpath -w "$1")"

          local ps_path
          ps_path="$(/usr/bin/wslpath -a 'C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe')"

          shift

          "$ps_path" -ExecutionPolicy Bypass -File "$ps_file" $verbose_arg $dry_run_arg -XmlPath "$xml_path" "$@"
        }

        function cleanOldWinEnv() {
          if [[ ! -v oldGenPath || ! -e "$oldGenPath/hm-winenv.xml" || "$oldGenPath" == "$newGenPath" ]] ; then
            return
          fi

          if [[ -v VERBOSE ]]; then
            echo "Clearing old windows env vars"
          fi

          runPsScript "$oldGenPath/hm-winenv.xml" -Clear
        }

        function makeNewWinEnv() {
          if [[ ! -v newGenPath || ! -e "$newGenPath/hm-winenv.xml" || "$oldGenPath" == "$newGenPath" ]] ; then
            return
          fi

          if [[ -v VERBOSE ]]; then
            echo "Adding new windows env vars"
          fi

          runPsScript "$newGenPath/hm-winenv.xml"
        }

        cleanOldWinEnv
        makeNewWinEnv
      '';
    in
      mkMerge [
        { home.activation.addWinEnv = hm.dag.entryAfter ["writeBoundary"] exec; }
        (mkIf cfg.enable (let
          wslenvVar = lib.concatStringsSep ":" (lib.mapAttrsToList mkWslEnvVar cfg.wslenv);
          vars = lib.mergeAttrs (lib.optionalAttrs (wslenvVar != "") { WSLENV = wslenvVar; }) cfg.variables;
          varmapText = if vars == {} then "" else toPsHashmap vars;
          varmapFile = if varmapText == "" then null else pkgs.writeText "hm-winenv.xml" varmapText;
        in {
          home.extraBuilderCommands = lib.optionalString (varmapText != "") ''ln -s ${varmapFile} $out/hm-winenv.xml'';
        }))
      ];
}
