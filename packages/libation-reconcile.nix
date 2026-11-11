{writeShellApplication}:
writeShellApplication {
  name = "libation-reconcile";
  text = ''
    usage() {
      echo "Usage: libation-reconcile"
    }

    case "''${1-}" in
      -h|--help)
        usage
        exit 0
        ;;
      "")
        ;;
      *)
        usage >&2
        exit 2
        ;;
    esac
    if [ "$#" -ne 0 ]; then
      usage >&2
      exit 2
    fi

    if ! command -v systemctl >/dev/null; then
      echo "Run this command on globalhawk, where the systemd job is installed." >&2
      exit 1
    fi
    sudo systemctl start --no-block libation-reconcile.service
    echo "journalctl --unit=libation-reconcile.service --follow"
    echo "systemctl status libation-reconcile.service"
  '';
}
