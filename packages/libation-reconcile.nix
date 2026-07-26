{
  writeShellApplication,
  kubectl,
  jq,
  coreutils,
}:
writeShellApplication {
  name = "libation-reconcile";
  runtimeInputs = [kubectl jq coreutils];
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

    job_name="libation-reconcile-manual-$(date -u +%Y%m%d%H%M%S)-$RANDOM"

    kubectl create job \
      --from=cronjob/libation-reconcile \
      "$job_name" \
      --namespace library \
      --dry-run=client \
      --output=json \
      | jq 'del(.spec.template.spec.initContainers)' \
      | kubectl apply --filename=-

    echo "kubectl logs --namespace library --follow job/$job_name"
    echo "kubectl wait --namespace library --for=condition=complete --timeout=48h job/$job_name"
  '';
}
