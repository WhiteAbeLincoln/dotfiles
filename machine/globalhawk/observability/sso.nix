{config, ...}: {
  services.k3s.workloads.module = {
    applications.monitoring-stack.helm.releases.kube-prometheus-stack.values.grafana = {
      adminUser = "admin";
      admin = {
        existingSecret = "grafana-secrets";
        passwordKey = "admin-password";
      };
      env.GF_SECURITY_ADMIN_USER = "admin";
      envValueFrom = {
        GF_SECURITY_ADMIN_PASSWORD.secretKeyRef = {
          name = "grafana-secrets";
          key = "admin-password";
        };
        GF_AUTH_GENERIC_OAUTH_CLIENT_SECRET.secretKeyRef = {
          name = "grafana-secrets";
          key = "oidc-client-secret";
        };
      };
      "grafana.ini" = {
        server.root_url = "https://grafana${config.homelab.ingressSuffix}";
        auth = {
          disable_login_form = false;
          oauth_auto_login = false;
        };
        "auth.generic_oauth" = {
          enabled = true;
          name = "Authelia";
          client_id = "grafana";
          scopes = "openid profile email groups";
          auth_url = "https://auth${config.homelab.ingressSuffix}/api/oidc/authorization";
          token_url = "https://auth${config.homelab.ingressSuffix}/api/oidc/token";
          api_url = "https://auth${config.homelab.ingressSuffix}/api/oidc/userinfo";
          login_attribute_path = "preferred_username";
          name_attribute_path = "name";
          groups_attribute_path = "groups";
          use_pkce = true;
          auth_style = "InHeader";
          allow_sign_up = true;
          role_attribute_path = "contains(groups[*], 'admins') && 'Admin' || 'None'";
          role_attribute_strict = true;
          allow_assign_grafana_admin = false;
        };
      };
    };
  };
}
