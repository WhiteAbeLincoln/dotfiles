{pkgs, config, ...}:

{
  extensions = with pkgs.nur.repos.rycee.firefox-addons; [
    https-everywhere
    multi-account-containers
    react-devtools
    reddit-enhancement-suite
    terms-of-service-didnt-read
    tridactyl
    ublock-origin
    lastpass-password-manager
    darkreader
    cookie-autodelete
  ];
  profiles.default = {
    id = 0;
    isDefault = true;
    settings = {
      "extensions.pocket.enabled" = false; # remove pocket extension
      "browser.newtabpage.activity-stream.section.highlights.includePocket" = false; # remove pocket from new-tab
      "browser.newtabpage.activity-stream.feeds.section.topstories" = false;
      "browser.newtabpage.activity-stream.feeds.system.topstories" = false;
      "media.eme.enabled" = true; # enable drm content
      "signon.autofillForms" = false; # disable form filling
      "signon.rememberSignons" = false; # disable password saving
    };
  };
  extraPackageConfig = {
    enableTridactylNative = true;
  };
}
