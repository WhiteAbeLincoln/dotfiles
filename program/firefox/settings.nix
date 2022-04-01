{pkgs, config, ...}:

{
  extensions = with pkgs.nur.repos.rycee.firefox-addons; [
    multi-account-containers
    temporary-containers
    violentmonkey
    react-devtools
    reddit-enhancement-suite
    terms-of-service-didnt-read
    ublock-origin
    bitwarden
    cookie-autodelete
  ];
  profiles.default = {
    id = 0;
    isDefault = true;
    settings = {
      "browser.urlbar.placeholderName" = "DuckDuckGo";
      "extensions.pocket.enabled" = false; # remove pocket extension
      "browser.newtabpage.activity-stream.section.highlights.includePocket" = false; # remove pocket from new-tab
      "browser.newtabpage.activity-stream.feeds.section.topstories" = false;
      "browser.newtabpage.activity-stream.feeds.system.topstories" = false;
      "browser.newtabpage.activity-stream.feeds.sections" = false;
      "media.eme.enabled" = true; # enable drm content
      "signon.autofillForms" = false; # disable form filling
      "signon.rememberSignons" = false; # disable password saving
      "dom.security.https_only_mode" = true;
      "dom.security.https_only_mode_ever_enabled" = true;
    };
  };
}
