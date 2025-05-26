{ config, lib, pkgs, ... }:

{
  user.packages = with pkgs; [
    obsidian
    (google-cloud-sdk.withExtraComponents [
      google-cloud-sdk.components.cloud_sql_proxy
      google-cloud-sdk.components.gke-gcloud-auth-plugin
    ])
    kubectl
    python313Packages.playwright
    figma-linux
  ];
}
