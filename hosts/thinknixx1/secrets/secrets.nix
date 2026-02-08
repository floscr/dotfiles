let key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDetq/NjvtvZ46agj6d4mBkjlCR0j+XzcW8e5qHh37BT/PxWazNWtDVrAmg7oImkvyTpXPzyMnJwS/V/eztDLo/LN+NhMLSiBqt5fAu0mXyQrLXWDrVpsV3uHRYj/+dkACdpJ5mgiMff5KNgO8Ls7LXXhlTTxqWxELc9DuiC+cjuiXVyNKPKFoDOZmeBVZfAMFmUbpLVgukyk/QPTpnZ9uO0Df9XpFJWfYWEGzcx/tykZiff7YRh45A2fwOlt/oVMUj/9wDZ8FX06TE80b1R+Igt+iQQ5kwNpfSnk65HoYS53FMy7FoSxjnP76Htlp2lhwPvKLHKf2NKYa5+1bqRsoel1EtZ3Fk70IAVka2SN2mlX49qXyT2IOfazZIpb6YarkrAkD+/aYI+r6WOjiEaMeVSNliplXLrX4nAPOgBRes8/MK4mhFVY5kaxpoWWyEZwk9DAw1iiQIXCzFTSM7qFToB73DbEs8QbSameA4U9hgBWac9VUd1cp/5IacoahfrD8= floscr@thinknixx1";
in {
  "mullvad.age".publicKeys = [ key ];
  "wg-hetzner.age".publicKeys = [ key ];
}
