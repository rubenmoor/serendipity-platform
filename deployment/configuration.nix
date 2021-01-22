let
  region = "eu-central-1";
  accessKeyId = "rubm-xmg";
  backend = (import ./../default.nix { }).ghc.backend;
in {
  main = { resources, pkgs, ... }: {
    networking.hostName = "serendipity";
    networking.firewall.allowedTCPPorts = [ 22 80 443 ];
    deployment = {
      targetEnv = "ec2";
      ec2 = {
        inherit region accessKeyId;
        ebsInitialRootDiskSize = 30;
        instanceType = "t2.micro";
        keyPair = resources.ec2KeyPairs.keys;
        elasticIPv4 = resources.elasticIPs.serendipity;
        securityGroups = [ resources.ec2SecurityGroups.serendipityGroup ];
      };
    };
    users.extraUsers.root.shell = pkgs.zsh;
    programs = {
      zsh = {
        enable = true;
        interactiveShellInit = ''
          export TERM=rxvt
        '';
        ohMyZsh = {
          enable = true;
          theme = "robbyrussell";
        };
        syntaxHighlighting.enable = true;
      };
    };
    services = {
      mysql = {
        enable = true;
        package = pkgs.mariadb;
        initialDatabases = [ { name="serendipity"; } ];
        ensureUsers = [
          {
            name = "root";
            ensurePermissions = {
              "serendipity.*" = "ALL PRIVILEGES";
            };
          }
        ];
        settings.mysqld = {
          character-set-server = "utf8mb4";
          collation-server = "utf8mb4_unicode_ci";
        };
      };
    };
    systemd.services = {
      backend = {
        description = "Serendipity Platform server";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        serviceConfig = {
          ExecStart = ''
            ${backend}/bin/backend \
              --mysql-user root \
              --mysql-database serendipity \
              --port 80 \
              --url https://podcast-staging.rubenmoor.net \
              --media-directory /home/www-data/media
          '';
        };
      };
    };
  };

  resources = {
    ec2KeyPairs.keys = { inherit region accessKeyId; };
    ec2SecurityGroups.serendipityGroup = {
      inherit region accessKeyId;
      name = "serendipity-group";
      description = "serendipity.works security group";
      rules = [
        { fromPort = 22; toPort = 22; sourceIp = "0.0.0.0/0"; }
        { fromPort = 80; toPort = 80; sourceIp = "0.0.0.0/0"; }
      ];
    };
    elasticIPs.serendipity = { inherit region accessKeyId; };
  };
}
