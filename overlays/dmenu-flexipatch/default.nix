final: prev: {
      dmenu = prev.stdenv.mkDerivation rec {
        pname = "dmenu-flexipatch";
        version = "5.3-20241115";

        src = prev.fetchFromGitHub {
          owner = "bakkeby";
          repo = "dmenu-flexipatch";
          rev = "master";
          sha256 = "sha256-kGqx84CmOi8YG2VxdtknhZMImsnScM+/sLgdEO2fLK0=";
        };

        buildInputs = with prev; [
          xorg.libX11
          xorg.libXft
          xorg.libXinerama
          fontconfig
        ];

        nativeBuildInputs = with prev; [ pkg-config ];

        postPatch = ''
          # Copy the default configuration
          cp patches.def.h patches.h

          # Enable custom patches
          sed -i 's/#define XYW_PATCH 0/#define XYW_PATCH 1/' patches.h
          sed -i 's/#define CENTER_PATCH 0/#define CENTER_PATCH 1/' patches.h
          sed -i 's/#define CTRL_V_TO_PASTE_PATCH 0/#define CTRL_V_TO_PASTE_PATCH 1/' patches.h
          sed -i 's/#define FUZZYMATCH_PATCH 0/#define FUZZYMATCH_PATCH 1/' patches.h
          sed -i 's/#define HIGHLIGHT_PATCH 0/#define HIGHLIGHT_PATCH 1/' patches.h
          sed -i 's/#define BORDER_PATCH 0/#define BORDER_PATCH 1/' patches.h
          sed -i 's/#define BORDER_PATCH 0/#define BORDER_PATCH 1/' patches.h
          sed -i 's/#define VERTFULL_PATCH 0/#define VERTFULL_PATCH 1/' patches.h
        '';

        makeFlags = [ "PREFIX=$(out)" ];

        meta = with prev.lib; {
          description = "dmenu with flexipatch - includes xyw and many other patches";
          homepage = "https://github.com/bakkeby/dmenu-flexipatch";
          license = licenses.mit;
          platforms = platforms.all;
        };
      };
    }
