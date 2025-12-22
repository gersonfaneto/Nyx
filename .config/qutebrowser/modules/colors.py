import os
from dataclasses import dataclass


@dataclass
class ColorsBase16:
    base00: str | None = None
    base01: str | None = None
    base02: str | None = None
    base03: str | None = None
    base04: str | None = None
    base05: str | None = None
    base06: str | None = None
    base07: str | None = None
    base08: str | None = None
    base09: str | None = None
    base0A: str | None = None
    base0B: str | None = None
    base0C: str | None = None
    base0D: str | None = None
    base0E: str | None = None
    base0F: str | None = None

    def get_scheme(self):
        """Extract base16 color scheme from stylix environment variables."""
        hex_codes = [
            os.getenv("STYLIX_BASE_0" + hex(i).replace("0x", "").capitalize())
            for i in range(16)
        ]
        color_codes = {
            f"base0{hex(i).replace('0x', '').capitalize()}": hex_codes[i]
            for i in range(16)
            if hex_codes[i]
        }

        # Fallback to `base16-solarized-dark` if Stylix theme cannot be found.
        self.base00 = color_codes.get("base00", "#002b36")
        self.base01 = color_codes.get("base01", "#073642")
        self.base02 = color_codes.get("base02", "#586e75")
        self.base03 = color_codes.get("base03", "#657b83")
        self.base04 = color_codes.get("base04", "#839496")
        self.base05 = color_codes.get("base05", "#93a1a1")
        self.base06 = color_codes.get("base06", "#eee8d5")
        self.base07 = color_codes.get("base07", "#fdf6e3")
        self.base08 = color_codes.get("base08", "#dc322f")
        self.base09 = color_codes.get("base09", "#cb4b16")
        self.base0A = color_codes.get("base0A", "#b58900")
        self.base0B = color_codes.get("base0B", "#859900")
        self.base0C = color_codes.get("base0C", "#2aa198")
        self.base0D = color_codes.get("base0D", "#268bd2")
        self.base0E = color_codes.get("base0E", "#6c71c4")
        self.base0F = color_codes.get("base0F", "#d33682")
