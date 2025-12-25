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
        self.base00 = color_codes.get("base00", "#1d2021")
        self.base01 = color_codes.get("base01", "#3c3836")
        self.base02 = color_codes.get("base02", "#504945")
        self.base03 = color_codes.get("base03", "#665c54")
        self.base04 = color_codes.get("base04", "#bdae93")
        self.base05 = color_codes.get("base05", "#d5c4a1")
        self.base06 = color_codes.get("base06", "#ebdbb2")
        self.base07 = color_codes.get("base07", "#fbf1c7")
        self.base08 = color_codes.get("base08", "#fb4934")
        self.base09 = color_codes.get("base09", "#fe8019")
        self.base0A = color_codes.get("base0A", "#fabd2f")
        self.base0B = color_codes.get("base0B", "#b8bb26")
        self.base0C = color_codes.get("base0C", "#8ec07c")
        self.base0D = color_codes.get("base0D", "#83a598")
        self.base0E = color_codes.get("base0E", "#d3869b")
        self.base0F = color_codes.get("base0F", "#d65d0e")
