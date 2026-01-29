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
        self.base00 = color_codes.get("base00", "#1D2021")
        self.base01 = color_codes.get("base01", "#3C3836")
        self.base02 = color_codes.get("base02", "#504945")
        self.base03 = color_codes.get("base03", "#665C54")
        self.base04 = color_codes.get("base04", "#BDAE93")
        self.base05 = color_codes.get("base05", "#D5C4A1")
        self.base06 = color_codes.get("base06", "#EBDBB2")
        self.base07 = color_codes.get("base07", "#FBF1C7")
        self.base08 = color_codes.get("base08", "#FB4934")
        self.base09 = color_codes.get("base09", "#FE8019")
        self.base0A = color_codes.get("base0A", "#FABD2F")
        self.base0B = color_codes.get("base0B", "#B8BB26")
        self.base0C = color_codes.get("base0C", "#8EC07C")
        self.base0D = color_codes.get("base0D", "#83A598")
        self.base0E = color_codes.get("base0E", "#D3869B")
        self.base0F = color_codes.get("base0F", "#D65D0E")
