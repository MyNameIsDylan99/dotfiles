-- Pull in the wezterm API
local wezterm = require("wezterm")

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices

config.font = wezterm.font("FiraCode Nerd Font Mono")
config.font_size = 16

config.enable_tab_bar = false

config.window_decorations = "RESIZE"
config.window_background_opacity = 1.0
config.colors = require("cyberdream")

config.default_prog = { 'pwsh.exe', '-NoLogo' }

-- and finally, return the configuration to wezterm
return config
