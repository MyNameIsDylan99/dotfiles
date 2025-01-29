return {
	{
		"scottmckendry/cyberdream.nvim",
		lazy = false,
		priority = 1000,
		config = function()
			require("cyberdream").setup({
				-- Enable transparent background
				transparent = true,

				-- Enable italics comments
				italic_comments = false,

				-- Replace all fillchars with ' ' for the ultimate clean look
				hide_fillchars = false,

				-- Modern borderless telescope theme - also applies to fzf-lua
				-- Turning this on makes telescope not transparent anymore
				borderless_telescope = false,

				-- Set terminal colors used in `:terminal`
				terminal_colors = true,

				-- Improve start up time by caching highlights. Generate cache with :CyberdreamBuildCache and clear with :CyberdreamClearCache
				cache = false,

				theme = {
					variant = "default", -- use "light" for the light variant. Also accepts "auto" to set dark or light colors based on the current value of `vim.o.background`
					saturation = 1, -- accepts a value between 0 and 1. 0 will be fully desaturated (greyscale) and 1 will be the full color (default)

					highlights = {
						-- Highlight groups to override, adding new groups is also possible
						-- See `:h highlight-groups` for a list of highlight groups or run `:hi` to see all groups and their current values

						-- Example:
						--	TelescopeNormal = { fg = "NONE", bg = "NONE" },
						--	TelescopeBorder = { fg = "NONE", bg = "NONE" },
						--	TelescopePromptNormal = { fg = "NONE", bg = "NONE" },
						--	TelescopePromptBorder = { fg = "NONE", bg = "NONE" },
						--	TelescopePromptPrefix = { fg = "NONE", bg = "NONE" },
						--	TelescopeMatching = { fg = "NONE", bg = "NONE" },
						--	TelescopePromptTitle = { fg = "NONE", bg = "NONE" },
						--	TelescopeResultsTitle = { fg = "NONE", bg = "NONE" },
						--	TelescopePreviewTitle = { fg = "NONE", bg = "NONE" },
						--	TelescopeSelection = { fg = "NONE", bg = "NONE" },
						--	TelescopeSelectionCaret = { fg = "NONE", bg = "NONE" },
						--	TelescopeMultiSelection = { fg = "NONE", bg = "NONE" },
						--	TelescopeMultiSelectionCaret = { fg = "NONE", bg = "NONE" },
						--	TelescopePreviewLine = { fg = "NONE", bg = "NONE" },
						--	TelescopePreviewMatch = { fg = "NONE", bg = "NONE" },
						--	TelescopeResultsSeparator = { fg = "NONE", bg = "NONE" },
						--	TelescopeResultsLine = { fg = "NONE", bg = "NONE" },
						--	TelescopeHelpBorder = { fg = "NONE", bg = "NONE" },
						--	TelescopeHelpNormal = { fg = "NONE", bg = "NONE" },
						--	TelescopeHelpTitle = { fg = "NONE", bg = "NONE" },

						-- Complete list can be found in `lua/cyberdream/theme.lua`
					},
					--
					--					-- Override a highlight group entirely using the color palette
					--					overrides = function(colors) -- NOTE: This function nullifies the `highlights` option
					--						-- Example:
					--						return {
					--							Comment = { fg = colors.green, bg = "NONE", italic = true },
					--							["@property"] = { fg = colors.magenta, bold = true },
					--						}
					--					end,
					--
					--					-- Override a color entirely
					--					colors = {
					--						-- For a list of colors see `lua/cyberdream/colours.lua`
					--						-- Example:
					--						bg = "#000000",
					--						green = "#00ff00",
					--						magenta = "#ff00ff",
					--					},
				},

				-- Disable or enable colorscheme extensions
				extensions = {
					telescope = true,
					notify = true,
					mini = true,
				},
			})

			vim.cmd("colorscheme cyberdream")
		end,
	},
}
