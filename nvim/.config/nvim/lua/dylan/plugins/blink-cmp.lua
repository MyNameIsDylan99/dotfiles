return {
	"saghen/blink.cmp",
	enabled = true,
	dependencies = {
		"giuxtaposition/blink-cmp-copilot",
		"L3MON4D3/LuaSnip",
		config = function()
			require("luasnip").setup({})
		end,
	},
	priority = 1,
	opts = function(_, opts)
		-- Merge custom sources with the existing ones from lazyvim
		-- NOTE: by default lazyvim already includes the lazydev source, so not
		-- adding it here again
		opts.sources = vim.tbl_deep_extend("force", opts.sources or {}, {
			default = {
				"lsp",
				"path",
				"snippets",
				"buffer",
				"copilot",
				"luasnip" --[[, "dadbod" --]],
			},
			providers = {
				lsp = {
					name = "lsp",
					enabled = true,
					module = "blink.cmp.sources.lsp",
					-- When linking markdown notes, I would get snippets and text in the
					-- suggestions, I want those to show only if there are no LSP
					-- suggestions
					fallbacks = { "snippets", "luasnip", "buffer" },
					score_offset = 90, -- the higher the number, the higher the priority
				},
				luasnip = {
					name = "luasnip",
					enabled = true,
					module = "blink.cmp.sources.luasnip",
					min_keyword_length = 2,
					fallbacks = { "snippets" },
					score_offset = 85, -- the higher the number, the higher the priority
				},
				path = {
					name = "Path",
					module = "blink.cmp.sources.path",
					score_offset = 3,
					-- When typing a path, I would get snippets and text in the
					-- suggestions, I want those to show only if there are no path
					-- suggestions
					fallbacks = { "snippets", "luasnip", "buffer" },
					opts = {
						trailing_slash = false,
						label_trailing_slash = true,
						get_cwd = function(context)
							return vim.fn.expand(("#%d:p:h"):format(context.bufnr))
						end,
						show_hidden_files_by_default = true,
					},
				},
				buffer = {
					name = "Buffer",
					module = "blink.cmp.sources.buffer",
					min_keyword_length = 2,
				},
				snippets = {
					name = "snippets",
					enabled = true,
					module = "blink.cmp.sources.snippets",
					score_offset = 80, -- the higher the number, the higher the priority
				},
				-- Example on how to configure dadbod found in the main repo
				-- https://github.com/kristijanhusak/vim-dadbod-completion
				--	dadbod = {
				--		name = "Dadbod",
				--		module = "vim_dadbod_completion.blink",
				--		score_offset = 85, -- the higher the number, the higher the priority
				--	},
				-- Third class citizen mf always talking shit
				copilot = {
					name = "copilot",
					enabled = true,
					module = "blink-cmp-copilot",
					min_keyword_length = 2,
					score_offset = -100, -- the higher the number, the higher the priority
					async = true,
					transform_items = function(_, items)
						local CompletionItemKind = require("blink.cmp.types").CompletionItemKind
						local kind_idx = #CompletionItemKind + 1
						CompletionItemKind[kind_idx] = "Copilot"
						for _, item in ipairs(items) do
							item.kind = kind_idx
						end
						return items
					end,
				},
			},
		})

		-- This comes from the luasnip extra, if you don't add it, won't be able to
		-- jump forward or backward in luasnip snippets
		-- https://www.lazyvim.org/extras/coding/luasnip#blinkcmp-optional
		opts.snippets = {
			expand = function(snippet)
				require("luasnip").lsp_expand(snippet)
			end,
			active = function(filter)
				if filter and filter.direction then
					return require("luasnip").jumpable(filter.direction)
				end
				return require("luasnip").in_snippet()
			end,
			jump = function(direction)
				require("luasnip").jump(direction)
			end,
		}

		-- The default preset used by lazyvim accepts completions with enter
		-- I don't like using enter because if on markdown and typing
		-- something, but you want to go to the line below, if you press enter,
		-- the completion will be accepted
		-- https://cmp.saghen.dev/configuration/keymap.html#default
		opts.keymap = {
			preset = "default",
			["<Tab>"] = { "snippet_forward", "fallback" },
			["<S-Tab>"] = { "snippet_backward", "fallback" },

			["<Up>"] = { "select_prev", "fallback" },
			["<Down>"] = { "select_next", "fallback" },
			["<C-p>"] = { "select_prev", "fallback" },
			["<C-n>"] = { "select_next", "fallback" },
			["<C-k>"] = { "select_prev", "fallback" },
			["<C-j>"] = { "select_next", "fallback" },

			["<C-b>"] = { "scroll_documentation_up", "fallback" },
			["<C-f>"] = { "scroll_documentation_down", "fallback" },

			["<C-space>"] = { "show", "show_documentation", "hide_documentation" },
			["<C-e>"] = { "hide", "fallback" },
		}

		opts.appearance = {
			-- Blink does not expose its default kind icons so you must copy them all (or set your custom ones) and add Copilot
			kind_icons = {
				Copilot = "",
				Text = "󰉿",
				Method = "󰊕",
				Function = "󰊕",
				Constructor = "󰒓",

				Field = "󰜢",
				Variable = "󰆦",
				Property = "󰖷",

				Class = "󱡠",
				Interface = "󱡠",
				Struct = "󱡠",
				Module = "󰅩",

				Unit = "󰪚",
				Value = "󰦨",
				Enum = "󰦨",
				EnumMember = "󰦨",

				Keyword = "󰻾",
				Constant = "󰏿",

				Snippet = "󱄽",
				Color = "󰏘",
				File = "󰈔",
				Reference = "󰬲",
				Folder = "󰉋",
				Event = "󱐋",
				Operator = "󰪚",
				TypeParameter = "󰬛",
			},
		}

		return opts
	end,
}
