return {
	"L3MON4D3/LuaSnip",
	-- follow latest release.
	-- install jsregexp (optional!).
	build = "make install_jsregexp",
	dependencies = { "rafamadriz/friendly-snippets" },
	config = function()
		local ls = require("luasnip")
		local s = ls.snippet
		local t = ls.text_node
		local i = ls.insert_node
		local c = ls.choice_node
		local f = ls.function_node
		local d = ls.dynamic_node
		local sn = ls.snippet_node
		local extras = require("luasnip.extras")
		local rep = extras.rep

		require("luasnip.loaders.from_vscode").lazy_load()
		require("luasnip.loaders.from_vscode").load({ paths = { "/home/dylan/.config/nvim/lua/dylan/snippets" } })
		ls.filetype_extend("cs", { "unity" })

		-- Adds a summary snippet for C#
		ls.add_snippets("cs", {
			s("summ", {
				t({ "/// <summary>" }),
				t({ "", "/// " }),
				i(0, "Description..."),
				t({ "", "/// </summary>" }),
			}),
		})
	end,
}
