return {
	"williamboman/mason.nvim",
	dependencies = {
		"williamboman/mason-lspconfig.nvim",
		"WhoIsSethDaniel/mason-tool-installer.nvim",
	},
	config = function()
		-- import mason
		local mason = require("mason")

		-- import mason-lspconfig
		local mason_lspconfig = require("mason-lspconfig")

		local mason_tool_installer = require("mason-tool-installer")

		-- enable mason and configure icons
		mason.setup({
			ui = {
				icons = {
					package_installed = "✓",
					package_pending = "➜",
					package_uninstalled = "✗",
				},
			},
		})

		mason_lspconfig.setup({
			-- list of servers for mason to install
			ensure_installed = {
				"omnisharp",
				"lua_ls",
				"basedpyright",
			},
		})

		mason_tool_installer.setup({
			ensure_installed = {
				--"prettier", -- prettier formatter - fails to install
				"stylua", -- lua formatter
				--	"isort", -- python formatter - fails to install
				--	"black", -- python formatter - fails to install
				"csharpier", --c_sharp formatter
			},
		})
	end,
}
