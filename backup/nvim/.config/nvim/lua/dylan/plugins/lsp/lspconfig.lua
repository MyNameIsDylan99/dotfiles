return {
	"neovim/nvim-lspconfig",
	event = { "BufReadPre", "BufNewFile" },
	dependencies = {
		"hrsh7th/cmp-nvim-lsp",
		{ "antosha417/nvim-lsp-file-operations", config = true },
		{ "folke/neodev.nvim", opts = {} },
	},

	config = function()
		-- import lspconfig plugin
		local lspconfig = require("lspconfig")

		vim.filetype.add({
			extension = {
				compute = "hlsl",
				hlsl = "hlsl",
			},
		})

		--Sadly hlsl-lsp not working well

		--		vim.api.nvim_create_autocmd({ "BufReadPre", "BufNewFile" }, {
		--			pattern = { "*.hlsl", "*.compute" },
		--			callback = function()
		--				vim.lsp.start({
		--					name = "hlsl",
		--					cmd = { "hlsl-lsp" },
		--					root_dir = vim.fs.dirname(vim.fs.find({ ".git" }, { upward = true })[1]),
		--					on_attach = function(client, bufnr)
		--						print(vim.inspect(client.server_capabilities))
		--					end,
		--				})
		--			end,
		--		})

		-- import mason_lspconfig plugin
		local mason_lspconfig = require("mason-lspconfig")

		-- import cmp-nvim-lsp plugin
		local cmp_nvim_lsp = require("cmp_nvim_lsp")

		local keymap = vim.keymap -- for conciseness

		vim.api.nvim_create_autocmd("LspAttach", {
			group = vim.api.nvim_create_augroup("UserLspConfig", {}),
			callback = function(ev)
				-- Buffer local mappings.
				-- See `:help vim.lsp.*` for documentation on any of the below functions
				local opts = { buffer = ev.buf, silent = true }

				-- set keybinds
				opts.desc = "Show LSP references"
				keymap.set("n", "gR", "<cmd>Telescope lsp_references<CR>", opts) -- show definition, references

				opts.desc = "Go to declaration"
				keymap.set("n", "gD", vim.lsp.buf.declaration, opts) -- go to declaration

				opts.desc = "Show LSP definitions"
				keymap.set("n", "gd", "<cmd>Telescope lsp_definitions<CR>", opts) -- show lsp definitions

				opts.desc = "Show LSP implementations"
				keymap.set("n", "gi", "<cmd>Telescope lsp_implementations<CR>", opts) -- show lsp implementations

				opts.desc = "Show LSP type definitions"
				keymap.set("n", "gt", "<cmd>Telescope lsp_type_definitions<CR>", opts) -- show lsp type definitions

				opts.desc = "See available code actions"
				keymap.set({ "n", "v" }, "<leader>ca", vim.lsp.buf.code_action, opts) -- see available code actions, in visual mode will apply to selection

				opts.desc = "Smart rename"
				keymap.set("n", "<leader>rn", vim.lsp.buf.rename, opts) -- smart rename

				opts.desc = "Show buffer diagnostics"
				keymap.set("n", "<leader>D", "<cmd>Telescope diagnostics bufnr=0<CR>", opts) -- show  diagnostics for file

				opts.desc = "Show line diagnostics"
				keymap.set("n", "<leader>d", vim.diagnostic.open_float, opts) -- show diagnostics for line

				opts.desc = "Go to previous diagnostic"
				keymap.set("n", "[d", vim.diagnostic.goto_prev, opts) -- jump to previous diagnostic in buffer

				opts.desc = "Go to next diagnostic"
				keymap.set("n", "]d", vim.diagnostic.goto_next, opts) -- jump to next diagnostic in buffer

				opts.desc = "Show documentation for what is under cursor"
				keymap.set("n", "K", vim.lsp.buf.hover, opts) -- show documentation for what is under cursor

				opts.desc = "Restart LSP"
				keymap.set("n", "<leader>rs", ":LspRestart<CR>", opts) -- mapping to restart lsp if necessary
			end,
		})

		-- used to enable autocompletion (assign to every lsp server config)
		local capabilities = cmp_nvim_lsp.default_capabilities()

		-- Change the Diagnostic symbols in the sign column (gutter)
		-- (not in youtube nvim video)
		local signs = { Error = " ", Warn = " ", Hint = "󰠠 ", Info = " " }
		for type, icon in pairs(signs) do
			local hl = "DiagnosticSign" .. type
			vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = "" })
		end

		mason_lspconfig.setup_handlers({
			-- default handler for installed servers
			function(server_name)
				lspconfig[server_name].setup({
					capabilities = capabilities,
				})
			end,
			["svelte"] = function()
				-- configure svelte server
				lspconfig["svelte"].setup({
					capabilities = capabilities,
					on_attach = function(client, bufnr)
						vim.api.nvim_create_autocmd("BufWritePost", {
							pattern = { "*.js", "*.ts" },
							callback = function(ctx)
								-- Here use ctx.match instead of ctx.file
								client.notify("$/onDidChangeTsOrJsFile", { uri = ctx.match })
							end,
						})
					end,
				})
			end,
			["graphql"] = function()
				-- configure graphql language server
				lspconfig["graphql"].setup({
					capabilities = capabilities,
					filetypes = { "graphql", "gql", "svelte", "typescriptreact", "javascriptreact" },
				})
			end,
			["emmet_ls"] = function()
				-- configure emmet language server
				lspconfig["emmet_ls"].setup({
					capabilities = capabilities,
					filetypes = {
						"html",
						"typescriptreact",
						"javascriptreact",
						"css",
						"sass",
						"scss",
						"less",
						"svelte",
					},
				})
			end,
			["lua_ls"] = function()
				-- configure lua server (with special settings)
				lspconfig["lua_ls"].setup({
					capabilities = capabilities,
					filetypes = {
						"lua",
					},
					settings = {
						Lua = {
							-- make the language server recognize "vim" global
							diagnostics = {
								globals = { "vim" },
							},
							completion = {
								callSnippet = "Replace",
							},
						},
					},
				})
			end,

			["basedpyright"] = function()
				local util = require("lspconfig.util")

				local root_files = {
					"pyproject.toml",
					"setup.py",
					"setup.cfg",
					"requirements.txt",
					"Pipfile",
					"pyrightconfig.json",
					".git",
				}

				local function organize_imports()
					local params = {
						command = "basedpyright.organizeimports",
						arguments = { vim.uri_from_bufnr(0) },
					}

					local clients = vim.lsp.get_clients({
						bufnr = vim.api.nvim_get_current_buf(),
						name = "basedpyright",
					})
					for _, client in ipairs(clients) do
						client.request("workspace/executeCommand", params, nil, 0)
					end
				end

				local function set_python_path(path)
					local clients = vim.lsp.get_clients({
						bufnr = vim.api.nvim_get_current_buf(),
						name = "basedpyright",
					})
					for _, client in ipairs(clients) do
						if client.settings then
							client.settings.python =
								vim.tbl_deep_extend("force", client.settings.python or {}, { pythonPath = path })
						else
							client.config.settings =
								vim.tbl_deep_extend("force", client.config.settings, { python = { pythonPath = path } })
						end
						client.notify("workspace/didChangeConfiguration", { settings = nil })
					end
				end

				lspconfig["basedpyright"].setup({

					capabilities = capabilities,
					cmd = { "basedpyright-langserver", "--stdio" },
					filetypes = { "python" },
					root_dir = function(fname)
						return util.root_pattern(unpack(root_files))(fname)
					end,
					single_file_support = true,
					settings = {
						basedpyright = {
							analysis = {
								autoSearchPaths = true,
								useLibraryCodeForTypes = true,
								diagnosticMode = "openFilesOnly",
							},
						},
					},
					commands = {
						PyrightOrganizeImports = {
							organize_imports,
							description = "Organize Imports",
						},
						PyrightSetPythonPath = {
							set_python_path,
							description = "Reconfigure basedpyright with the provided python path",
							nargs = 1,
							complete = "file",
						},
					},
					docs = {
						description = [[
https://detachhead.github.io/basedpyright

`basedpyright`, a static type checker and language server for python
]],
					},
				})
			end,
			["omnisharp"] = function()
				-- configure lua server (with special settings)
				lspconfig["omnisharp"].setup({
					capabilities = capabilities,
					cmd = { "dotnet", "/home/dylan/.local/share/nvim/mason/packages/omnisharp/libexec/OmniSharp.dll" },

					settings = {
						FormattingOptions = {
							-- Enables support for reading code style, naming convention and analyzer
							-- settings from .editorconfig.
							EnableEditorConfigSupport = true,
							-- Specifies whether 'using' directives should be grouped and sorted during
							-- document formatting.
							OrganizeImports = nil,
						},
						MsBuild = {
							-- If true, MSBuild project system will only load projects for files that
							-- were opened in the editor. This setting is useful for big C# codebases
							-- and allows for faster initialization of code navigation features only
							-- for projects that are relevant to code that is being edited. With this
							-- setting enabled OmniSharp may load fewer projects and may thus display
							-- incomplete reference lists for symbols.
							LoadProjectsOnDemand = nil,
						},
						RoslynExtensionsOptions = {
							-- Enables support for roslyn analyzers, code fixes and rulesets.
							EnableAnalyzersSupport = nil,
							-- Enables support for showing unimported types and unimported extension
							-- methods in completion lists. When committed, the appropriate using
							-- directive will be added at the top of the current file. This option can
							-- have a negative impact on initial completion responsiveness,
							-- particularly for the first few completion sessions after opening a
							-- solution.
							EnableImportCompletion = nil,
							-- Only run analyzers against open files when 'enableRoslynAnalyzers' is
							-- true
							AnalyzeOpenDocumentsOnly = nil,
						},
						Sdk = {
							-- Specifies whether to include preview versions of the .NET SDK when
							-- determining which version to use for project loading.
							IncludePrereleases = true,
						},
					},
				})
			end,
		})
	end,
}
