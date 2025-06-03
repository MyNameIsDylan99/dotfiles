return {

	"hrsh7th/cmp-nvim-lsp-signature-help",

	config = function()
		require("cmp").setup({
			sources = {
				{ name = "nvim_lsp_signature_help" },
			},
		})
	end,
}
