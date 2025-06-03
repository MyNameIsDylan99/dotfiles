-- Creating an autocommand that autocommits and pushes my changes in my notes directory
vim.api.nvim_create_autocmd("VimLeave", {
	callback = function()
		local cwd = vim.fn.getcwd()
		local notes_path = vim.fn.expand("~/notes")

		-- Check if we're in ~/notes or a subdirectory
		if cwd:sub(1, #notes_path) == notes_path then
			local status = vim.fn.system("git -C " .. notes_path .. " status --porcelain")

			-- Only commit if there are changes
			if status ~= "" then
				local datetime = os.date("%Y-%m-%d %H:%M:%S")
				vim.fn.system("git -C " .. notes_path .. " add .")
				vim.fn.system("git -C " .. notes_path .. " commit -m 'Auto commit: " .. datetime .. "'")
				vim.fn.system("git -C " .. notes_path .. " push origin main") -- adjust 'main' if your branch is different
			end
		end
	end,
})

-- Creating an autocommand that autocommits and pushes my changes in my dotfiles directory
-- vim.api.nvim_create_autocmd("VimLeave", {
-- 	callback = function()
-- 		local cwd = vim.fn.getcwd()
-- 		local notes_path = vim.fn.expand("~/dotfiles")
--
-- 		-- Check if we're in ~/notes or a subdirectory
-- 		if cwd:sub(1, #notes_path) == notes_path then
-- 			local status = vim.fn.system("git -C " .. notes_path .. " status --porcelain")
--
-- 			-- Only commit if there are changes
-- 			if status ~= "" then
-- 				local datetime = os.date("%Y-%m-%d %H:%M:%S")
-- 				vim.fn.system("git -C " .. notes_path .. " add .")
-- 				vim.fn.system("git -C " .. notes_path .. " commit -m 'Auto commit: " .. datetime .. "'")
-- 				vim.fn.system("git -C " .. notes_path .. " push origin main") -- adjust 'main' if your branch is different
-- 			end
-- 		end
-- 	end,
-- })
