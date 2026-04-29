-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git", "clone", "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- Haskell Language Server (built-in LSP config)
vim.lsp.config.hls = {
  cmd = { "haskell-language-server-wrapper", "--lsp" },
  filetypes = { "haskell", "lhaskell", "cabal" },
  root_markers = { "hie.yaml", "stack.yaml", "cabal.project", "*.cabal", "package.yaml", "flake.nix" },
}
vim.lsp.enable("hls")

-- Clojure Language Server (built-in LSP config)
vim.lsp.config.clojure_lsp = {
  cmd = { "clojure-lsp" },
  filetypes = { "clojure", "edn" },
  root_markers = { "project.clj", "deps.edn", "build.boot", "shadow-cljs.edn", "bb.edn" },
}
vim.lsp.enable("clojure_lsp")

-- OCaml Language Server (built-in LSP config)
vim.lsp.config.ocamllsp = {
  cmd = { "ocamllsp" },
  filetypes = { "ocaml", "ocaml.menhir", "ocaml.interface", "ocaml.ocamllex", "reason" },
  root_markers = { "dune-project", "dune-workspace", "*.opam", ".ocamlformat" },
}
vim.lsp.enable("ocamllsp")

-- LSP keybindings and code lens (attached per-buffer when a language server starts)
vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    local opts = { buffer = args.buf }
    vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
    vim.keymap.set("n", "gr", vim.lsp.buf.references, opts)
    vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
    vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, opts)
    vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, opts)
    vim.keymap.set("n", "<leader>cl", vim.lsp.codelens.run, opts)
    if client and client.supports_method("textDocument/codeLens") then
      vim.api.nvim_create_autocmd({ "BufEnter", "InsertLeave" }, {
        buffer = args.buf,
        callback = vim.lsp.codelens.refresh,
      })
      vim.lsp.codelens.refresh({ bufnr = args.buf })
    end
  end,
})

-- Plugins
require("lazy").setup({
  -- Colorschemes
  { "miikanissi/modus-themes.nvim", lazy = true },
  { "rebelot/kanagawa.nvim", lazy = true },
  { "catppuccin/nvim", name = "catppuccin", lazy = true },
  { "folke/tokyonight.nvim", lazy = true },
  { "sainnhe/gruvbox-material", lazy = true },
  {
    "nvim-orgmode/orgmode",
    event = "VeryLazy",
    ft = { "org" },
    config = function()
      require("orgmode").setup({})
    end,
  },
  {
    "Olical/conjure",
    ft = { "clojure", "edn" },
  },
  {
    "Julian/lean.nvim",
    event = { "BufReadPre *.lean", "BufNewFile *.lean" },
    dependencies = {
      "neovim/nvim-lspconfig",
      "nvim-lua/plenary.nvim",
    },
    opts = {
      mappings = true,
    },
  },
  {
    "github/copilot.vim",
    lazy = false,
  },
  {
    "CopilotC-Nvim/CopilotChat.nvim",
    dependencies = {
      "github/copilot.vim",
      "nvim-lua/plenary.nvim",
    },
    opts = {},
    cmd = { "CopilotChat", "CopilotChatExplain", "CopilotChatFix", "CopilotChatTests" },
    keys = {
      { "<leader>cc", "<cmd>CopilotChat<cr>", desc = "Copilot Chat" },
    },
  },
  {
    "tpope/vim-fugitive",
    cmd = { "Git", "Gvdiffsplit", "Gblame" },
    keys = {
      { "<leader>gs", "<cmd>Git<cr>", desc = "Git status" },
      { "<leader>gb", "<cmd>Git blame<cr>", desc = "Git blame" },
      { "<leader>gd", "<cmd>Gvdiffsplit<cr>", desc = "Git diff" },
      { "<leader>gl", "<cmd>Git log --oneline<cr>", desc = "Git log" },
    },
  },
  {
    "lewis6991/gitsigns.nvim",
    event = { "BufReadPre", "BufNewFile" },
    opts = {
      on_attach = function(bufnr)
        local gs = require("gitsigns")
        local opts = { buffer = bufnr }
        vim.keymap.set("n", "]c", gs.next_hunk, opts)
        vim.keymap.set("n", "[c", gs.prev_hunk, opts)
        vim.keymap.set("n", "<leader>hs", gs.stage_hunk, opts)
        vim.keymap.set("n", "<leader>hr", gs.reset_hunk, opts)
        vim.keymap.set("n", "<leader>hu", gs.undo_stage_hunk, opts)
        vim.keymap.set("n", "<leader>hp", gs.preview_hunk, opts)
        vim.keymap.set("n", "<leader>hb", function() gs.blame_line({ full = true }) end, opts)
      end,
    },
  },
  {
    "lervag/vimtex",
    ft = { "tex" },
    init = function()
      vim.g.vimtex_view_method = "skim"
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter",
    branch = "master",
    build = ":TSUpdate",
    config = function()
      require("nvim-treesitter.configs").setup({
        ensure_installed = { "haskell", "clojure", "ocaml", "latex" },
        highlight = { enable = true },
      })
    end,
  },
  {
    "direnv/direnv.vim",
    lazy = false,
  },
})

-- Default colorscheme (change after trying others)
vim.cmd.colorscheme("tokyonight-night")

-- Font (Neovide/GUI only)
vim.o.guifont = "Iosevka Term Slab:h16"
