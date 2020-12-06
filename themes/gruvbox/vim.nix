variant: contrast: {
  settings = {
    background = variant;
  };
  themePlugin = pkgs: pkgs.vimPlugins.gruvbox;
  extraConfig = ''
  autocmd vimenter * ++nested colorscheme gruvbox

  let ${if variant == "dark" then "g:gruvbox_contrast_dark" else "g:gruvbox_contrast_light"} = "${contrast}"
  nnoremap <silent> [oh :call gruvbox#hls_show()<CR>
  nnoremap <silent> ]oh :call gruvbox#hls_hide()<CR>
  nnoremap <silent> coh :call gruvbox#hls_toggle()<CR>

  nnoremap * :let @/ = ""<CR>:call gruvbox#hls_show()<CR>*
  nnoremap / :let @/ = ""<CR>:call gruvbox#hls_show()<CR>/
  nnoremap ? :let @/ = ""<CR>:call gruvbox#hls_show()<CR>?
  '';
}
