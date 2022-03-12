# klaxon syntax highlighting for kakoune

provide-module -override klx %{
	add-highlighter shared/klx regions
	add-highlighter shared/klx/other default-region group

	# single line comment
	add-highlighter shared/klx/comment region '#' '$' group
	add-highlighter shared/klx/comment/ fill comment

	# literals
	add-highlighter shared/klx/other/ regex "(\s|^)\K(\d+)(\s|$)(\d+(\s|$))*" 0:value                              # integer
	add-highlighter shared/klx/other/ regex "(\s|^)\K('\w)(\s|$)('\w(\s|$))*" 0:value                              # character
	add-highlighter shared/klx/other/ regex "(\s|^)\K(\\x[A-Fa-f0-9]{2})(\s|$)(\\x[A-Fa-f0-9]{2}(\s|$))*" 0:value  # hex
	add-highlighter shared/klx/other/ regex "(\s|^)\K(0b[0-1]{8})(\s|$)(0b[0-1]{8}(\s|$))*" 0:value                # binary

	# keywords and operators
	add-highlighter shared/klx/other/ regex "\b(move|copy|remove|while|if|else|fn|decl|extern)\b" 0:keyword

	# string
	add-highlighter shared/klx/string region '"' (?<!\\)(\\\\)*" group
	add-highlighter shared/klx/string/ fill string
	add-highlighter shared/klx/string/ regex \\[\\ntr'"]|\\x[A-Fa-f0-9]{2}|\\b[01]{8} 0:keyword
}

hook global BufCreate .*\.(klx) %{ set-option buffer filetype klx }
hook global WinSetOption filetype=klx %{ require-module klx }

hook -group wpp-highlight global WinSetOption filetype=klx %{
	add-highlighter window/klx ref klx
	hook -once -always window WinSetOption filetype=.* %{ remove-highlighter window/klx }
}

# comment token
hook global BufSetOption filetype=klx %{
	set-option buffer comment_line '#'
	set-option buffer comment_block_begin '#'
	set-option buffer comment_block_end ''
}

