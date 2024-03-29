# klax syntax highlighting for kakoune

provide-module -override klx %{
	add-highlighter shared/klx regions
	add-highlighter shared/klx/other default-region group

	# single line comment
	add-highlighter shared/klx/comment region '#' '$' group
	add-highlighter shared/klx/comment/ fill comment

	# literals
	add-highlighter shared/klx/other/ regex %{\b([_0-9]+|0x[_0-9a-fA-F]+|0b[_01]+)} 0:value

	# keywords and operators
	add-highlighter shared/klx/other/ regex "\b(at|load|store|word|true|false|drop|dup|swap|over|while|if|macro|def|extern|syscall|and|or|not)\b" 0:keyword
	add-highlighter shared/klx/other/ regex "\B(@|\+|-|\*|/|%|<|>|=|<=|>=|!=|<<|>>|&|\||~)" 0:operator

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

