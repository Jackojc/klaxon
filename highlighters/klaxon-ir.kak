# klaxon ir syntax highlighting for kakoune

provide-module -override kir %{
	add-highlighter shared/kir regions
	add-highlighter shared/kir/other default-region group

	# single line comment
	add-highlighter shared/kir/comment region '#' '$' group
	add-highlighter shared/kir/comment/ fill comment

	# literals
	add-highlighter shared/kir/other/ regex %{\b[_0-9]+} 0:value

	# keywords and operators
	add-highlighter shared/kir/other/ regex "\b(def|block|ret|end|copy|remove|move|push|jump|branch|call)\b" 0:keyword
}

hook global BufCreate .*\.(kir) %{ set-option buffer filetype kir }
hook global WinSetOption filetype=kir %{ require-module kir }

hook -group wpp-highlight global WinSetOption filetype=kir %{
	add-highlighter window/kir ref kir
	hook -once -always window WinSetOption filetype=.* %{ remove-highlighter window/kir }
}

# comment token
hook global BufSetOption filetype=kir %{
	set-option buffer comment_line '#'
	set-option buffer comment_block_begin '#'
	set-option buffer comment_block_end ''
}

