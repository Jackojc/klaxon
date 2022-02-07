# klaxon asm syntax highlighting for kakoune

provide-module -override kasm %{
	add-highlighter shared/kasm regions
	add-highlighter shared/kasm/other default-region group

	# single line comment
	add-highlighter shared/kasm/comment region '#' '$' group
	add-highlighter shared/kasm/comment/ fill comment

	# literals
	add-highlighter shared/kasm/other/ regex %{\b[0-9]+} 0:value

	# keywords and operators
	add-highlighter shared/kasm/other/ regex "\b(label|call|ret|push|pop|let|literal)\b" 0:keyword
}

hook global BufCreate .*\.(kasm) %{ set-option buffer filetype kasm }
hook global WinSetOption filetype=kasm %{ require-module kasm }

hook -group wpp-highlight global WinSetOption filetype=kasm %{
	add-highlighter window/kasm ref kasm
	hook -once -always window WinSetOption filetype=.* %{ remove-highlighter window/kasm }
}

# comment token
hook global BufSetOption filetype=kasm %{
	set-option buffer comment_line '#'
	set-option buffer comment_block_begin '#'
	set-option buffer comment_block_end ''
}

