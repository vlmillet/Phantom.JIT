
Phantom_console_app("HelloWorld.JIT",  { "Phantom.JIT" } -- DEPENDENCIES
	,
	function(Vars)  -- include private
		files { "projects/HelloWorld.JIT/**.cpplite" }
	end
	,
	function(Vars) -- link
		filter {}
	end

)
