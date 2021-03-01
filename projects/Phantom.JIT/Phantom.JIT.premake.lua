
Phantom_lib("Phantom.JIT",  { "Phantom.Code" } -- DEPENDENCIES
	,
	function(Vars) -- include public

	end
	, 
	function(Vars)  -- include private
		defines
		{
			"PHANTOM_LIB_PHANTOM_JIT",
			"_CRT_SECURE_NO_WARNINGS"
		}

		local LLVMRoot = "dependencies/LLVM/"

		includedirs {
			LLVMRoot .. "include",
		}

		files {
			LLVMRoot .. "utils/LLVMVisualizers/llvm.natvis"
		}

		filter { "action:vs2015" }
		includedirs {
			LLVMRoot .. "build/vs2015/include"
		}

		filter { "action:vs2017" }
		includedirs {
			LLVMRoot .. "build/vs2017/include"
		}

		filter { "action:vs2019" }
		includedirs {
			LLVMRoot .. "build/vs2019/include"
		}
					
        Phantom_pch()

		local hauntParams = {}
		hauntParams["IsHaunted"] = true
		Phantom_plugin("Phantom.JIT", Vars, hauntParams)
		
		filter { "platforms:Win32 or x64" }
			warnings "Default"
			removeflags { "FatalWarnings" }
			
		filter {}

	end
	,
	function(Vars) -- link

		if(not Phantom_file_exists("dependencies/LLVM/CMakeLists.txt")) then 
			error("LLVM missing, please mklink to your LLVM repository in Phantom/dependencies folder")
			return
		end
		
		local LLVMRoot = "dependencies/LLVM/"
		local LLVMCmakeBuildDir = "dependencies/LLVM/build/"
		
		filter { "platforms:Win32 or x64", "configurations:debug", "action:vs2015" }
		libdirs { 
			LLVMCmakeBuildDir .. "vs2015/debug/lib"
		}
			
		filter { "platforms:Win32 or x64", "configurations:debug", "action:vs2017" }
		libdirs { 
			LLVMCmakeBuildDir .. "vs2017/debug/lib"
		}
			
		filter { "platforms:Win32 or x64", "configurations:debug", "action:vs2019" }
		libdirs { 
			LLVMCmakeBuildDir .. "vs2019/debug/lib"
		}

		filter { "platforms:Win32 or x64", "configurations:release", "action:vs2015" }
		libdirs { 
			LLVMCmakeBuildDir .. "vs2015/relwithdebinfo/lib"
		}

		filter { "platforms:Win32 or x64", "configurations:release", "action:vs2017" }
		libdirs { 
			LLVMCmakeBuildDir .. "vs2017/relwithdebinfo/lib"
		}

		filter { "platforms:Win32 or x64", "configurations:release", "action:vs2019" }
		libdirs { 
			LLVMCmakeBuildDir .. "vs2019/relwithdebinfo/lib"
		}

		filter {}

		links { "LLVMJITPDB", "LLVMDemangle", "LLVMProfileData", "LLVMBinaryFormat", "LLVMGlobalISel", "LLVMDebugInfoCodeView", "LLVMDebugInfoMSF", "LLVMDebugInfoPDB", "LLVMDebugInfoDWARF", "LLVMCore", "LLVMOption", "LLVMCodeGen", "LLVMAnalysis", "LLVMTarget", "LLVMScalarOpts", "LLVMInterpreter", "LLVMSelectionDAG", "LLVMSupport", "LLVMTableGen", "LLVMX86CodeGen", "LLVMX86Desc", "LLVMX86Info", "LLVMX86AsmParser", "LLVMX86Disassembler", "LLVMMC", "LLVMMCDisassembler", "LLVMMCParser", "LLVMMCJIT", "LLVMExecutionEngine", "LLVMObject", "LLVMTransformUtils", "LLVMInstrumentation", "LLVMBitReader", "LLVMRuntimeDyld", "LLVMAsmPrinter", "LLVMRemarks", "LLVMBitstreamReader", "LLVMCFGuard", "LLVMTextAPI" }

	end

)
