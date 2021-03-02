# Phantom.JIT
**[Phantom.Code](https://github.com/vlmillet/Phantom.Code)** extension providing Just-In-Time asm x64 compilation based on LLVM 
(see my related project  **[LLVM JIT PDB](https://github.com/vlmillet/llvmjitpdb)** for how on-the-fly PDB debugging is done inside LLVM).

# Installation
- Download, clone or fork **[Phantom](https://github.com/vlmillet/Phantom)** and **[Phantom.Code](https://github.com/vlmillet/Phantom.Code)**.
- Copy (or mklink) every content of this repo, same as for the **Phantom.Code** repo, to your **Phantom** folder.
- Get **[LLVM 11.0](https://github.com/llvm/llvm-project/releases/download/llvmorg-11.0.0/llvm-11.0.0.src.tar.xz)**
- Get **[LLVM JIT PDB](https://github.com/vlmillet/llvmjitpdb)** and follow the instructions in the README to integrate it to the LLVM build pipeline
- Use `CMake` to generate the `LLVM.sln` inside `%YOUR_LLVM_PATH%/build/vs20XX` (XX depending on your Visual Studio version year number, ex: vs2019).
- Check you have the `LLVMJITPDB` project inside `LLVM.sln/Libraries`
- Build `LLVM.sln/Libraries` folder, either using `Debug` or `RelWithDebInfo` configurations (no ~~`Release`~~, or you'll need to modify the `Phantom.JIT.premake.lua`)
- Open a cmd with administrator rights in `%YOUR_PHANTOM_PATH%/dependencies`
- Run into the cmd `mklink /D LLVM %YOUR_LLVM_PATH%` to create a link to LLVM inside the `dependencies` folder of Phantom 
- Run (again) the Premake-vs20XX.bat in the **Phantom** folder
- NOTES : 
    - To build LLVM somewhere else, edit `Phantom.JIT.premake.lua`
    - You can directly download LLVM into `%YOUR_PHANTOM_PATH%/dependencies/LLVM` and skip the cmd/mklink part above

# Running
Just explore, compile, run **HelloWorld.JIT** sample, insert breakpoints inside the **HelloWorld.Jit.cpplite** file and see how it works ! Have fun !
