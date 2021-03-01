// license [
// This file is part of the Phantom project. Copyright 2011-2020 Vivien Millet.
// Distributed under the MIT license. Text available here at
// https://github.com/vlmillet/phantom
// ]

#include <iostream>
#include <malloc.h>
#include <phantom/lang/CompiledSource.h>
#include <phantom/lang/Compiler.h>
#include <phantom/lang/Function.h>
#include <phantom/lang/Source.h>

// some standard reflection registration (do your owns as you wish)

#include <phantom/function>
#include <phantom/jit/autoload.h>
#include <phantom/main>
#include <phantom/utils/Path.h>

void HelloWorldPrint(const char* _text)
{
    std::cout << _text << std::endl;
}

PHANTOM_FUNCTION(void, HelloWorldPrint, (const char*));

int main(int argc, char** argv)
{
    using namespace phantom::lang;

    phantom::jit::autoload(); // ensure loading of Phantom.JIT.dll through .lib

    Main app(main, "HelloWorld.JIT", argc, argv);

    auto sessionId = Compiler::Get()->newSession({}, {{"configuration", "Debug"}});

    auto compiledSource = Compiler::Get()->buildSource(
    sessionId, phantom::Path(__FILE__).parentPath().childPath("HelloWorld.JIT.cpplite").genericString());

    PHANTOM_ASSERT(compiledSource);

    compiledSource->dumpMessages();

    if (!compiledSource->hasError())
    {
        if (phantom::lang::Function* main = compiledSource->getCurrentBuild().getSource()->getFunction(
            "main", phantom::lang::Types{PHANTOM_TYPEOF(int), PHANTOM_TYPEOF(char**)}))
        {
            if (main->getReturnType() == PHANTOM_TYPEOF(int))
            {
                return main->call<int>(argc, argv);
            }
        }
    }
    return 1;
}
