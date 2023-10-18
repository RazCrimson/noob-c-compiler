#include <iostream>
#include "codegen.h"
#include "node.h"
#include "llvm/ADT/Optional.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"

using namespace std;

extern int yyparse();

extern NBlock *programBlock;

void open_file(const char *filename) {
    // openfile
    freopen(filename, "r", stdin);
}

void createCoreFunctions(CodeGenContext &context);

int main(int argc, char **argv) {
    if (argc > 1) {
        open_file(argv[1]);
    }
    yyparse();
    cout << programBlock << endl;

    // Initialize the target registry etc.
    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

    CodeGenContext context;
    createCoreFunctions(context);
    context.generateCode(*programBlock);

    auto TargetTriple = LLVMGetDefaultTargetTriple();
    context.module->setTargetTriple(TargetTriple);

    std::string Error;
    auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);

    // Print an error and exit if we couldn't find the requested target.
    // This generally occurs if we've forgotten to initialise the
    // TargetRegistry or we have a bogus target triple.
    if (!Target) {
        errs() << Error;
        return 1;
    }

    auto CPU = "generic";
    auto Features = "";

    TargetOptions opt;
    auto RM = llvm::Optional<Reloc::Model>(Reloc::PIC_);
    auto TheTargetMachine =
            Target->createTargetMachine(TargetTriple, CPU, Features, opt, RM);

    context.module->setDataLayout(TheTargetMachine->createDataLayout());

    std::error_code EC;
    auto Filename = "output.o";
    raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

    if (EC) {
        errs() << "Could not open file: " << EC.message();
        return 1;
    }

    legacy::PassManager pass;

    auto FileType = CodeGenFileType::CGFT_ObjectFile;

    if (TheTargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
        errs() << "TheTargetMachine can't emit a file of this type";
        return 1;
    }

    pass.run(*(context.module));
    dest.flush();

    outs() << "Wrote " << Filename << "\n";

    return 0;
}