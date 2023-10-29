#include <stack>
#include <typeinfo>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include "llvm/Pass.h"
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Bitstream/BitstreamReader.h>
#include <llvm/Bitstream/BitstreamWriter.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/Support/raw_ostream.h>

using namespace llvm;

class NBlock;

static LLVMContext MyContext;
static IRBuilder<> Builder(MyContext);


class CodeGenBlock {
public:
    BasicBlock *block;
    Value *returnValue;
    std::map<std::string, Value *>& locals;

    CodeGenBlock(): locals(*(new std::map<std::string, Value *>())) {}

    CodeGenBlock(std::map<std::string, Value *>& parentLocals): locals(parentLocals) {}

};

class CodeGenContext {
    std::stack<CodeGenBlock *> blocks;
    Function *mainFunction;

public:

    Module *module;

    CodeGenContext() { module = new Module("main", MyContext); }

    void generateCode(NBlock &root);

    GenericValue runCode();

    std::map<std::string, Value *> &locals() { return blocks.top()->locals; }

    BasicBlock *currentBlock() { return blocks.top()->block; }

    void pushBlock(BasicBlock *block, bool inheritLocals = false) {
        CodeGenBlock* newCodeBlock;
        if(inheritLocals) {
            newCodeBlock = new CodeGenBlock(locals());
        } else {
            newCodeBlock = new CodeGenBlock();
        }
        newCodeBlock->returnValue = NULL;
        newCodeBlock->block = block;
        blocks.push(newCodeBlock);
    }

    void popBlock() {
        CodeGenBlock *top = blocks.top();
        blocks.pop();
        delete top;
    }

    void replaceBlock(BasicBlock *block) {
        CodeGenBlock* newCodeBlock = new CodeGenBlock(locals());
        newCodeBlock->returnValue = blocks.top()->returnValue;
        newCodeBlock->block = block;
        popBlock();
        blocks.push(newCodeBlock);
    }


    void setCurrentReturnValue(Value *value) { blocks.top()->returnValue = value; }

    Value *getCurrentReturnValue() { return blocks.top()->returnValue; }
};
