#include "node.h"
#include "codegen.h"
#include "parser.hpp"
#include "llvm/IR/Function.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

using namespace std;

/* Compile the AST into a module */
void CodeGenContext::generateCode(NBlock &root) {
    std::cout << "Generating code...\n";

    /* Create the top level interpreter function to call as entry */
    // vector<Type *> argTypes;
    // FunctionType *ftype = FunctionType::get(Type::getVoidTy(MyContext), makeArrayRef(argTypes), false);
    // mainFunction = Function::Create(ftype, GlobalValue::ExternalLinkage, "main", module);
    BasicBlock *bblock = BasicBlock::Create(MyContext, "global");

    /* Push a new variable/block context */
    pushBlock(bblock);
    root.codeGen(*this); /* emit bytecode for the toplevel block */
    // ReturnInst::Create(MyContext, currentBlock());
    popBlock();
   
    std::cout << "Code Generation complete.\n";

    legacy::PassManager pm;
    pm.add(createPrintModulePass(outs()));
    pm.run(*module);
}

/* Executes the AST by running the main function */
GenericValue CodeGenContext::runCode() {
    std::cout << "Running code...\n";
    ExecutionEngine *ee = EngineBuilder(unique_ptr<Module>(module)).create();
    ee->finalizeObject();
    vector<GenericValue> noargs;
    GenericValue v = ee->runFunction(mainFunction, noargs);
    std::cout << "Code was run.\n";
    return v;
}

/* Returns an LLVM type based on the identifier */
static Type *typeOf(const NIdentifier &type) {
    if (type.name.compare("int") == 0) {
        return Type::getInt64Ty(MyContext);
    } else if (type.name.compare("double") == 0) {
        return Type::getDoubleTy(MyContext);
    }
    return Type::getVoidTy(MyContext);
}

/* -- Code Generation -- */

Value *NInteger::codeGen(CodeGenContext &context) {
    std::cout << "Creating integer: " << value << endl;
    return ConstantInt::get(Type::getInt64Ty(MyContext), value, true);
}

Value *NDouble::codeGen(CodeGenContext &context) {
    std::cout << "Creating double: " << value << endl;
    return ConstantFP::get(Type::getDoubleTy(MyContext), value);
}

Value *NIdentifier::codeGen(CodeGenContext &context) {
    std::cout << "Creating identifier reference: " << name << endl;
    if (context.locals().find(name) == context.locals().end()) {
        std::cerr << "undeclared variable " << name << endl;
        return NULL;
    }

    // return nullptr;
    return new LoadInst(context.locals()[name]->getType(), context.locals()[name], name, false, context.currentBlock());
}

Value *NMethodCall::codeGen(CodeGenContext &context) {
    Function *function = context.module->getFunction(id.name.c_str());
    if (function == NULL) {
        std::cerr << "no such function " << id.name << endl;
    }
    std::vector<Value *> args;
    ExpressionList::const_iterator it;
    for (it = arguments.begin(); it != arguments.end(); it++) {
        args.push_back((**it).codeGen(context));
    }
    CallInst *call = CallInst::Create(function, makeArrayRef(args), "", context.currentBlock());
    std::cout << "Creating method call: " << id.name << endl;
    return call;
}

Value *NUnaryOperator::codeGen(CodeGenContext &context) {
    std::cout << "Creating unary operation " << op << endl;

    Value *value = operand.codeGen(context);
    bool hasFloatComponent = value->getType()->isFloatingPointTy();

    Instruction::UnaryOps instr;
    switch (op) {
        case TMINUS:
            if (hasFloatComponent) {
                return UnaryOperator::Create(Instruction::FNeg, value, "", context.currentBlock());
            } else {
                Type *intType = Type::getInt64Ty(MyContext);
                return BinaryOperator::Create(Instruction::Sub, ConstantInt::get(intType, 0), value, "",
                                              context.currentBlock());
            }

        case TLOGNOT:
            return value;

    }
    return NULL;
}


Value *NBinaryOperator::codeGen(CodeGenContext &context) {
    std::cout << "Creating binary operation " << op << endl;

    // Generate code for the left-hand side and right-hand side expressions
    Value *LHSValue = lhs.codeGen(context);
    Value *RHSValue = rhs.codeGen(context);

    // Ensure both LHS and RHS values are generated successfully
    if (!LHSValue || !RHSValue) return NULL; // Throw err

    // Determine the type of comparison (integer or floating-point)
    bool hasFloatComponent = LHSValue->getType()->isFloatingPointTy() || RHSValue->getType()->isFloatingPointTy();

    std::cout << "LHS: "  << LHSValue->getType()->getTypeID() << endl;
    std::cout << "RHS: "  << RHSValue->getType()->getTypeID()  << endl;
    std::cout << "Has Float"  << hasFloatComponent << endl;

    if(hasFloatComponent && !LHSValue->getType()->isFloatingPointTy()) {
        LHSValue = new SIToFPInst(LHSValue, Type::getDoubleTy(MyContext), "typeConv", context.currentBlock());
    }
    if(hasFloatComponent && !RHSValue->getType()->isFloatingPointTy()) {
        RHSValue = new SIToFPInst(RHSValue, Type::getDoubleTy(MyContext), "typeConv", context.currentBlock());
    }


    Instruction::BinaryOps instr;
    switch (op) {
        case TPLUS:
            instr = hasFloatComponent ? Instruction::FAdd : Instruction::Add;
            break;
        case TMINUS:
            instr = hasFloatComponent ? Instruction::FSub : Instruction::Sub;
            break;
        case TMUL:
            instr = hasFloatComponent ? Instruction::FMul : Instruction::Mul;
            break;
        case TDIV:
            instr = hasFloatComponent ? Instruction::FDiv : Instruction::SDiv;
            break;

        case TLOGAND:
            instr = Instruction::And;
            break;
        case TLOGOR:
            instr = Instruction::Or;
            break;
        default:
            goto compare_label;
    }
    return BinaryOperator::Create(instr, LHSValue, RHSValue, "", context.currentBlock());

    compare_label:
    // Relational Operators
    CmpInst::Predicate predicate;
    switch (op) {
        case TCEQ:
            predicate = hasFloatComponent ? CmpInst::Predicate::FCMP_OEQ : CmpInst::Predicate::ICMP_EQ;
            break;
        case TCNE:
            predicate = hasFloatComponent ? CmpInst::Predicate::FCMP_ONE : CmpInst::Predicate::ICMP_NE;
            break;
        case TCGE:
            predicate = hasFloatComponent ? CmpInst::Predicate::FCMP_OGE : CmpInst::Predicate::ICMP_SGE;
            break;
        case TCGT:
            predicate = hasFloatComponent ? CmpInst::Predicate::FCMP_OGT : CmpInst::Predicate::ICMP_SGT;
            break;
        case TCLE:
            predicate = hasFloatComponent ? CmpInst::Predicate::FCMP_OLE : CmpInst::Predicate::ICMP_SLE;
            break;
        case TCLT:
            predicate = hasFloatComponent ? CmpInst::Predicate::FCMP_OLT : CmpInst::Predicate::ICMP_SLT;
            break;
        default:
            return NULL;
    }
    return CmpInst::Create(
            hasFloatComponent ? Instruction::FCmp : Instruction::ICmp,
            predicate, LHSValue, RHSValue, "", context.currentBlock());
}

Value *NAssignment::codeGen(CodeGenContext &context) {
    std::cout << "Creating assignment for " << lhs.name << endl;
    if (context.locals().find(lhs.name) == context.locals().end()) {
        std::cerr << "undeclared variable " << lhs.name << endl;
        return NULL;
    }
    return new StoreInst(rhs.codeGen(context), context.locals()[lhs.name], false, context.currentBlock());
}

Value *NBlock::codeGen(CodeGenContext &context) {
    StatementList::const_iterator it;
    Value *last = NULL;
    for (it = statements.begin(); it != statements.end(); it++) {
        std::cout << "Generating code for " << typeid(**it).name() << endl;
        last = (**it).codeGen(context);
    }
    std::cout << "Creating block" << endl;
    return last;
}

Value *NExpressionStatement::codeGen(CodeGenContext &context) {
    std::cout << "Generating code for " << typeid(expression).name() << endl;
    return expression.codeGen(context);
}

Value *NConditionalStatement::codeGen(CodeGenContext &context) {
    std::cout << "Generating if statement code for " << typeid(cond).name() << endl;
    Value *CondV = cond.codeGen(context);
    if (!CondV)
        return NULL;

    // Compare condition value with 0.
    Builder.SetInsertPoint(context.currentBlock());
    if (CondV->getType()->isFloatingPointTy()) {
        CondV = Builder.CreateFCmpONE(CondV, ConstantFP::get(MyContext, APFloat(0.0)), "ifcond");
    } else if (CondV->getType()->isIntegerTy()) {
        CondV = Builder.CreateICmpNE(CondV, ConstantInt::get(MyContext, APInt(CondV->getType()->getIntegerBitWidth(), 0)), "ifcond");
    } else {
        std::cerr << "Unsupported type found in if condition"<< endl;
        return NULL;
    }

    Function *TheFunction = Builder.GetInsertBlock()->getParent();

    // Create blocks for the then, else and merge cases
    BasicBlock *ThenBB = BasicBlock::Create(MyContext, "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(MyContext, "else", TheFunction);
    BasicBlock *MergeBB = BasicBlock::Create(MyContext, "ifcont", TheFunction);

    context.pushBlock(ThenBB, true);
    thenBlock.codeGen(context);
    context.popBlock();
    Builder.SetInsertPoint(ThenBB);
    Builder.CreateBr(MergeBB);

    context.pushBlock(ElseBB, true);
    elseBlock.codeGen(context);
    context.popBlock();
    Builder.SetInsertPoint(ElseBB);
    Builder.CreateBr(MergeBB);

    Builder.SetInsertPoint(context.currentBlock());
    Builder.CreateCondBr(CondV, ThenBB, ElseBB);

    context.replaceBlock(MergeBB);
    return CondV;
}

Value *NLoopStatement::codeGen(CodeGenContext &context) {
    std::cout << "Generating loop statement code for " << typeid(cond).name() << endl;

    Builder.SetInsertPoint(context.currentBlock());
    Function *TheFunction = Builder.GetInsertBlock()->getParent();

    // Create blocks for the conditional, body and after loop cases
    BasicBlock *ConditionBB = BasicBlock::Create(MyContext, "condition", TheFunction);
    BasicBlock *BodyBB = BasicBlock::Create(MyContext, "body", TheFunction);
    BasicBlock *AfterLoopBB = BasicBlock::Create(MyContext, "afterloop", TheFunction);

    Builder.CreateBr(ConditionBB);
    
    Builder.SetInsertPoint(ConditionBB);
    context.pushBlock(ConditionBB, true);
    Value *CondV = cond.codeGen(context);
    context.popBlock();
    if (!CondV)
        return NULL;
    if (CondV->getType()->isFloatingPointTy()) {
        CondV = Builder.CreateFCmpONE(CondV, ConstantFP::get(MyContext, APFloat(0.0)), "ifcond");
    } else if (CondV->getType()->isIntegerTy()) {
        CondV = Builder.CreateICmpNE(CondV, ConstantInt::get(MyContext, APInt(CondV->getType()->getIntegerBitWidth(), 0)), "ifcond");
    } else {
        std::cerr << "Unsupported type found in if condition"<< endl;
        return NULL;
    }
    Builder.CreateCondBr(CondV, BodyBB, AfterLoopBB);

    Builder.SetInsertPoint(BodyBB);
    context.pushBlock(BodyBB, true);
    body.codeGen(context);
    context.popBlock();
    Builder.CreateBr(ConditionBB);

    context.replaceBlock(AfterLoopBB);
    return 0;
}

Value *NReturnStatement::codeGen(CodeGenContext &context) {
    std::cout << "Generating return code for " << typeid(expression).name() << endl;
    Value *returnValue = expression.codeGen(context);
    context.setCurrentReturnValue(returnValue);
    return returnValue;
}

Value *NVariableDeclaration::codeGen(CodeGenContext &context) {
    std::cout << "Creating variable declaration " << type.name << " " << id.name << endl;
    AllocaInst *alloc = new AllocaInst(typeOf(type), 4, id.name.c_str(), context.currentBlock());
    context.locals()[id.name] = alloc;
    if (assignmentExpr != NULL) {
        NAssignment assn(id, *assignmentExpr);
        assn.codeGen(context);
    }
    return alloc;
}

Value *NExternDeclaration::codeGen(CodeGenContext &context) {
    vector<Type *> argTypes;
    VariableList::const_iterator it;
    for (it = arguments.begin(); it != arguments.end(); it++) {
        argTypes.push_back(typeOf((**it).type));
    }
    FunctionType *ftype = FunctionType::get(typeOf(type), makeArrayRef(argTypes), false);
    Function *function = Function::Create(ftype, GlobalValue::ExternalLinkage, id.name.c_str(), context.module);
    return function;
}

Value *NFunctionDeclaration::codeGen(CodeGenContext &context) {
    vector<Type *> argTypes;
    VariableList::const_iterator it;
    for (it = arguments.begin(); it != arguments.end(); it++) {
        argTypes.push_back(typeOf((**it).type));
    }
    FunctionType *ftype = FunctionType::get(typeOf(type), makeArrayRef(argTypes), false);
    Function *function = Function::Create(ftype, GlobalValue::ExternalWeakLinkage, id.name.c_str(), context.module);
    BasicBlock *bblock = BasicBlock::Create(MyContext, "entry", function, 0);

    context.pushBlock(bblock);

    Function::arg_iterator argsValues = function->arg_begin();
    Value *argumentValue;

    for (it = arguments.begin(); it != arguments.end(); it++) {
        (**it).codeGen(context);

        argumentValue = &*argsValues++;
        argumentValue->setName((*it)->id.name.c_str());
        StoreInst *inst = new StoreInst(argumentValue, context.locals()[(*it)->id.name], false, bblock);
    }

    block.codeGen(context);
    ReturnInst::Create(MyContext, context.getCurrentReturnValue(), context.currentBlock());

    context.popBlock();
    std::cout << "Creating function: " << id.name << endl;
    return function;
}
