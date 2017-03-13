/* File: ast_decl.cc
 * -----------------
 * Implementation of Decl node classes.
 */
#include "ast_decl.h"
#include "ast_type.h"
#include "ast_stmt.h"
#include "symtable.h"        
         
llvm::Type* GetllvmType(Type* ast_type){
        if ( ast_type == Type::intType )
                return Node::irgen->GetIntType();
        else if ( ast_type == Type::boolType )
                return Node::irgen->GetBoolType();
        else if ( ast_type == Type::voidType )
                return llvm::Type::getVoidTy(*Node::irgen->GetContext());
        else if ( ast_type == Type::floatType )
                return Node::irgen->GetFloatType();
        else if ( ast_type == Type::vec2Type)
                return llvm::VectorType::get(llvm::Type::getFloatTy(*Node::irgen->GetContext()),2);
        else if ( ast_type == Type::vec3Type)
                return llvm::VectorType::get(llvm::Type::getFloatTy(*Node::irgen->GetContext()), 3);
        else if ( ast_type == Type::vec4Type)
                return llvm::VectorType::get(llvm::Type::getFloatTy(*Node::irgen->GetContext()), 4);
        else if ( dynamic_cast<ArrayType*>(ast_type) != NULL ){
                ArrayType* astArrayType = dynamic_cast<ArrayType*>(ast_type);
                llvm::Type* ty = GetllvmType(astArrayType->GetElemType());
                return llvm::ArrayType::get(ty,astArrayType->GetElemCount());
        }
        else{
                return NULL;
        }
}

llvm::Constant* GetllvmConstant(Type* ast_type){
	if ( ast_type == Type::intType )
		return llvm::ConstantInt::get(Node::irgen->GetIntType(),0);
	else if ( ast_type == Type::floatType )
		return llvm::ConstantFP::get(Node::irgen->GetFloatType(),(double)0);
	else if ( ast_type == Type::boolType )
		return llvm::ConstantInt::get(Node::irgen->GetBoolType(), (int) false);
	else if ( ast_type == Type::vec2Type )
		return llvm::ConstantAggregateZero::get(llvm::VectorType::get(llvm::Type::getFloatTy(*Node::irgen->GetContext()),2));
	else if ( ast_type == Type::vec3Type )
		return llvm::ConstantAggregateZero::get(llvm::VectorType::get(llvm::Type::getFloatTy(*Node::irgen->GetContext()),3));
	else if ( ast_type == Type::vec4Type )
		return llvm::ConstantAggregateZero::get(llvm::VectorType::get(llvm::Type::getFloatTy(*Node::irgen->GetContext()),4));
	else if ( dynamic_cast<ArrayType*>(ast_type) != NULL ){
		ArrayType* astArrayType = dynamic_cast<ArrayType*>(ast_type);
                return GetllvmConstant(astArrayType->GetElemType());
		
	} 
		return NULL;
}


Decl::Decl(Identifier *n) : Node(*n->GetLocation()) {
    Assert(n != NULL);
    (id=n)->SetParent(this); 
}

VarDecl::VarDecl(Identifier *n, Type *t, Expr *e) : Decl(n) {
    Assert(n != NULL && t != NULL);
    (type=t)->SetParent(this);
    if (e) (assignTo=e)->SetParent(this);
    typeq = NULL;
}

VarDecl::VarDecl(Identifier *n, TypeQualifier *tq, Expr *e) : Decl(n) {
    Assert(n != NULL && tq != NULL);
    (typeq=tq)->SetParent(this);
    if (e) (assignTo=e)->SetParent(this);
    type = NULL;
}

VarDecl::VarDecl(Identifier *n, Type *t, TypeQualifier *tq, Expr *e) : Decl(n) {
    Assert(n != NULL && t != NULL && tq != NULL);
    (type=t)->SetParent(this);
    (typeq=tq)->SetParent(this);
    if (e) (assignTo=e)->SetParent(this);
}
  
void VarDecl::PrintChildren(int indentLevel) { 
   if (typeq) typeq->Print(indentLevel+1);
   if (type) type->Print(indentLevel+1);
   if (id) id->Print(indentLevel+1);
   if (assignTo) assignTo->Print(indentLevel+1, "(initializer) ");
}

/* VarDecl emit() */
void VarDecl::Emit(){
	llvm::Value* inst = NULL;
	if ( symbolTable->isGlobalScope() ){
		llvm::Constant* constant = NULL;	

		if ( assignTo != NULL ){
			constant = GetllvmConstant(this->type);
		}

		inst = new llvm::GlobalVariable(*irgen->GetOrCreateModule("Program_Module.bc"), GetllvmType(this->GetType()),false,llvm::GlobalValue::ExternalLinkage, constant, this->GetIdentifier()->GetName());
	}
	else {
	//Local Variable Declaration
		inst = new llvm::AllocaInst(GetllvmType(this->GetType()),this->GetIdentifier()->GetName(),irgen->GetEntryBB());
	}

	Symbol varsym(this->GetIdentifier()->GetName(),this,E_VarDecl,inst);
	symbolTable->insert(varsym);
}

FnDecl::FnDecl(Identifier *n, Type *r, List<VarDecl*> *d) : Decl(n) {
    Assert(n != NULL && r!= NULL && d != NULL);
    (returnType=r)->SetParent(this);
    (formals=d)->SetParentAll(this);
    body = NULL;
    returnTypeq = NULL;
}

FnDecl::FnDecl(Identifier *n, Type *r, TypeQualifier *rq, List<VarDecl*> *d) : Decl(n) {
    Assert(n != NULL && r != NULL && rq != NULL&& d != NULL);
    (returnType=r)->SetParent(this);
    (returnTypeq=rq)->SetParent(this);
    (formals=d)->SetParentAll(this);
    body = NULL;
}

void FnDecl::SetFunctionBody(Stmt *b) { 
    (body=b)->SetParent(this);
}

void FnDecl::PrintChildren(int indentLevel) {
    if (returnType) returnType->Print(indentLevel+1, "(return type) ");
    if (id) id->Print(indentLevel+1);
    if (formals) formals->PrintAll(indentLevel+1, "(formals) ");
    if (body) body->Print(indentLevel+1, "(body) ");
}

/* FnDecl Emit() */
void FnDecl::Emit(){
	std::vector<llvm::Type*> args_type;

	for ( int i = 0; i < formals->NumElements(); i++ ){
		args_type.push_back(GetllvmType(formals->Nth(i)->GetType()));
	}

	llvm::ArrayRef<llvm::Type*> argArray(args_type);
	llvm::FunctionType* func_type = llvm::FunctionType::get(GetllvmType(this->GetType()), argArray, false);

	llvm::Function* func = llvm::cast<llvm::Function>(irgen->GetOrCreateModule("Program_Module.bc")->getOrInsertFunction(this->GetIdentifier()->GetName(), func_type));

	Symbol varsym(this->GetIdentifier()->GetName(),this,E_FunctionDecl,func);
	symbolTable->insert(varsym);

	// Function parameters
	symbolTable->push();
	
	llvm::BasicBlock* entryBB = llvm::BasicBlock::Create(*irgen->GetContext(), "entry", func);

	llvm::BasicBlock* nextBB = llvm::BasicBlock::Create(*irgen->GetContext(), "next", func);

	irgen->SetEntryBB(entryBB);
	irgen->SetBasicBlock(nextBB);
	irgen->SetFunction(func);
	
	int i = 0;
	for ( llvm::Function::arg_iterator args = func->arg_begin(); args != func->arg_end(); args++){
		args->setName(formals->Nth(i)->GetIdentifier()->GetName());
		llvm::Value* mem = new llvm::AllocaInst(GetllvmType(formals->Nth(i)->GetType()), "arg"+ to_string(i), entryBB);
		
		Symbol varsym(formals->Nth(i)->GetIdentifier()->GetName(),formals->Nth(i), E_VarDecl, mem);

		symbolTable->insert(varsym);

		new llvm::StoreInst(args,mem,nextBB);
		i++;
	}

	if ( body != NULL ){
		body->Emit();	
	}

	llvm::BranchInst::Create(nextBB,entryBB);
	new llvm::UnreachableInst(*irgen->GetContext(), irgen->GetBasicBlock());
	symbolTable->pop();
}
