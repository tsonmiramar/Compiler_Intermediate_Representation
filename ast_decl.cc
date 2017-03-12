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
		inst = new llvm::GlobalVariable(*irgen->GetOrCreateModule("Program_Module.bc"), GetllvmType(this->GetType()),false,llvm::GlobalValue::ExternalLinkage, constant, this->GetIdentifier()->GetName());
	}
	else {
		inst = new llvm::AllocaInst(GetllvmType(this->GetType()),this->GetIdentifier()->GetName(),irgen->GetBasicBlock());
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

