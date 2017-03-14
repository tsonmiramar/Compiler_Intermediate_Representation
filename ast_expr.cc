/* File: ast_expr.cc
 * -----------------
 * Implementation of expression node classes.
 */

#include <string.h>
#include "ast_expr.h"
#include "ast_type.h"
#include "ast_decl.h"
#include "symtable.h"

IntConstant::IntConstant(yyltype loc, int val) : Expr(loc) {
    value = val;
}
void IntConstant::PrintChildren(int indentLevel) { 
    printf("%d", value);
}

void IntConstant::Emit(){
	this->type = Type::intType;
	this->llvm_val = llvm::ConstantInt::get(Node::irgen->GetIntType(), this->value);
}

FloatConstant::FloatConstant(yyltype loc, double val) : Expr(loc) {
    value = val;
}
void FloatConstant::PrintChildren(int indentLevel) { 
    printf("%g", value);
}

void FloatConstant::Emit(){
	this->type = Type::floatType;
	this->llvm_val = llvm::ConstantFP::get(Node::irgen->GetFloatType(), this->value);
}

BoolConstant::BoolConstant(yyltype loc, bool val) : Expr(loc) {
    value = val;
}

void BoolConstant::Emit(){
	this->type = Type::boolType;
	this->llvm_val = llvm::ConstantInt::get(Node::irgen->GetBoolType(), this->value);
}

void BoolConstant::PrintChildren(int indentLevel) { 
    printf("%s", value ? "true" : "false");
}

VarExpr::VarExpr(yyltype loc, Identifier *ident) : Expr(loc) {
    Assert(ident != NULL);
    this->id = ident;
}

void VarExpr::PrintChildren(int indentLevel) {
    id->Print(indentLevel+1);
}

void VarExpr::Emit(){
	Symbol* varsym = symbolTable->findAllScope(this->id->GetName());
	VarDecl* varDecl = dynamic_cast<VarDecl*>(varsym->decl);
	llvm::Value* var_val = varsym->value;

	this->type = varDecl->GetType();
	this->llvm_val = new llvm::LoadInst(var_val,"",irgen->GetBasicBlock());
}

Operator::Operator(yyltype loc, const char *tok) : Node(loc) {
    Assert(tok != NULL);
    strncpy(tokenString, tok, sizeof(tokenString));
}

void Operator::PrintChildren(int indentLevel) {
    printf("%s",tokenString);
}

bool Operator::IsOp(const char *op) const {
    return strcmp(tokenString, op) == 0;
}

CompoundExpr::CompoundExpr(Expr *l, Operator *o, Expr *r) 
  : Expr(Join(l->GetLocation(), r->GetLocation())) {
    Assert(l != NULL && o != NULL && r != NULL);
    (op=o)->SetParent(this);
    (left=l)->SetParent(this); 
    (right=r)->SetParent(this);
}

CompoundExpr::CompoundExpr(Operator *o, Expr *r) 
  : Expr(Join(o->GetLocation(), r->GetLocation())) {
    Assert(o != NULL && r != NULL);
    left = NULL; 
    (op=o)->SetParent(this);
    (right=r)->SetParent(this);
}

CompoundExpr::CompoundExpr(Expr *l, Operator *o) 
  : Expr(Join(l->GetLocation(), o->GetLocation())) {
    Assert(l != NULL && o != NULL);
    (left=l)->SetParent(this);
    (op=o)->SetParent(this);
}

void CompoundExpr::PrintChildren(int indentLevel) {
   if (left) left->Print(indentLevel+1);
   op->Print(indentLevel+1);
   if (right) right->Print(indentLevel+1);
}
   
ConditionalExpr::ConditionalExpr(Expr *c, Expr *t, Expr *f)
  : Expr(Join(c->GetLocation(), f->GetLocation())) {
    Assert(c != NULL && t != NULL && f != NULL);
    (cond=c)->SetParent(this);
    (trueExpr=t)->SetParent(this);
    (falseExpr=f)->SetParent(this);
}

void ConditionalExpr::PrintChildren(int indentLevel) {
    cond->Print(indentLevel+1, "(cond) ");
    trueExpr->Print(indentLevel+1, "(true) ");
    falseExpr->Print(indentLevel+1, "(false) ");
}
ArrayAccess::ArrayAccess(yyltype loc, Expr *b, Expr *s) : LValue(loc) {
    (base=b)->SetParent(this); 
    (subscript=s)->SetParent(this);
}

void ArrayAccess::PrintChildren(int indentLevel) {
    base->Print(indentLevel+1);
    subscript->Print(indentLevel+1, "(subscript) ");
}

void ArrayAccess::Emit(){
	subscript->Emit();
	
	VarExpr* var_expr = dynamic_cast<VarExpr*>(base);

	Symbol* varsym = symbolTable->findAllScope(var_expr->GetIdentifier()->GetName());
	VarDecl* varDecl = dynamic_cast<VarDecl*>(varsym->decl);
	
	ArrayType* arr_type = dynamic_cast<ArrayType*>(varDecl->GetType());
	this->type = arr_type->GetElemType();
	
	vector<llvm::Value*> indices;
	indices.push_back(llvm::ConstantInt::get(irgen->GetIntType(), 0));
	indices.push_back(subscript->llvm_val);

	llvm::ArrayRef<llvm::Value*> indRef = llvm::ArrayRef<llvm::Value*>(indices);

	this->llvm_val = llvm::GetElementPtrInst::Create(varsym->value, indRef, "", irgen->GetBasicBlock());
	
}
     
FieldAccess::FieldAccess(Expr *b, Identifier *f) 
  : LValue(b? Join(b->GetLocation(), f->GetLocation()) : *f->GetLocation()) {
    Assert(f != NULL); // b can be be NULL (just means no explicit base)
    base = b; 
    if (base) base->SetParent(this); 
    (field=f)->SetParent(this);
}


void FieldAccess::PrintChildren(int indentLevel) {
    if (base) base->Print(indentLevel+1);
    field->Print(indentLevel+1);
}

void FieldAccess::Emit(){
	//TODO: not yet implemented
}

/* AssignExpr Emit() */
void AssignExpr::Emit(){
	VarExpr* lhs = dynamic_cast<VarExpr*>(left);
	ArrayAccess* lhs_arr = dynamic_cast<ArrayAccess*>(left);
	FieldAccess* lhs_field = dynamic_cast<FieldAccess*>(left);
 	
	if ( op->IsOp("=") ){
		right->Emit();
		if ( dynamic_cast<ArrayAccess*>(right) != NULL)
                	right->llvm_val = new llvm::LoadInst(right->llvm_val,"",irgen->GetBasicBlock());

		if ( lhs != NULL ){
			Symbol* varsym = symbolTable->findAllScope(lhs->GetIdentifier()->GetName());
			new llvm::StoreInst(right->llvm_val, varsym->value, irgen->GetBasicBlock());
			this->llvm_val = right->llvm_val;
		}
		else if ( lhs_arr != NULL ){
			lhs_arr->Emit();
			new llvm::StoreInst(right->llvm_val,lhs_arr->llvm_val, irgen->GetBasicBlock());
			
			this->llvm_val = right->llvm_val;
			this->type = right->type;
		}
	}
	else if ( op->IsOp("+=") ){
		if ( lhs != NULL ){	
			lhs->Emit();
			Symbol* varsym = symbolTable->findAllScope(lhs->GetIdentifier()->GetName());
			right->Emit();
			if ( dynamic_cast<ArrayAccess*>(right) != NULL ){
				right->llvm_val = new llvm::LoadInst(right->llvm_val,"", irgen->GetBasicBlock());
			}
			
			if ( lhs->type == Type::intType || lhs->type == Type::boolType){
				this->llvm_val = llvm::BinaryOperator::CreateAdd(lhs->llvm_val,right->llvm_val,"",irgen->GetBasicBlock());
			}
			else if ( lhs->type == Type::floatType ){
				this->llvm_val = llvm::BinaryOperator::CreateFAdd(lhs->llvm_val,right->llvm_val,"",irgen->GetBasicBlock());
			}

			new llvm::StoreInst(this->llvm_val, varsym->value, irgen->GetBasicBlock());
			this->type = right->type;
		} 
		else if ( lhs_arr != NULL ){
			lhs_arr->Emit();
			llvm::LoadInst* load_temp = new llvm::LoadInst(lhs_arr->llvm_val, "", irgen->GetBasicBlock());
		
			right->Emit();
			if ( dynamic_cast<ArrayAccess*>(right) != NULL ){
				right->llvm_val = new llvm::LoadInst(right->llvm_val, "", irgen->GetBasicBlock());
			}
			
			if ( lhs_arr->type == Type::intType || lhs_arr->type == Type::boolType)
				this->llvm_val = llvm::BinaryOperator::CreateAdd(load_temp,right->llvm_val,"", irgen->GetBasicBlock());
			else if ( lhs_arr->type == Type::floatType)
				this->llvm_val = llvm::BinaryOperator::CreateFAdd(load_temp,right->llvm_val,"", irgen->GetBasicBlock());

			this->type = right->type;
			
			lhs_arr->Emit();
			new llvm::StoreInst(this->llvm_val, lhs_arr->llvm_val, irgen->GetBasicBlock());
		}

	}
	else if ( op->IsOp("-=") ){
		if ( lhs != NULL ){
                        lhs->Emit();
			Symbol* varsym = symbolTable->findAllScope(lhs->GetIdentifier()->GetName());
                        right->Emit();
                        if ( dynamic_cast<ArrayAccess*>(right) != NULL ){
                                right->llvm_val = new llvm::LoadInst(right->llvm_val,"", irgen->GetBasicBlock());
                        }

                        if ( lhs->type == Type::intType || lhs->type == Type::boolType){
                                this->llvm_val = llvm::BinaryOperator::CreateSub(lhs->llvm_val,right->llvm_val,"",irgen->GetBasicBlock());
                        }
                        else if ( lhs->type == Type::floatType ){
                                this->llvm_val = llvm::BinaryOperator::CreateFSub(lhs->llvm_val,right->llvm_val,"",irgen->GetBasicBlock());
                        }

                        new llvm::StoreInst(this->llvm_val, varsym->value, irgen->GetBasicBlock());
                        this->type = right->type;
                }
                else if ( lhs_arr != NULL ){
                        lhs_arr->Emit();
                        llvm::LoadInst* load_temp = new llvm::LoadInst(lhs_arr->llvm_val, "", irgen->GetBasicBlock());

                        right->Emit();
                        if ( dynamic_cast<ArrayAccess*>(right) != NULL ){
                                right->llvm_val = new llvm::LoadInst(right->llvm_val, "", irgen->GetBasicBlock());
                        }

                        if ( lhs_arr->type == Type::intType || lhs_arr->type == Type::boolType)
                                this->llvm_val = llvm::BinaryOperator::CreateSub(load_temp,right->llvm_val,"", irgen->GetBasicBlock());
                        else if ( lhs_arr->type == Type::floatType)
                                this->llvm_val = llvm::BinaryOperator::CreateFSub(load_temp,right->llvm_val,"", irgen->GetBasicBlock());

                        this->type = right->type;

                        lhs_arr->Emit();
                        new llvm::StoreInst(this->llvm_val, lhs_arr->llvm_val, irgen->GetBasicBlock());
               }
	}
	else if ( op->IsOp("*=") ){
		if ( lhs != NULL ){
                        lhs->Emit();
			Symbol* varsym = symbolTable->findAllScope(lhs->GetIdentifier()->GetName());
                        right->Emit();
                        if ( dynamic_cast<ArrayAccess*>(right) != NULL ){
                                right->llvm_val = new llvm::LoadInst(right->llvm_val,"", irgen->GetBasicBlock());
                        }

                        if ( lhs->type == Type::intType || lhs->type == Type::boolType){
                                this->llvm_val = llvm::BinaryOperator::CreateMul(lhs->llvm_val,right->llvm_val,"",irgen->GetBasicBlock());
                        }
                        else if ( lhs->type == Type::floatType ){
                                this->llvm_val = llvm::BinaryOperator::CreateFMul(lhs->llvm_val,right->llvm_val,"",irgen->GetBasicBlock());
                        }

                        new llvm::StoreInst(this->llvm_val, varsym->value, irgen->GetBasicBlock());
                        this->type = right->type;
                }
                else if ( lhs_arr != NULL ){
                        lhs_arr->Emit();
                        llvm::LoadInst* load_temp = new llvm::LoadInst(lhs_arr->llvm_val, "", irgen->GetBasicBlock());

                        right->Emit();
                        if ( dynamic_cast<ArrayAccess*>(right) != NULL ){
                                right->llvm_val = new llvm::LoadInst(right->llvm_val, "", irgen->GetBasicBlock());
                        }

                        if ( lhs_arr->type == Type::intType || lhs_arr->type == Type::boolType)
                                this->llvm_val = llvm::BinaryOperator::CreateMul(load_temp,right->llvm_val,"", irgen->GetBasicBlock());
                        else if ( lhs_arr->type == Type::floatType)
                                this->llvm_val = llvm::BinaryOperator::CreateFMul(load_temp,right->llvm_val,"", irgen->GetBasicBlock());

                        this->type = right->type;

                        lhs_arr->Emit();
                        new llvm::StoreInst(this->llvm_val, lhs_arr->llvm_val, irgen->GetBasicBlock());
                }
	}
	else if ( op->IsOp("/=")) {
		if ( lhs != NULL ){
                        lhs->Emit();
			Symbol* varsym = symbolTable->findAllScope(lhs->GetIdentifier()->GetName());
                        right->Emit();
                        if ( dynamic_cast<ArrayAccess*>(right) != NULL ){
                                right->llvm_val = new llvm::LoadInst(right->llvm_val,"", irgen->GetBasicBlock());
                        }

                        if ( lhs->type == Type::intType || lhs->type == Type::boolType){
                                this->llvm_val = llvm::BinaryOperator::CreateSDiv(lhs->llvm_val,right->llvm_val,"",irgen->GetBasicBlock());
                        }
                        else if ( lhs->type == Type::floatType ){
                                this->llvm_val = llvm::BinaryOperator::CreateFDiv(lhs->llvm_val,right->llvm_val,"",irgen->GetBasicBlock());
                        }

                        new llvm::StoreInst(this->llvm_val, varsym->value, irgen->GetBasicBlock());
                        this->type = right->type;
                }
                else if ( lhs_arr != NULL ){
                        lhs_arr->Emit();
                        llvm::LoadInst* load_temp = new llvm::LoadInst(lhs_arr->llvm_val, "", irgen->GetBasicBlock());

                        right->Emit();
                        if ( dynamic_cast<ArrayAccess*>(right) != NULL ){
                                right->llvm_val = new llvm::LoadInst(right->llvm_val, "", irgen->GetBasicBlock());
                        }

                        if ( lhs_arr->type == Type::intType || lhs_arr->type == Type::boolType)
                                this->llvm_val = llvm::BinaryOperator::CreateSDiv(load_temp,right->llvm_val,"", irgen->GetBasicBlock());
                        else if ( lhs_arr->type == Type::floatType)
                                this->llvm_val = llvm::BinaryOperator::CreateFDiv(load_temp,right->llvm_val,"", irgen->GetBasicBlock());

                        this->type = right->type;

                        lhs_arr->Emit();
                        new llvm::StoreInst(this->llvm_val, lhs_arr->llvm_val, irgen->GetBasicBlock());
                }
	}
}

/* ArithmeticExpr Emit() */
void ArithmeticExpr::Emit(){
	
	left->Emit();
	
	right->Emit();

	this->type = left->type;

	if (op->IsOp("+")){
		if ( left->type == Type::intType || left->type == Type::boolType){
			this->llvm_val = llvm::BinaryOperator::CreateAdd(left->llvm_val,right->llvm_val,"",irgen->GetBasicBlock());
		}	
		else if ( left->type == Type::floatType ){
			this->llvm_val = llvm::BinaryOperator::CreateFAdd(left->llvm_val,right->llvm_val,"",irgen->GetBasicBlock());
		}
	}

	else if (op->IsOp("-")){
		if ( left->type == Type::intType || left->type == Type::boolType){
			this->llvm_val = llvm::BinaryOperator::CreateSub(left->llvm_val,right->llvm_val,"",irgen->GetBasicBlock());
		}
		else if ( left->type == Type::floatType ){
			this->llvm_val = llvm::BinaryOperator::CreateFSub(left->llvm_val,right->llvm_val,"",irgen->GetBasicBlock());
		}
	}
	else if (op->IsOp("*")){
		if ( left->type == Type::intType || left->type == Type::boolType){
			this->llvm_val = llvm::BinaryOperator::CreateMul(left->llvm_val,right->llvm_val,"",irgen->GetBasicBlock());
		}
		else if ( left->type == Type::floatType ){
			this->llvm_val = llvm::BinaryOperator::CreateFMul(left->llvm_val,right->llvm_val,"",irgen->GetBasicBlock());
		}
	}
	else if ( op->IsOp("/")){
		if ( left->type == Type::intType || left->type == Type::boolType){
			this->llvm_val = llvm::BinaryOperator::CreateSDiv(left->llvm_val,right->llvm_val,"",irgen->GetBasicBlock());
		}
		else if ( left->type == Type::floatType ){
			this->llvm_val = llvm::BinaryOperator::CreateFDiv(left->llvm_val,right->llvm_val,"",irgen->GetBasicBlock());
		}
	}

}

/* Equality Expr Emit() */
void EqualityExpr::Emit() {
	left->Emit();
	right->Emit();

	this->type = Type::boolType;

	if ( op->IsOp("==") ){
		if ( left->type == Type::intType || left->type == Type::boolType) {
			llvm::CmpInst::Predicate pred = llvm::ICmpInst::ICMP_EQ;
			if ( right->type == Type::floatType )
				pred = llvm::FCmpInst::FCMP_OEQ;

			this->llvm_val = llvm::CmpInst::Create(llvm::CmpInst::ICmp, pred, left->llvm_val, right->llvm_val, "", irgen->GetBasicBlock());
		}
		else if ( left->type == Type::floatType ){
			llvm::CmpInst::Predicate pred = llvm::FCmpInst::FCMP_OEQ;
			if ( right->type != left->type )
				pred = llvm::ICmpInst::ICMP_EQ;

			this->llvm_val = llvm::CmpInst::Create(llvm::CmpInst::FCmp, pred, left->llvm_val, right->llvm_val, "", irgen->GetBasicBlock());
		}
	}
	else if ( op->IsOp("!=") ){
		if ( left->type == Type::intType || left->type == Type::boolType) {
                        llvm::CmpInst::Predicate pred = llvm::ICmpInst::ICMP_NE;
                        if ( right->type == Type::floatType )
                                pred = llvm::FCmpInst::FCMP_ONE;

                        this->llvm_val = llvm::CmpInst::Create(llvm::CmpInst::ICmp, pred, left->llvm_val, right->llvm_val, "", irgen->GetBasicBlock());
                }
                else if ( left->type == Type::floatType ){
                        llvm::CmpInst::Predicate pred = llvm::FCmpInst::FCMP_ONE;
                        if ( right->type != left->type )
                                pred = llvm::ICmpInst::ICMP_NE;

                        this->llvm_val = llvm::CmpInst::Create(llvm::CmpInst::FCmp, pred, left->llvm_val, right->llvm_val, "", irgen->GetBasicBlock());
                }	
	}		
}

void LogicalExpr::Emit(){
	left->Emit();
	right->Emit();
	this->type = Type::boolType;

	if ( op->IsOp("&&") ){
		this->llvm_val = llvm::BinaryOperator::CreateAnd(left->llvm_val, right->llvm_val, "", irgen->GetBasicBlock());
	}
	else if ( op->IsOp("||") ){
		this->llvm_val = llvm::BinaryOperator::CreateOr(left->llvm_val, right->llvm_val, "", irgen->GetBasicBlock());
	}
}

void RelationalExpr::Emit(){
	left->Emit();
	right->Emit();
	
	this->type = Type::boolType;

	if ( op->IsOp(">") ){
                if ( left->type == Type::intType || left->type == Type::boolType) {
                        llvm::CmpInst::Predicate pred = llvm::ICmpInst::ICMP_SGT;
                        if ( right->type == Type::floatType )
                                pred = llvm::FCmpInst::FCMP_OGT;

                        this->llvm_val = llvm::CmpInst::Create(llvm::CmpInst::ICmp,pred, left->llvm_val, right->llvm_val, "", irgen->GetBasicBlock());
                }
                else if ( left->type == Type::floatType ){
                        llvm::CmpInst::Predicate pred = llvm::FCmpInst::FCMP_OGT;
                        if ( right->type != left->type )
                                pred = llvm::ICmpInst::ICMP_SGT;

                        this->llvm_val = llvm::CmpInst::Create(llvm::CmpInst::FCmp,pred, left->llvm_val, right->llvm_val, "", irgen->GetBasicBlock());
                }
        }
	else if ( op->IsOp("<") ){
                if ( left->type == Type::intType || left->type == Type::boolType) {
                        llvm::CmpInst::Predicate pred = llvm::ICmpInst::ICMP_SLT;
                        if ( right->type == Type::floatType )
                                pred = llvm::FCmpInst::FCMP_OLT;

                        this->llvm_val = llvm::CmpInst::Create(llvm::CmpInst::ICmp,pred, left->llvm_val, right->llvm_val, "", irgen->GetBasicBlock());
                }
                else if ( left->type == Type::floatType ){
                        llvm::CmpInst::Predicate pred = llvm::FCmpInst::FCMP_OLT;
                        if ( right->type != left->type )
                                pred = llvm::ICmpInst::ICMP_SLT;

                        this->llvm_val = llvm::CmpInst::Create(llvm::CmpInst::FCmp,pred, left->llvm_val, right->llvm_val, "", irgen->GetBasicBlock());
                }
        }
	else if ( op->IsOp(">=") ){
                if ( left->type == Type::intType || left->type == Type::boolType) {
                        llvm::CmpInst::Predicate pred = llvm::ICmpInst::ICMP_SGE;
                        if ( right->type == Type::floatType )
                                pred = llvm::FCmpInst::FCMP_OGE;

                        this->llvm_val = llvm::CmpInst::Create(llvm::CmpInst::ICmp,pred, left->llvm_val, right->llvm_val, "", irgen->GetBasicBlock());
                }
                else if ( left->type == Type::floatType ){
                        llvm::CmpInst::Predicate pred = llvm::FCmpInst::FCMP_OGE;
                        if ( right->type != left->type )
                                pred = llvm::ICmpInst::ICMP_SGE;

                        this->llvm_val = llvm::CmpInst::Create(llvm::CmpInst::FCmp,pred, left->llvm_val, right->llvm_val, "", irgen->GetBasicBlock());
                }
        }
	else if ( op->IsOp("<=") ){
                if ( left->type == Type::intType || left->type == Type::boolType) {
                        llvm::CmpInst::Predicate pred = llvm::ICmpInst::ICMP_SLE;
                        if ( right->type == Type::floatType )
                                pred = llvm::FCmpInst::FCMP_OLE;

                        this->llvm_val = llvm::CmpInst::Create(llvm::CmpInst::ICmp,pred, left->llvm_val, right->llvm_val, "", irgen->GetBasicBlock());
                }
                else if ( left->type == Type::floatType ){
                        llvm::CmpInst::Predicate pred = llvm::FCmpInst::FCMP_OLE;
                        if ( right->type != left->type )
                                pred = llvm::ICmpInst::ICMP_SLE;

                        this->llvm_val = llvm::CmpInst::Create(llvm::CmpInst::FCmp,pred, left->llvm_val, right->llvm_val, "", irgen->GetBasicBlock());
                }
        }
}

void PostfixExpr::Emit(){
	VarExpr* lhs = dynamic_cast<VarExpr*>(left);
	if ( lhs == NULL )
		return;
	lhs->Emit();
	this->type = lhs->type;
	this->llvm_val = lhs->llvm_val;

	Symbol* varsym = symbolTable->findAllScope(lhs->GetIdentifier()->GetName());
	
	if ( op->IsOp("++") ){
		llvm::Value* add_inst;
		if ( left->type == Type::intType )
			add_inst = llvm::BinaryOperator::CreateAdd(left->llvm_val, llvm::ConstantInt::get(irgen->GetIntType(),1), "", irgen->GetBasicBlock());	
		else if ( left->type == Type::boolType)
			add_inst = llvm::BinaryOperator::CreateAdd(left->llvm_val, llvm::ConstantInt::get(irgen->GetBoolType(),(int) true), "", irgen->GetBasicBlock());
		else if ( left->type == Type::floatType )
			add_inst = llvm::BinaryOperator::CreateFAdd(left->llvm_val, llvm::ConstantFP::get(irgen->GetFloatType(),(double)1), "", irgen->GetBasicBlock());

		new llvm::StoreInst(add_inst, varsym->value, true, irgen->GetBasicBlock());
	}
	else if ( op->IsOp("--") ){
                llvm::Value* sub_inst;
                if ( left->type == Type::intType )
                        sub_inst = llvm::BinaryOperator::CreateSub(left->llvm_val, llvm::ConstantInt::get(irgen->GetIntType(),1), "", irgen->GetBasicBlock());
                else if ( left->type == Type::boolType)
                        sub_inst = llvm::BinaryOperator::CreateSub(left->llvm_val, llvm::ConstantInt::get(irgen->GetBoolType(),(int) true), "", irgen->GetBasicBlock());                else if ( left->type == Type::floatType )
                        sub_inst = llvm::BinaryOperator::CreateFSub(left->llvm_val, llvm::ConstantFP::get(irgen->GetFloatType(),(double)1), "", irgen->GetBasicBlock());
                new llvm::StoreInst(sub_inst, varsym->value, true, irgen->GetBasicBlock());
        }
}

Call::Call(yyltype loc, Expr *b, Identifier *f, List<Expr*> *a) : Expr(loc)  {
    Assert(f != NULL && a != NULL); // b can be be NULL (just means no explicit base)
    base = b;
    if (base) base->SetParent(this);
    (field=f)->SetParent(this);
    (actuals=a)->SetParentAll(this);
}

void Call::PrintChildren(int indentLevel) {
   if (base) base->Print(indentLevel+1);
   if (field) field->Print(indentLevel+1);
   if (actuals) actuals->PrintAll(indentLevel+1, "(actuals) ");
}
