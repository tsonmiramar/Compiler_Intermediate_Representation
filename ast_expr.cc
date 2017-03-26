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

void VarExpr::GetType(){
	Symbol* varsym = symbolTable->findAllScope(this->id->GetName());
	VarDecl* varDecl = dynamic_cast<VarDecl*>(varsym->decl);
	ArrayType* arr_type = dynamic_cast<ArrayType*>(varDecl->GetType());
	if ( arr_type != NULL )
        	this->type = arr_type->GetElemType();
        else
        	this->type = varDecl->GetType();
}

void VarExpr::Emit(){
	Symbol* varsym = symbolTable->findAllScope(this->id->GetName());
	VarDecl* varDecl = dynamic_cast<VarDecl*>(varsym->decl);
	ArrayType* arr_type = dynamic_cast<ArrayType*>(varDecl->GetType());	
	
	if ( arr_type != NULL )
		this->type = arr_type->GetElemType();
	else
		this->type = varDecl->GetType();

	this->llvm_val = new llvm::LoadInst(varsym->value,"",irgen->GetBasicBlock());
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

void ArrayAccess::GetType(){
	VarExpr* var_expr = dynamic_cast<VarExpr*>(base);
        Symbol* varsym = symbolTable->findAllScope(var_expr->GetIdentifier()->GetName());        
	VarDecl* varDecl = dynamic_cast<VarDecl*>(varsym->decl);

        ArrayType* arr_type = dynamic_cast<ArrayType*>(varDecl->GetType());
        this->type = arr_type->GetElemType();
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
	this->getElemPtrInst = this->llvm_val;
}
     
FieldAccess::FieldAccess(Expr *b, Identifier *f) 
  : LValue(b? Join(b->GetLocation(), f->GetLocation()) : *f->GetLocation()) {
    Assert(f != NULL); // b can be be NULL (just means no explicit base)
    base = b; 
    if (base) base->SetParent(this); 
    (field=f)->SetParent(this);
}

llvm::Value* FieldAccess::InsertWithUndef(Type* newType, llvm::Value* insertVal){
	int swizzle_len;
	if ( newType == Type::vec2Type )
		swizzle_len =2;
	else if ( newType == Type::vec3Type)
		swizzle_len =3;
	else
		swizzle_len =4;

	llvm::Value* dest_vec = llvm::UndefValue::get(Decl::GetllvmType(newType));
	for ( int i = 0; i < swizzle_len; i++ ){
		dest_vec = llvm::InsertElementInst::Create(dest_vec,insertVal,llvm::ConstantInt::get(irgen->GetIntType(),i),"",irgen->GetBasicBlock());	
	}
	return dest_vec;	
}


void FieldAccess::PrintChildren(int indentLevel) {
    if (base) base->Print(indentLevel+1);
    field->Print(indentLevel+1);
}

llvm::Value* FieldAccess::GetllvmField(){
	string swizzle = string(field->GetName());
        
	if ( swizzle.length() == 1 ){
	        llvm::Value* swizzleIdx;
                if ( swizzle[0] == 'x' )
                        swizzleIdx = llvm::ConstantInt::get(irgen->GetIntType(), 0);
                else if ( swizzle[0] == 'y' )
                        swizzleIdx = llvm::ConstantInt::get(irgen->GetIntType(), 1);
                else if ( swizzle[0] == 'z' )
                        swizzleIdx = llvm::ConstantInt::get(irgen->GetIntType(), 2);
                else
                     swizzleIdx = llvm::ConstantInt::get(irgen->GetIntType(), 3);
		
		this->type = Type::floatType;
                return llvm::ExtractElementInst::Create(base->llvm_val,swizzleIdx,"",irgen->GetBasicBlock());
        }
	else {
		vector<llvm::Constant*> swizzle_vec;
                for ( unsigned int i = 0; i < swizzle.length(); i++ ){
                        if ( swizzle[i] == 'x' )
                                swizzle_vec.push_back(llvm::ConstantInt::get(irgen->GetIntType(), 0));
                        else if ( swizzle[i] == 'y' )
                                swizzle_vec.push_back(llvm::ConstantInt::get(irgen->GetIntType(), 1));
                        else if ( swizzle[i] == 'z' )
                                swizzle_vec.push_back(llvm::ConstantInt::get(irgen->GetIntType(), 2));
                        else if ( swizzle[i] == 'w' )
                                swizzle_vec.push_back(llvm::ConstantInt::get(irgen->GetIntType(), 3));
                }

                llvm::ArrayRef<llvm::Constant*> swizzleArrayRef(swizzle_vec);
                llvm::Constant *mask = llvm::ConstantVector::get(swizzleArrayRef);
		
		if ( swizzle.length() == 2 )
			this->type = Type::vec2Type;
		else if ( swizzle.length() == 3) 
			this->type = Type::vec3Type;
		else
			this->type = Type::vec4Type;

                return new llvm::ShuffleVectorInst( base->llvm_val, base->llvm_val, mask, "", irgen->GetBasicBlock());
	}	
}

llvm::Value* FieldAccess::InsertllvmElems(llvm::Value* insertVal){
	string swizzles = string(field->GetName());
	if ( swizzles.length() == 1){
		llvm::Value* swizzleIdx;
                if ( swizzles[0] == 'x' )
                        swizzleIdx = llvm::ConstantInt::get(irgen->GetIntType(), 0);
                else if ( swizzles[0] == 'y' )
                        swizzleIdx = llvm::ConstantInt::get(irgen->GetIntType(), 1);
                else if ( swizzles[0] == 'z' )
                        swizzleIdx = llvm::ConstantInt::get(irgen->GetIntType(), 2);
                else
                     swizzleIdx = llvm::ConstantInt::get(irgen->GetIntType(), 3);

		return llvm::InsertElementInst::Create(this->llvm_val, insertVal, swizzleIdx,"", irgen->GetBasicBlock());
	}
	else {
		llvm::Constant* swizzleIdx;
		llvm::Value* insert_inst = this->llvm_val;

		for ( unsigned int i = 0; i < swizzles.length(); i++ ){
			swizzleIdx = llvm::ConstantInt::get(irgen->GetIntType(), i);
			llvm::Constant* swizzleTobeInsert;
			if ( swizzles[i] == 'x' )
                        	swizzleTobeInsert = llvm::ConstantInt::get(irgen->GetIntType(), 0);
                	else if ( swizzles[i] == 'y' )
                        	swizzleTobeInsert = llvm::ConstantInt::get(irgen->GetIntType(), 1);
                	else if ( swizzles[i] == 'z' )
                        	swizzleTobeInsert = llvm::ConstantInt::get(irgen->GetIntType(), 2);
                	else
                     		swizzleTobeInsert = llvm::ConstantInt::get(irgen->GetIntType(), 3);

			llvm::Value* extract_inst = llvm::ExtractElementInst::Create(insertVal,swizzleIdx,"",irgen->GetBasicBlock());
			insert_inst = llvm::InsertElementInst::Create(insert_inst, extract_inst,swizzleTobeInsert,"", irgen->GetBasicBlock());
		}
		return insert_inst;
	}
}

void FieldAccess::GetType(){
	VarExpr* var_expr = dynamic_cast<VarExpr*>(base);
	var_expr->GetType();
	this->type = var_expr->type;
}

void FieldAccess::Emit(){
	base->Emit();
	this->llvm_val = base->llvm_val;
	this->type = base->type;
}

/* AssignExpr Emit() */
void AssignExpr::Emit(){
	if ( op->IsOp("=") ){
		this->ops_perform("=");
	}
	else if ( op->IsOp("+=") ){
		this->ops_perform("+");
	}
	else if ( op->IsOp("-=") ){
		this->ops_perform("-");
	}
	else if ( op->IsOp("*=") ){
		this->ops_perform("*");
	}
	else if ( op->IsOp("/=")) {
		this->ops_perform("/");
	}
}

void AssignExpr::ops_perform(const char* opsTok, bool doPrefix){
        yyltype loc;
        ArithmeticExpr* arith = new ArithmeticExpr(left,new Operator(loc,opsTok), right);
        VarExpr* var_expr = dynamic_cast<VarExpr*>(left);
        ArrayAccess* arr_expr = dynamic_cast<ArrayAccess*>(left);
	FieldAccess* field_expr = dynamic_cast<FieldAccess*>(left);

	ArrayAccess* arr_rhs_expr = dynamic_cast<ArrayAccess*>(right);
	FieldAccess* field_rhs_expr = dynamic_cast<FieldAccess*>(right);

	if ( strcmp(opsTok,"=" ) == 0 ){
		right->Emit();
		if ( arr_rhs_expr != NULL ){
			right->llvm_val = new llvm::LoadInst(right->llvm_val,"",irgen->GetBasicBlock());
		}
		else if ( field_rhs_expr != NULL ){
			right->llvm_val = field_rhs_expr->GetllvmField();
			right->type = field_rhs_expr->type;	
		}

		this->llvm_val = right->llvm_val;
		this->type = right->type;
	}
	else {
		arith->Emit();
        	this->llvm_val = arith->llvm_val;
        	this->type = arith->type;
	}

        if ( var_expr != NULL ){
                Symbol* varsym = symbolTable->findAllScope(var_expr->GetIdentifier()->GetName());
                new llvm::StoreInst(this->llvm_val, varsym->value, irgen->GetBasicBlock());
        }
        else if ( arr_expr != NULL ){
                arr_expr->Emit();
                new llvm::StoreInst(this->llvm_val, arr_expr->llvm_val, irgen->GetBasicBlock());
        }
	else if ( field_expr != NULL){
		field_expr->Emit();
	
		Symbol* varsym = symbolTable->findAllScope(dynamic_cast<VarExpr*>(field_expr->GetBase())->GetIdentifier()->GetName());
		llvm::Value* insert_inst = field_expr->InsertllvmElems(this->llvm_val);
                new llvm::StoreInst(insert_inst,varsym->value,true,irgen->GetBasicBlock());
	}
}

void ArithmeticExpr::ops_perform(const char* opsTok, bool doPrefix){
	/* This form only do ++ and -- */
	yyltype loc;
	PostfixExpr* prefix = new PostfixExpr(right,new Operator(loc,opsTok));
	prefix->doPrefix = true;
	prefix->Emit();
	this->llvm_val = prefix->llvm_val;
	this->type = prefix->type;
}

/* ArithmeticExpr Emit() */
void ArithmeticExpr::Emit(){
	if ( op->IsOp("++") ){
		this->ops_perform("++");
		return;
	}
	else if ( op->IsOp("--") ){
		this->ops_perform("--");
		return;
	}

	ArrayAccess* lhs_arr = dynamic_cast<ArrayAccess*>(left);
	FieldAccess* lhs_field = dynamic_cast<FieldAccess*>(left);

	ArrayAccess* rhs_arr = dynamic_cast<ArrayAccess*>(right);
        FieldAccess* rhs_field = dynamic_cast<FieldAccess*>(right);
	
	left->Emit();
	if ( lhs_arr != NULL )
		left->llvm_val = new llvm::LoadInst(left->llvm_val,"",irgen->GetBasicBlock());
	else if ( lhs_field != NULL ){
		left->llvm_val = lhs_field->GetllvmField();
		left->type = lhs_field->type;
	}
		
	right->Emit();
        if ( rhs_arr != NULL )
                right->llvm_val = new llvm::LoadInst(left->llvm_val,"",irgen->GetBasicBlock());
        else if ( rhs_field != NULL ){
                right->llvm_val = rhs_field->GetllvmField();
                right->type = rhs_field->type;
        }

	this->type = left->type;

	if (op->IsOp("+")){
		if ( left->type == Type::intType || left->type == Type::boolType){
			this->llvm_val = llvm::BinaryOperator::CreateAdd(left->llvm_val,right->llvm_val,"",irgen->GetBasicBlock());
		}	
		else {	
			if ( left->type == Type::floatType && right->type != Type::floatType && right->type != Type::intType && right->type != Type::boolType ) {
				left->llvm_val = FieldAccess::InsertWithUndef(right->type,left->llvm_val);
				this->type = left->type = right->type;
						
			}
			else if ( left->type != Type::intType && left->type != Type::boolType && left->type != Type::floatType && right->type == Type::floatType){
 				right->llvm_val = FieldAccess::InsertWithUndef(left->type,right->llvm_val);
				this->type = right->type = left->type;
			}
			
			this->llvm_val = llvm::BinaryOperator::CreateFAdd(left->llvm_val,right->llvm_val,"",irgen->GetBasicBlock());
		}
	}

	else if (op->IsOp("-")){
		if ( left->type == Type::intType || left->type == Type::boolType){
			this->llvm_val = llvm::BinaryOperator::CreateSub(left->llvm_val,right->llvm_val,"",irgen->GetBasicBlock());
		}
		else {
			if ( left->type == Type::floatType && right->type != Type::floatType && right->type != Type::intType && right->type != Type::boolType ) {
                                left->llvm_val = FieldAccess::InsertWithUndef(right->type,left->llvm_val);
                                this->type = left->type = right->type;

                        }
                        else if ( left->type != Type::intType && left->type != Type::boolType && left->type != Type::floatType && right->type == Type::floatType){
                                right->llvm_val = FieldAccess::InsertWithUndef(left->type,right->llvm_val);
                                this->type = right->type = left->type;
                        }
                        this->llvm_val = llvm::BinaryOperator::CreateFSub(left->llvm_val,right->llvm_val,"",irgen->GetBasicBlock());
		}
	}
	else if (op->IsOp("*")){
		if ( left->type == Type::intType || left->type == Type::boolType){
			this->llvm_val = llvm::BinaryOperator::CreateMul(left->llvm_val,right->llvm_val,"",irgen->GetBasicBlock());
		}
		else {
			if ( left->type == Type::floatType && right->type != Type::floatType && right->type != Type::intType && right->type != Type::boolType ) {
                                left->llvm_val = FieldAccess::InsertWithUndef(right->type,left->llvm_val);
                                this->type = left->type = right->type;

                        }
                        else if ( left->type != Type::intType && left->type != Type::boolType && left->type != Type::floatType && right->type == Type::floatType){
                                right->llvm_val = FieldAccess::InsertWithUndef(left->type,right->llvm_val);
                                this->type = right->type = left->type;
                        }

			this->llvm_val = llvm::BinaryOperator::CreateFMul(left->llvm_val,right->llvm_val,"",irgen->GetBasicBlock());
		}
	}
	else if ( op->IsOp("/")){
		if ( left->type == Type::intType || left->type == Type::boolType){
			this->llvm_val = llvm::BinaryOperator::CreateSDiv(left->llvm_val,right->llvm_val,"",irgen->GetBasicBlock());
		}
		else {
			if ( left->type == Type::floatType && right->type != Type::floatType && right->type != Type::intType && right->type != Type::boolType ) {
                                left->llvm_val = FieldAccess::InsertWithUndef(right->type,left->llvm_val);
                                this->type = left->type = right->type;

                        }
                        else if ( left->type != Type::intType && left->type != Type::boolType && left->type != Type::floatType && right->type == Type::floatType){
                                right->llvm_val = FieldAccess::InsertWithUndef(left->type,right->llvm_val);
                                this->type = right->type = left->type;
                        }
	
			this->llvm_val = llvm::BinaryOperator::CreateFDiv(left->llvm_val,right->llvm_val,"",irgen->GetBasicBlock());
		}
	}

}

void EqualityExpr::ops_perform(const char* opsTok, bool doPrefix){
	left->Emit();
        if ( dynamic_cast<ArrayAccess*>(left) != NULL )
                left->llvm_val = new llvm::LoadInst(left->llvm_val,"",irgen->GetBasicBlock());

        right->Emit();
        if ( dynamic_cast<ArrayAccess*>(right) != NULL )
                right->llvm_val = new llvm::LoadInst(right->llvm_val,"",irgen->GetBasicBlock());

        this->type = Type::boolType;

        llvm::CmpInst::Predicate pred;

        if ( strcmp(opsTok,"==") == 0 )
                pred = (right->type == Type::floatType) ? llvm::FCmpInst::FCMP_OEQ : llvm::ICmpInst::ICMP_EQ;
        else /* strcmp(opsTok,"!=") == 0 */
		pred = (right->type == Type::floatType) ? llvm::FCmpInst::FCMP_ONE : llvm::ICmpInst::ICMP_NE;

        if ( left->type == Type::floatType ){
                this->llvm_val = llvm::CmpInst::Create(llvm::CmpInst::FCmp,pred, left->llvm_val, right->llvm_val, "", irgen->GetBasicBlock());
        }
        else {
                this->llvm_val = llvm::CmpInst::Create(llvm::CmpInst::ICmp,pred, left->llvm_val, right->llvm_val, "", irgen->GetBasicBlock());
        }	
}

/* Equality Expr Emit() */
void EqualityExpr::Emit() {
	if ( op->IsOp("==") ){
		this->ops_perform("==");
	}
	else if ( op->IsOp("!=") ){
		this->ops_perform("!=");
	}		
}

void LogicalExpr::ops_perform(const char* opsTok, bool doPrefix){
	left->Emit();
        if ( dynamic_cast<ArrayAccess*>(left) != NULL )
                left->llvm_val = new llvm::LoadInst(left->llvm_val,"",irgen->GetBasicBlock());

        right->Emit();
        if ( dynamic_cast<ArrayAccess*>(right) != NULL )
                right->llvm_val = new llvm::LoadInst(right->llvm_val,"",irgen->GetBasicBlock());

        this->type = Type::boolType;

        if ( strcmp(opsTok,"&&") == 0){
                this->llvm_val = llvm::BinaryOperator::CreateAnd(left->llvm_val, right->llvm_val, "", irgen->GetBasicBlock());
        }
        else /* ( opsTok == "||" )*/ {
                this->llvm_val = llvm::BinaryOperator::CreateOr(left->llvm_val, right->llvm_val, "", irgen->GetBasicBlock());
        }
}

void LogicalExpr::Emit(){
	if ( op->IsOp("&&") ){
		this->ops_perform("&&");
	}
	else {
		this->ops_perform("||");
	}
}

void RelationalExpr::ops_perform(const char* opsTok,bool doPrefix){
	left->Emit();
        if ( dynamic_cast<ArrayAccess*>(left) != NULL )
                left->llvm_val = new llvm::LoadInst(left->llvm_val,"",irgen->GetBasicBlock());

        right->Emit();
        if ( dynamic_cast<ArrayAccess*>(right) != NULL )
                right->llvm_val = new llvm::LoadInst(right->llvm_val,"",irgen->GetBasicBlock());

	this->type = Type::boolType;
	
	llvm::CmpInst::Predicate pred;

	if ( strcmp(opsTok,">") == 0 )
		pred = (right->type == Type::floatType) ? llvm::FCmpInst::FCMP_OGT : llvm::ICmpInst::ICMP_SGT;
	else if ( strcmp(opsTok,"<") == 0 )
		pred = (right->type == Type::floatType) ? llvm::FCmpInst::FCMP_OLT : llvm::ICmpInst::ICMP_SLT;
	else if ( strcmp(opsTok,">=") == 0 )
                pred = (right->type == Type::floatType) ? llvm::FCmpInst::FCMP_OGE : llvm::ICmpInst::ICMP_SGE;
	else /* <= */
                pred = (right->type == Type::floatType) ? llvm::FCmpInst::FCMP_OLE : llvm::ICmpInst::ICMP_SLE;

	if ( left->type == Type::floatType ){
		this->llvm_val = llvm::CmpInst::Create(llvm::CmpInst::FCmp,pred, left->llvm_val, right->llvm_val, "", irgen->GetBasicBlock());
        }
	else {
		this->llvm_val = llvm::CmpInst::Create(llvm::CmpInst::ICmp,pred, left->llvm_val, right->llvm_val, "", irgen->GetBasicBlock());
	}		
	
}

void RelationalExpr::Emit(){
	if ( op->IsOp(">") ){
        	this->ops_perform(">");
	}
	else if ( op->IsOp("<") ){
        	this->ops_perform("<");
	}
	else if ( op->IsOp(">=") ){
        	this->ops_perform(">=");
	}
	else if ( op->IsOp("<=") ){
        	this->ops_perform("<=");
	}
}

void PostfixExpr::ops_perform(const char* opsTok, bool doPrefix){
	VarExpr* var_expr = dynamic_cast<VarExpr*>(left);
        ArrayAccess* arr_expr = dynamic_cast<ArrayAccess*>(left);
	
	if ( var_expr != NULL ){
		var_expr->GetType();
	}
	else if ( arr_expr != NULL){
		arr_expr->GetType();
	}

	yyltype loc;
	Expr* rhs;
	if ( left->type == Type::intType )
		rhs = new IntConstant(loc,1);
	else if ( left->type == Type::floatType )
		rhs = new FloatConstant(loc,(double) 1);
	else /*( left->type == Type::boolType)*/
		rhs = new BoolConstant(loc, (int) true);

	ArithmeticExpr* arith;
	if ( strcmp(opsTok,"++") == 0 )
		arith = new ArithmeticExpr(left,new Operator(loc,"+"),rhs);
	else /* opsTok = "--" */
		arith = new ArithmeticExpr(left,new Operator(loc,"-"),rhs);

	arith->Emit();
	
	if ( !doPrefix ){
		this->llvm_val = left->llvm_val;
		this->type = left->type;	
	}
	else{
		this->llvm_val = arith->llvm_val;
                this->type = arith->type;
	}

	if ( var_expr != NULL ){
		Symbol* varsym = symbolTable->findAllScope(var_expr->GetIdentifier()->GetName());
		new llvm::StoreInst(arith->llvm_val,varsym->value,true,irgen->GetBasicBlock());
	}
	else if ( arr_expr != NULL ){
		new llvm::StoreInst(arith->llvm_val,arr_expr->getElemPtrInst,true,irgen->GetBasicBlock());
		
	}
}

void PostfixExpr::Emit(){
	if ( op->IsOp("++") ){
		this->ops_perform("++",doPrefix);				
	}
	else if ( op->IsOp("--") ){
        	this->ops_perform("--",doPrefix);
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

void Call::Emit(){
	Symbol* fnsym = symbolTable->findAllScope(field->GetName());		
	vector<llvm::Value*> args;
	for ( int i = 0; i < actuals->NumElements(); i++ ){
		Expr* expr = actuals->Nth(i);
		expr->Emit();
		args.push_back(expr->llvm_val);
	}

	llvm::ArrayRef<llvm::Value*> argsRef = llvm::ArrayRef<llvm::Value*>(args); 
	
	this->llvm_val = llvm::CallInst::Create(fnsym->value,argsRef,"",irgen->GetBasicBlock());
}
