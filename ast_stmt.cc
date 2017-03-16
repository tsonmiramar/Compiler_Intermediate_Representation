/* File: ast_stmt.cc
 * -----------------
 * Implementation of statement node classes.
 */
#include "ast_stmt.h"
#include "ast_type.h"
#include "ast_decl.h"
#include "ast_expr.h"
#include "symtable.h"

#include "irgen.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Support/raw_ostream.h"                                                   


Program::Program(List<Decl*> *d) {
    Assert(d != NULL);
    (decls=d)->SetParentAll(this);
}

void Program::PrintChildren(int indentLevel) {
    decls->PrintAll(indentLevel+1);
    printf("\n");
}

void Program::Emit() {
    // TODO:
    // This is just a reference for you to get started
    //
    // You can use this as a template and create Emit() function
    // for individual node to fill in the module structure and instructions.
    //
    llvm::Module *mod = irgen->GetOrCreateModule("Program_Module.bc");

    symbolTable->push();
     for ( int i = 0; i < decls->NumElements(); ++i ) {
        Decl *d = decls->Nth(i);

        d->Emit();
    }
   
    symbolTable->pop();

    // write the BC into standard output
    llvm::WriteBitcodeToFile(mod, llvm::outs());
    
    //uncomment the next line to generate the human readable/assembly file
    mod->dump();	
    
    /* create a function signature
    std::vector<llvm::Type *> argTypes;
    llvm::Type *intTy = irgen.GetIntType();
    argTypes.push_back(intTy);
    llvm::ArrayRef<llvm::Type *> argArray(argTypes);
    llvm::FunctionType *funcTy = llvm::FunctionType::get(intTy, argArray, false);

    // llvm::Function *f = llvm::cast<llvm::Function>(mod->getOrInsertFunction("foo", intTy, intTy, (Type *)0));
    llvm::Function *f = llvm::cast<llvm::Function>(mod->getOrInsertFunction("Name_the_function", funcTy));
    llvm::Argument *arg = f->arg_begin();
    arg->setName("x");

    // insert a block into the runction
    llvm::LLVMContext *context = irgen.GetContext();
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(*context, "entry", f);

    // create a return instruction
    llvm::Value *val = llvm::ConstantInt::get(intTy, 1);
    llvm::Value *sum = llvm::BinaryOperator::CreateAdd(arg, val, "", bb);
    llvm::ReturnInst::Create(*context, sum, bb);

    // write the BC into standard output
    llvm::WriteBitcodeToFile(mod, llvm::outs());

    //uncomment the next line to generate the human readable/assembly file
    //mod->dump();
    */
}

StmtBlock::StmtBlock(List<VarDecl*> *d, List<Stmt*> *s) {
    Assert(d != NULL && s != NULL);
    (decls=d)->SetParentAll(this);
    (stmts=s)->SetParentAll(this);
}

void StmtBlock::PrintChildren(int indentLevel) {
    decls->PrintAll(indentLevel+1);
    stmts->PrintAll(indentLevel+1);
}

/* StmtBlock Emit() */
void StmtBlock::Emit(){
	for ( int i = 0; i < stmts->NumElements() ; i++){
		Stmt* stmt = stmts->Nth(i);
                StmtBlock* stmtBlk = dynamic_cast<StmtBlock*>(stmt);
		if ( stmtBlk != NULL ){
                        symbolTable->push();
                }

                stmt->Emit();

                if ( stmtBlk != NULL){
                        symbolTable->pop();
                }
	}
}

DeclStmt::DeclStmt(Decl *d) {
    Assert(d != NULL);
    (decl=d)->SetParent(this);
}

void DeclStmt::PrintChildren(int indentLevel) {
    decl->Print(indentLevel+1);
}

void DeclStmt::Emit(){
	decl->Emit();
}

ConditionalStmt::ConditionalStmt(Expr *t, Stmt *b) { 
    Assert(t != NULL && b != NULL);
    (test=t)->SetParent(this); 
    (body=b)->SetParent(this);
}

ForStmt::ForStmt(Expr *i, Expr *t, Expr *s, Stmt *b): LoopStmt(t, b) { 
    Assert(i != NULL && t != NULL && b != NULL);
    (init=i)->SetParent(this);
    step = s;
    if ( s )
      (step=s)->SetParent(this);
}

void ForStmt::PrintChildren(int indentLevel) {
    init->Print(indentLevel+1, "(init) ");
    test->Print(indentLevel+1, "(test) ");
    if ( step )
      step->Print(indentLevel+1, "(step) ");
    body->Print(indentLevel+1, "(body) ");
}

void ForStmt::Emit(){
	symbolTable->push();
	llvm::BasicBlock *fb = llvm::BasicBlock::Create(*irgen->GetContext(), "footerBB", irgen->GetFunction());
	llvm::BasicBlock *sb;
	
	if ( step != NULL)
		sb = llvm::BasicBlock::Create(*irgen->GetContext(), "stepBB", irgen->GetFunction());

        llvm::BasicBlock *bb = llvm::BasicBlock::Create(*irgen->GetContext(), "bodyBB", irgen->GetFunction());
        llvm::BasicBlock *hb = llvm::BasicBlock::Create(*irgen->GetContext(), "headerBB", irgen->GetFunction());

	this->hb = hb;
	this->fb = fb;
	if ( step != NULL)
		this->sb = sb;

	hb->moveAfter(irgen->GetBasicBlock());
	bb->moveAfter(hb);
	if ( step != NULL ){
		sb->moveAfter(bb);
		fb->moveAfter(sb);
	}
	else{
		fb->moveAfter(bb);
	}

	loop_switchStack->push(this);

	init->Emit();
	llvm::BranchInst::Create(hb,irgen->GetBasicBlock());

	irgen->SetBasicBlock(hb);

	test->Emit();
	
	llvm::BranchInst::Create(bb,fb,test->llvm_val,irgen->GetBasicBlock());

	irgen->SetBasicBlock(bb);
	
	body->Emit();

	if ( !irgen->GetBasicBlock()->getTerminator() ){
		if ( step != NULL ){
			llvm::BranchInst::Create(sb,irgen->GetBasicBlock());
		}
		else
			llvm::BranchInst::Create(hb,irgen->GetBasicBlock());

		if ( step != NULL ){
			irgen->SetBasicBlock(sb);
			step->Emit();
			
			llvm::BranchInst::Create(hb, irgen->GetBasicBlock());
		}
	}
	else {
		if ( step != NULL ){
                        irgen->SetBasicBlock(sb);
                        step->Emit();

                        llvm::BranchInst::Create(hb, irgen->GetBasicBlock());
                }			
	}

	irgen->SetBasicBlock(fb);

	loop_switchStack->pop();
	symbolTable->pop();
	
}

void WhileStmt::Emit(){
	symbolTable->push();
	llvm::BasicBlock *fb = llvm::BasicBlock::Create(*irgen->GetContext(), "footerBB", irgen->GetFunction());
	llvm::BasicBlock *bb = llvm::BasicBlock::Create(*irgen->GetContext(), "bodyBB", irgen->GetFunction());
	llvm::BasicBlock *hb = llvm::BasicBlock::Create(*irgen->GetContext(), "headerBB", irgen->GetFunction());

	this->hb = hb;
	this->fb = fb;

	loop_switchStack->push(this);
	llvm::BranchInst::Create(hb, irgen->GetBasicBlock());

	hb->moveAfter(irgen->GetBasicBlock());
	bb->moveAfter(hb);
	fb->moveAfter(bb);

	irgen->SetBasicBlock(hb);

	test->Emit();

	llvm::BranchInst::Create(bb,fb,test->llvm_val,irgen->GetBasicBlock());

	irgen->SetBasicBlock(bb);
	body->Emit();
	if ( !irgen->GetBasicBlock()->getTerminator() )
		llvm::BranchInst::Create(hb, irgen->GetBasicBlock());

	irgen->SetBasicBlock(fb);

	loop_switchStack->pop();
	symbolTable->pop();	

}

void WhileStmt::PrintChildren(int indentLevel) {
    test->Print(indentLevel+1, "(test) ");
    body->Print(indentLevel+1, "(body) ");
}

void BreakStmt::Emit(){
	LoopStmt* loop_stmt = dynamic_cast<LoopStmt*>(loop_switchStack->top());
	
	llvm::BranchInst::Create(loop_stmt->fb,irgen->GetBasicBlock());
}

void ContinueStmt::Emit(){
	LoopStmt* loop_stmt = dynamic_cast<LoopStmt*>(loop_switchStack->top());
	ForStmt* for_stmt = dynamic_cast<ForStmt*>(loop_switchStack->top());
	if ( for_stmt != NULL ){
		if ( for_stmt->sb != NULL )
	        	llvm::BranchInst::Create(for_stmt->sb,irgen->GetBasicBlock());
		else
			llvm::BranchInst::Create(for_stmt->hb,irgen->GetBasicBlock());
	}
	else{
		llvm::BranchInst::Create(loop_stmt->hb,irgen->GetBasicBlock());
	}
}

IfStmt::IfStmt(Expr *t, Stmt *tb, Stmt *eb): ConditionalStmt(t, tb) { 
    Assert(t != NULL && tb != NULL); // else can be NULL
    elseBody = eb;
    if (elseBody) elseBody->SetParent(this);
}

void IfStmt::PrintChildren(int indentLevel) {
    if (test) test->Print(indentLevel+1, "(test) ");
    if (body) body->Print(indentLevel+1, "(then) ");
    if (elseBody) elseBody->Print(indentLevel+1, "(else) ");
}

void IfStmt::Emit(){
	
	llvm::BasicBlock *eb;
	test->Emit();
	
	llvm::BasicBlock* fb = llvm::BasicBlock::Create(*irgen->GetContext(), "footerBB", irgen->GetFunction());
	
        if (elseBody != NULL)
                eb = llvm::BasicBlock::Create(*irgen->GetContext(), "ElseBB", irgen->GetFunction());

	llvm::BasicBlock* tb = llvm::BasicBlock::Create(*irgen->GetContext(), "ThenBB", irgen->GetFunction());
	
	tb->moveAfter(irgen->GetBasicBlock());
	if ( elseBody != NULL ){
		eb->moveAfter(tb);
		fb->moveAfter(eb);
	}
	else{
		fb->moveAfter(tb);
	}

	
	if ( elseBody !=NULL ){
                llvm::BranchInst::Create(tb,eb,test->llvm_val,irgen->GetBasicBlock());
        }
        else{
                llvm::BranchInst::Create(tb,fb,test->llvm_val,irgen->GetBasicBlock());
	}


	irgen->SetBasicBlock(tb);
	body->Emit();

	if ( !irgen->GetBasicBlock()->getTerminator()){
		llvm::BranchInst::Create(fb,irgen->GetBasicBlock());
	}
	
	if ( elseBody != NULL ){
		irgen->SetBasicBlock(eb);
		elseBody->Emit();

		if ( !irgen->GetBasicBlock()->getTerminator() ){
			llvm::BranchInst::Create(fb,irgen->GetBasicBlock());
		}
	}

	irgen->SetBasicBlock(fb);
}


ReturnStmt::ReturnStmt(yyltype loc, Expr *e) : Stmt(loc) { 
    expr = e;
    if (e != NULL) expr->SetParent(this);
}

void ReturnStmt::PrintChildren(int indentLevel) {
    if ( expr ) 
      expr->Print(indentLevel+1);
}

void ReturnStmt::Emit(){
	if (expr != NULL ){
		expr->Emit();
		llvm::ReturnInst::Create(*irgen->GetContext(), expr->llvm_val, irgen->GetBasicBlock());
	}
	else{
		llvm::ReturnInst::Create(*irgen->GetContext(), irgen->GetBasicBlock());
	}
}

SwitchLabel::SwitchLabel(Expr *l, Stmt *s) {
    Assert(l != NULL && s != NULL);
    (label=l)->SetParent(this);
    (stmt=s)->SetParent(this);
}

SwitchLabel::SwitchLabel(Stmt *s) {
    Assert(s != NULL);
    label = NULL;
    (stmt=s)->SetParent(this);
}

void SwitchLabel::PrintChildren(int indentLevel) {
    if (label) label->Print(indentLevel+1);
    if (stmt)  stmt->Print(indentLevel+1);
}

SwitchStmt::SwitchStmt(Expr *e, List<Stmt *> *c, Default *d) {
    Assert(e != NULL && c != NULL && c->NumElements() != 0 );
    (expr=e)->SetParent(this);
    (cases=c)->SetParentAll(this);
    def = d;
    if (def) def->SetParent(this);
}

void SwitchStmt::PrintChildren(int indentLevel) {
    if (expr) expr->Print(indentLevel+1);
    if (cases) cases->PrintAll(indentLevel+1);
    if (def) def->Print(indentLevel+1);
}

void SwitchStmt::Emit(){

	llvm::BasicBlock *switchExitBB = llvm::BasicBlock::Create(*context, "switchExit", irgen->GetFunction());
	expr->Emit();
}

