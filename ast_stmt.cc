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
    //mod->dump();	
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
	SwitchStmt* switch_stmt = dynamic_cast<SwitchStmt*>(loop_switchStack->top());
	if ( loop_stmt != NULL )
		llvm::BranchInst::Create(loop_stmt->fb,irgen->GetBasicBlock());
	else
		llvm::BranchInst::Create(switch_stmt->fb,irgen->GetBasicBlock());
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
	ArrayAccess* arr_expr = dynamic_cast<ArrayAccess*>(expr);
	if (expr != NULL ){
		expr->Emit();
		if ( arr_expr != NULL ){
			expr->llvm_val = new llvm::LoadInst(expr->llvm_val,"",irgen->GetBasicBlock());
		}

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
	symbolTable->push();

	llvm::BasicBlock *switchExitBB = llvm::BasicBlock::Create(*irgen->GetContext(), "switchExit", irgen->GetFunction());

	this->fb = switchExitBB;

	loop_switchStack->push(this);
	
	expr->Emit();

	llvm::SwitchInst* switch_inst = llvm::SwitchInst::Create(expr->llvm_val, switchExitBB, cases->NumElements(),irgen->GetBasicBlock());
	
	vector<llvm::BasicBlock*> caseBBList;
	for ( int i = 0; i < cases->NumElements(); i++ ){
		
		Case* case_expr = dynamic_cast<Case*>(cases->Nth(i));
		SwitchLabel* switch_label = dynamic_cast<SwitchLabel*>(cases->Nth(i));
		if ( switch_label == NULL ){
			continue;
		}

		llvm::BasicBlock* caseBB;
		if ( case_expr != NULL)
			caseBB = llvm::BasicBlock::Create(*irgen->GetContext(), "switchCase" + to_string(i), irgen->GetFunction());
		else{
			caseBB = llvm::BasicBlock::Create(*irgen->GetContext(), "switchDef", irgen->GetFunction());
			switch_inst->setDefaultDest(caseBB);
		}

		if ( i == 0 )
			caseBB->moveAfter(irgen->GetBasicBlock());

		caseBBList.push_back(caseBB);

		if ( case_expr != NULL ){
			case_expr->GetLabel()->Emit();
			llvm::ConstantInt *constLabelVal = llvm::cast<llvm::ConstantInt>(case_expr->GetLabel()->llvm_val);	
			switch_inst->llvm::SwitchInst::addCase(constLabelVal, caseBB);
		}

		irgen->SetBasicBlock(caseBB);
		switch_label->GetStmt()->Emit();
	}

	for ( unsigned int i = 0; i < caseBBList.size(); i++ ){
		if ( !caseBBList[i]->getTerminator() ){
			if ( i == caseBBList.size() -1 )
				llvm::BranchInst::Create(switchExitBB,caseBBList[i]);
			else
				llvm::BranchInst::Create(caseBBList[i+1],caseBBList[i]);
		}
	}

	switchExitBB->moveAfter(caseBBList.back());
	
	irgen->SetBasicBlock(switchExitBB);

	loop_switchStack->pop();
	symbolTable->pop();
}

