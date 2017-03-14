/*
 * Symbol table implementation
 *
 */

/*
 * Symbol table implementation
 *
 */

#include "symtable.h"
#include <iostream>

using namespace std;
/* Scope Table Class */
ScopedTable::ScopedTable() {}
ScopedTable::~ScopedTable() {}
void ScopedTable::insert(Symbol &sym){
	symbols.insert (  pair<const char*, Symbol>(sym.name,sym) );
}

void ScopedTable::remove(Symbol &sym){
	symbols.erase( sym.name );
}

Symbol* ScopedTable::find(const char *name){
	SymbolIterator iter = symbols.find(name);
	Symbol* sym = (iter != symbols.end()) ? &iter->second : NULL;
	return sym;
}

/* SymbolTable */
SymbolTable::SymbolTable() {}
SymbolTable::~SymbolTable() {}
void SymbolTable::pop(){
	tables.pop_back();
}

void SymbolTable::push(){
	tables.push_back(new ScopedTable());
}

void SymbolTable::insert(Symbol &sym){
	//Insert symbol to the current scope
	tables.back()->insert(sym);
}

void SymbolTable::remove(Symbol &sym){
	//Remove symbol from the current scope
	tables.back()->remove(sym);
}

Symbol* SymbolTable::find(const char *name){
	return tables.back()->find(name);
}

Symbol* SymbolTable::findAllScope(const char* name){
	for ( vector<ScopedTable*>::reverse_iterator iter = tables.rbegin(); iter != tables.rend(); ++iter){
		Symbol* foundSym = (*iter)->find(name);
		if ( foundSym != NULL )
			return foundSym;
	}

	return NULL;
}


/* My Stack */
bool MyStack::insideLoop(){
	if ( stmtStack.size() <= 0 )
		return false;

	ForStmt* forStmt = dynamic_cast<ForStmt*>(stmtStack.back());
	WhileStmt* whileStmt = dynamic_cast<WhileStmt*>(stmtStack.back());

	return ( forStmt != NULL || whileStmt != NULL );
}

bool MyStack::insideSwitch(){
	if ( stmtStack.size() <= 0 )
		return false;

	SwitchStmt* switchStmt = dynamic_cast<SwitchStmt*>(stmtStack.back());

	return ( switchStmt != NULL );
}
