from typing import Any, List
from Scanner import Symbol
from AstNodes import *

SymbolTable = dict[str, List[Symbol] | set[Symbol]]

class TreeVisitor:

    def visit(self, node: AstNode, **ctx) -> Any:
        match node:
            case SourceNode(): return self.visit_source(node, **ctx)
            case ProgramNode(): return self.visit_program(node, **ctx)
            case IdentNode(): return self.visit_ident(node, **ctx)
            case VariablesNode(): return self.visit_var_node(node, **ctx)
            case _:
                print(f"Unhandled node: {node}")

    def visit_program(self, node: ProgramNode, **ctx) -> Any:
        print(node.name)

    def visit_ident(self, node: IdentNode, **ctx) -> Any:
        self.visit(node.value, **ctx)

    def visit_source(self, node: SourceNode, **ctx) -> Any:
        self.visit(node.prog, **ctx)
        self.visit(node.vars, **ctx)

        for subprogram in node.subprogs:
            self.visit(subprogram, **ctx)

        self.visit(node.code, **ctx)

    # TODO: Update symbol table
    def visit_var_node(self, node: VariablesNode, **ctx) -> Any:
        print(node.start)
        symbols: SymbolTable = ctx

        for var in node.vars:
            self.visit(var, **ctx)
            print(var.name.val)
