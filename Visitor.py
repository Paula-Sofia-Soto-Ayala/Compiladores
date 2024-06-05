from typing import Any, List
from Scanner import Symbol
from AstNodes import *

SymbolTable = dict[str, List[Symbol] | set[Symbol]]

class TreeVisitor:
    """
    Symbol table has shape:
    symbol_table: dict[str, List[Symbol] | set[Symbol]] = {
        'identifier': set([]),
        'real': [],
        'integer': [],
        'string': []
    }
    """
    symbols: dict[str, List[Symbol] | set[Symbol]]


    def __init__(self, symbols: dict[str, List[Symbol] | set[Symbol]]) -> None:
        self.symbols = symbols

    def visit(self, node: AstNode) -> Any:
        match node:
            case SourceNode(): return self.visit_source(node)
            case ProgramNode(): return self.visit_program(node)
            case IdentNode(): return self.visit_ident(node)
            case VariablesNode(): return self.visit_variables(node)
            case SubprogNode(): return self.visit_subprogram(node)
            case VariablesNode(): return self.visit_variables(node)
            case VarNode(): return self.visit_variable(node)
            case _:
                print(f"Unhandled node of type: {node.id}")

    def visit_program(self, node: ProgramNode):
        print(f"Program: {node.name.val}")
        identifiers: set[Symbol] = self.symbols["identifier"]

        # Update the program symbol
        for symbol in identifiers:
            if symbol.name == node.name.val:
                symbol.sub_type = "program"

    def visit_ident(self, node: IdentNode):
        self.visit(node.value)

    def visit_source(self, node: SourceNode):
        self.visit(node.prog)
        self.visit(node.vars)

        for subprogram in node.subprogs:
            self.visit(subprogram)

        self.visit(node.code)

    def visit_subprogram(self, node: SubprogNode):
        identifiers: set[Symbol] = self.symbols["identifier"]
        print(f"Subprogram: {node.name}")

        for symbol in identifiers:
            if symbol.name == node.name.val:
                ret_type = node.ret_type
                symbol.ret_type = ret_type.value.val if ret_type else None
                symbol.sub_type = "function" if ret_type else "procedure"

        self.visit(node.vars)

        for param in node.params:
            self.visit(param)

    def visit_variable(self, node: VarNode):
        identifiers: set[Symbol] = self.symbols["identifier"]

        for symbol in identifiers:
            if symbol.name != node.name.val:
                continue
            
            if isinstance(node.type, ArrayTypeNode):
                array_type = node.type
                subtype = array_type.subtype.value
                type = f"{subtype.val}[{array_type.from_num}..{array_type.to_num}]"
                symbol.var_type = type
            else:
                symbol.var_type = node.type.value.val

        # print(f"{node.token.val}")

    def visit_variables(self, node: VariablesNode):
        print(f"Variable Declarations")

        for var in node.vars:
            print(var)
            self.visit(var)

    def visit_assignment(self, node: AssignStmtNode):
        print(node)
        self.visit(node.value)