from typing import Any, List
from Scanner import Symbol
from AstNodes import *

# Define a symbol table type
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

        """
        TreeVisitor class for traversing an abstract syntax tree (AST).
        """

        self.symbols = symbols

    def visit(self, node: AstNode) -> Any:

        """
        Dispatch method for different node types.

        Args:
            node (AstNode): The AST node to visit.
        """

        # Use pattern matching to handle different node types
        match node:
            case SourceNode(): return self.visit_source(node)
            case ProgramNode(): return self.visit_program(node)
            case IdentNode(): return self.visit_ident(node)
            case VariablesNode(): return self.visit_variables(node)
            case SubprogNode(): return self.visit_subprogram(node)
            case VariablesNode(): return self.visit_variables(node)
            case VarNode(): return self.visit_variable(node)
            case _:
                # Handle any unhandled node type
                print(f"Unhandled node of type: {node.id}")

    def visit_program(self, node: ProgramNode):

        """
        Processes the program node and updates the program symbol in the symbol table.

        Args:
            node (ProgramNode): The program node.
        """

        print(f"Program: {node.name.val}")
        identifiers: set[Symbol] = self.symbols["identifier"]

        # Update the program symbol
        for symbol in identifiers:
            if symbol.name == node.name.val:
                symbol.sub_type = "program"

    def visit_ident(self, node: IdentNode):

        """
        Visits an identifier node.

        Args:
            node (IdentNode): The identifier node.
        """

        self.visit(node.value)

    def visit_source(self, node: SourceNode):

        """
        Visits the entire source node, including program, variables, subprograms, and code.

        Args:
            node (SourceNode): The source node.
        """

        # Visit the program node
        self.visit(node.prog)

        # Visit the variables node
        self.visit(node.vars)

        # Visit each subprogram in the subprogs list
        for subprogram in node.subprogs:
            self.visit(subprogram)

        # Visit the code node
        self.visit(node.code)
        
        
    def visit_subprogram(self, node: SubprogNode):

        """
        Processes subprograms (functions or procedures) and updates their symbols.

        Args:
            node (SubprogNode): The subprogram node.
        """

        # Get the set of symbols associated with identifiers
        identifiers: set[Symbol] = self.symbols["identifier"]
        print(f"Subprogram: {node.name}")

        # Iterate through each symbol
        for symbol in identifiers:
            # Check if the symbol name matches the subprogram name
            if symbol.name == node.name.val:
                # Update the symbol's return type based on the subprogram's return type
                ret_type = node.ret_type
                symbol.ret_type = ret_type.value.val if ret_type else None
                # Set the subprogram type (function or procedure)
                symbol.sub_type = "function" if ret_type else "procedure"

        # Visit the subprogram's variables
        self.visit(node.vars)

        # Visit each parameter in the subprogram
        for param in node.params:
            self.visit(param)


    def visit_variable(self, node: VarNode):

        """
        Handles variable declarations, including array types.

        Args:
            node (VarNode): The variable node.
        """

        # Get the set of symbols associated with identifiers
        identifiers: set[Symbol] = self.symbols["identifier"]

        # Iterate through each symbol
        for symbol in identifiers:
            # Check if the symbol name matches the variable name
            if symbol.name != node.name.val:
                continue
            # If the variable type is an array
            if isinstance(node.type, ArrayTypeNode):
                array_type = node.type
                subtype = array_type.subtype.value
                # Construct the array type string
                type = f"{subtype.val}[{array_type.from_num}..{array_type.to_num}]"
                symbol.var_type = type
            else:
                # Set the symbol's variable type to the non-array type
                symbol.var_type = node.type.value.val

        # print(f"{node.token.val}")

    def visit_variables(self, node: VariablesNode):

        """
        Processes variable declarations.

        Args:
            node (VariablesNode): The variables node.
        """

        print(f"Variable Declarations")

        for var in node.vars:
            print(var)
            self.visit(var)

    def visit_assignment(self, node: AssignStmtNode):

        """
        Placeholder for handling assignment statements (not fully implemented).

        Args:
            node (AssignStmtNode): The assignment statement node.
        """

        print(node)
        self.visit(node.value)