from __future__ import annotations
from typing import Protocol

class Token:
    def __init__(self, val: str, type: str, id: int | None = None) -> None:

        """
        Initializes a Token object.

        Args:
            val (str): The value of the token.
            type (str): The type of the token.
            id (int | None, optional): An optional identifier. Defaults to None.
        """
        
        self.val = val
        self.type = type
        self.id = id

    def __str__(self) -> str:

        """
        Returns a string representation of the Token.

        Returns:
            str: A string representation of the Token.
        """

        return str((self.val, self.type))
        
class AstNode(Protocol):
    id: str

    def __str__(self) -> str:
        
        """
        Returns a string representation of the AstNode.

        Returns:
            str: A string representation of the AstNode.
        """

        return self.id

class IdentNode(AstNode):
    id: str = 'ident'

    def __init__(self, value: Token):

        """
        Initializes an IdentNode.

        Args:
            value (Token): The token representing the identifier.
        """
        
        self.value = value
    
    def __str__(self) -> str:

        """
        Returns a string representation of the IdentNode.

        Returns:
            str: A string representation of the IdentNode.
        """

        return f'(identifier {self.value.val!r})'

class SourceNode(AstNode):
    id = 'source'

    def __init__(self, prog: 'ProgramNode', vars: 'VariablesNode', subprogs: list['SubprogNode'], code: 'StmtNode'):
        
        """
        Initializes a SourceNode.

        Args:
            prog (ProgramNode): The program node.
            vars (VariablesNode): The variables node.
            subprogs (list[SubprogNode]): List of subprogram nodes.
            code (StmtNode): The code node.
        """
        
        self.prog = prog
        self.vars = vars
        self.subprogs = subprogs
        self.code = code
    
    def __str__(self) -> str:

        """
        Returns a string representation of the SourceNode.

        Returns:
            str: A string representation of the SourceNode.
        """
        
        return f'(source {self.prog} {self.vars} {" ".join(map(str, self.subprogs))} {self.code})'

class ProgramNode(AstNode):
    id = 'program'

    def __init__(self, name: Token):

        """
        Initializes a ProgramNode.

        Args:
            name (Token): The token representing the program name.
        """

        self.name = name
    
    def __str__(self) -> str:

        """
        Returns a string representation of the ProgramNode.

        Returns:
            str: A string representation of the ProgramNode.
        """

        return f'(program {self.name.val})'

class VariablesNode(AstNode):
    id = 'variables'

    def __init__(self, start: Token = None, vars: list['VarNode'] = None):

        """
        Initializes a VariablesNode.

        Args:
            start (Token, optional): The starting token. Defaults to None.
            vars (list[VarNode], optional): List of variable nodes. Defaults to an empty list.
        """

        self.start = start
        self.vars = vars or []
    
    def __str__(self) -> str:

        """
        Returns a string representation of the VariablesNode.

        Returns:
            str: A string representation of the VariablesNode.
        """

        return f'(vars {", ".join(map(str, self.vars))})'

class SubprogNode(AstNode):
    id = 'subprog'

    def __init__(self, type: Token, name: Token, params: list['VarNode'], ret_type: 'TypeNode' | None, vars: VariablesNode = None, code: 'StmtNode' = None):
        
        """
        Initializes a SubprogNode.

        Args:
            type (Token): The type of the subprogram (e.g., "function" or "procedure").
            name (Token): The name of the subprogram.
            params (list[VarNode]): List of parameters (variables) for the subprogram.
            ret_type ('TypeNode' | None): The return type (for functions) or None (for procedures).
            vars (VariablesNode, optional): Optional variables defined within the subprogram. Defaults to None.
            code (StmtNode, optional): The body of the subprogram (statements). Defaults to None.
        """
        
        self.type = type
        self.name = name
        self.params = params
        self.ret_type = ret_type
        self.vars = vars
        self.code = code
        self.arity = len(params)
    
class FunctionNode(SubprogNode):
    id = 'function'

    def __str__(self) -> str:

        """
        Returns a string representation of the FunctionNode.

        Returns:
            str: A string representation of the FunctionNode.
        """

        return f'(function {self.name.val} {", ".join(map(str, self.params))} {self.ret_type} {self.vars} {self.code})'
    
class ProcedureNode(SubprogNode):
    id = 'procedure'
    
    def __str__(self) -> str:

        """
        Returns a string representation of the ProcedureNode.

        Returns:
            str: A string representation of the ProcedureNode.
        """

        return f'(procedure {self.name.val} {", ".join(map(str, self.params))} {self.vars} {self.code})'

class VarNode(AstNode):
    id = 'var'

    def __init__(self, name: Token, type: 'TypeNode'):
        
        """
        Initializes a VarNode.

        Args:
            name (Token): The token representing the variable name.
            type ('TypeNode'): The type of the variable.
        """
        
        self.name = name
        self.type = type
    
    def __str__(self) -> str:

        """
        Returns a string representation of the VarNode.

        Returns:
            str: A string representation of the VarNode.
        """

        return f'(var {self.name.val}: {self.type})'

class StmtNode(AstNode):
    pass

class CallStmtNode(StmtNode):
    id = 'call'

    def __init__(self, ident: IdentNode, args: list['ExprNode']):
        self.ident = ident
        self.args = args
    
    def __str__(self) -> str:
        return f'(call {self.ident} {", ".join(map(str, self.args))})'

class AssignStmtNode(StmtNode):
    id = 'assign'

    def __init__(self, ident: IdentNode, value: 'ExprNode'):
        
        """
        Initializes a CallStmtNode.

        Args:
            ident (IdentNode): The identifier (function or procedure name) being called.
            args (list[ExprNode]): A list of expression nodes representing the arguments passed to the call.
        """
        
        self.ident = ident
        self.value = value
    
    def __str__(self) -> str:

        """
        Returns a string representation of the CallStmtNode.

        Returns:
            str: A string representation of the CallStmtNode.
        """
        
        return f'(:= {self.ident} {self.value})'

class IndexExpr(AstNode):
    id = 'index'

    def __init__(self, ident: IdentNode, index: 'ExprNode'):

        """
        Initializes an AssignStmtNode.

        Args:
            ident (IdentNode): The identifier (variable name) being assigned.
            value ('ExprNode'): An expression node representing the value being assigned.
        """

        self.ident = ident
        self.index = index

    def __str__(self) -> str:

        """
        Returns a string representation of the AssignStmtNode.

        Returns:
            str: A string representation of the AssignStmtNode.
        """

        return f'(index {self.ident} {self.index})'

class CompoundStmtNode(StmtNode):
    id = 'compound'

    def __init__(self, start: Token, stmts: list[StmtNode]):

        """
        Initializes a CompoundStmtNode.

        Args:
            start (Token): A token representing the start of the compound statement.
            stmts (list[StmtNode]): A list of statement nodes within the compound statement.
        """

        self._start = start
        self.stmts = stmts
    
    def __str__(self) -> str:

        """
        Returns a string representation of the CompoundStmtNode.

        Returns:
            str: A string representation of the CompoundStmtNode.
        """

        return f'({" ".join(map(str, self.stmts))})'

class RepeatNode(StmtNode):
    id = 'repeat'

    def __init__(self, start: Token, condition: AstNode, statements: list[AstNode]):

        """
        Initializes a RepeatNode.

        Args:
            start (Token): A token representing the start of the repeat block.
            condition (AstNode): An expression node representing the loop condition.
            statements (list[AstNode]): List of statements within the repeat block.
        """

        self._start = start
        self.test = condition
        self.stmts = CompoundStmtNode(start, statements)
    
    def __str__(self) -> str:

        """
        Returns a string representation of the RepeatNode.

        Returns:
            str: A string representation of the RepeatNode.
        """

        return f'(repeat-until {self.test} {self.stmts})'

class WriteLnNode(StmtNode):
    id: str = 'writeLn'

    def __init__(self, start: Token, args: list['ExprNode']):

        """
        Initializes a WriteLnNode.

        Args:
            start (Token): A token representing the start of the write-ln statement.
            args (list[ExprNode]): A list of expression nodes to be printed.
        """

        self._start = start
        self.args = args
    
    def __str__(self) -> str:

        """
        Returns a string representation of the WriteLnNode.

        Returns:
            str: A string representation of the WriteLnNode.
        """

        return f'(write-ln {" ".join(map(str, self.args))})'

class ReadLnNode(StmtNode):
    id: str = 'readLn'

    def __init__(self, start: Token, var_name: Token):

        """
        Initializes a ReadLnNode.

        Args:
            start (Token): A token representing the start of the read-ln statement.
            var_name (Token): A token representing the variable where input is stored.
        """

        self._start = start
        self.var = var_name

    def __str__(self) -> str:

        """
        Returns a string representation of the ReadLnNode.

        Returns:
            str: A string representation of the ReadLnNode.
        """

        return f'(read-ln {self.var})'

class ForNode(StmtNode):
    id: str = 'for'

    def __init__(self, start: Token, name: Token, to_num: 'IntLiteral', from_num: 'IntLiteral', statements: list[AstNode]):
        
        """
        Initializes a ForNode.

        Args:
            start (Token): A token representing the start of the for loop.
            name (Token): The loop variable name.
            to_num (IntLiteral): The upper bound of the loop.
            from_num (IntLiteral): The lower bound of the loop.
            statements (list[AstNode]): List of statements within the for loop.
        """
        
        self._start = start
        self.name = name
        self.to_num = to_num
        self.from_num = from_num
        self.stmts = CompoundStmtNode(start, statements)

    def __str__(self) -> str:

        """
        Returns a string representation of the ForNode.

        Returns:
            str: A string representation of the ForNode.
        """

        return f'(for ({self.name.val} {self.from_num} {self.to_num}) {self.stmts})'

class IfNode(StmtNode):
    id: str = 'if'
    
    def __init__(self, start: Token, condition: 'ExprNode', then_branch: StmtNode, else_branch: StmtNode | None = None):
        
        """
        Initializes an IfNode.

        Args:
            start (Token): A token representing the start of the if statement.
            condition (ExprNode): An expression node representing the condition.
            then_branch (StmtNode): The statement executed if the condition is true.
            else_branch (StmtNode | None, optional): The statement executed if the condition is false. Defaults to None.
        """
        
        self._start = start
        self.test = condition
        self.then_branch = then_branch
        self.else_branch = else_branch
        
    def __str__(self) -> str:

        """
        Returns a string representation of the IfNode.

        Returns:
            str: A string representation of the IfNode.
        """

        if self.else_branch is not None:
            return f'(if ({self.test} {self.then_branch}) (else {self.else_branch}))'
        else:
            return f'(if ({self.test} {self.then_branch}))'

class ExprNode(AstNode):
    id: str = 'expr'
    type: str | None = None

    def __init__(self, start: Token, type: str):

        """
        Initializes an ExprNode.

        Args:
            start (Token): A token representing the start of the expression.
            type (str): The type of the expression.
        """

        self._start = start
        self.type = type

    def __str__(self) -> str:

        """
        Returns a string representation of the ExprNode.

        Returns:
            str: A string representation of the ExprNode.
        """

        return f"(expr ({self.start} {self.type}))"

class AddExpr(ExprNode):
    id = 'add_expr'
    
    def __init__(self, lhs: ExprNode, rhs: ExprNode):

        """
        Initializes an AddExpr.

        Args:
            lhs (ExprNode): The left-hand side expression.
            rhs (ExprNode): The right-hand side expression.
        """

        self.lhs = lhs
        self.rhs = rhs
    
    def __str__(self) -> str:

        """
        Returns a string representation of the AddExpr.

        Returns:
            str: A string representation of the AddExpr.
        """

        return f'(+ {self.lhs} {self.rhs})'

class SubExpr(ExprNode):
    id = 'sub_expr'

    def __init__(self, lhs: ExprNode, rhs: ExprNode):

        """
        Initializes a SubExpr.

        Args:
            lhs (ExprNode): The left-hand side expression.
            rhs (ExprNode): The right-hand side expression.
        """

        self.lhs = lhs
        self.rhs = rhs
    
    def __str__(self) -> str:

        """
        Returns a string representation of the SubExpr.

        Returns:
            str: A string representation of the SubExpr.
        """

        return f'(- {self.lhs} {self.rhs})'
    
class MultExpr(ExprNode):
    id = 'mult_expr'
    
    def __init__(self, lhs: ExprNode, rhs: ExprNode):

        """
        Initializes a MultExpr.

        Args:
            lhs (ExprNode): The left-hand side expression.
            rhs (ExprNode): The right-hand side expression.
        """

        self.lhs = lhs
        self.rhs = rhs
    
    def __str__(self) -> str:

        """
        Returns a string representation of the MultExpr.

        Returns:
            str: A string representation of the MultExpr.
        """

        return f'(* {self.lhs} {self.rhs})'

class DivExpr(ExprNode):
    id = 'div_expr'
    
    def __init__(self, lhs: ExprNode, rhs: ExprNode):

        """
        Initializes a DivExpr.

        Args:
            lhs (ExprNode): The left-hand side expression.
            rhs (ExprNode): The right-hand side expression.
        """

        self.lhs = lhs
        self.rhs = rhs
    
    def __str__(self) -> str:

        """
        Returns a string representation of the DivExpr.

        Returns:
            str: A string representation of the DivExpr.
        """

        return f'(/ {self.lhs} {self.rhs})'

class EqExpr(ExprNode):
    id = 'eq_expr'

    def __init__(self, lhs: ExprNode, rhs: ExprNode):

        """
        Initializes an EqExpr.

        Args:
            lhs (ExprNode): The left-hand side expression.
            rhs (ExprNode): The right-hand side expression.
        """

        self.lhs = lhs
        self.rhs = rhs
    
    def __str__(self) -> str:

        """
        Returns a string representation of the EqExpr.

        Returns:
            str: A string representation of the EqExpr.
        """

        return f'(= {self.lhs} {self.rhs})'

class NeExpr(ExprNode):
    id = 'ne_expr'

    def __init__(self, lhs: ExprNode, rhs: ExprNode):

        """
        Initializes a NeExpr.

        Args:
            lhs (ExprNode): The left-hand side expression.
            rhs (ExprNode): The right-hand side expression.
        """

        self.lhs = lhs
        self.rhs = rhs
    
    def __str__(self) -> str:

        """
        Returns a string representation of the NeExpr.

        Returns:
            str: A string representation of the NeExpr.
        """

        return f'(<> {self.lhs} {self.rhs})'

class LeExpr(ExprNode):
    id = 'le_expr'

    def __init__(self, lhs: ExprNode, rhs: ExprNode):

        """
        Initializes a LeExpr.

        Args:
            lhs (ExprNode): The left-hand side expression.
            rhs (ExprNode): The right-hand side expression.
        """

        self.lhs = lhs
        self.rhs = rhs
    
    def __str__(self) -> str:

        """
        Returns a string representation of the LeExpr.

        Returns:
            str: A string representation of the LeExpr.
        """

        return f'(<= {self.lhs} {self.rhs})'

class LtExpr(ExprNode):
    id = 'lt_expr'

    def __init__(self, lhs: ExprNode, rhs: ExprNode):

        """
        Initializes an LtExpr.

        Args:
            lhs (ExprNode): The left-hand side expression.
            rhs (ExprNode): The right-hand side expression.
        """

        self.lhs = lhs
        self.rhs = rhs
    
    def __str__(self) -> str:

        """
        Returns a string representation of the LtExpr.

        Returns:
            str: A string representation of the LtExpr.
        """

        return f'(< {self.lhs} {self.rhs})'

class GeExpr(ExprNode):
    id = 'ge_expr'

    def __init__(self, lhs: ExprNode, rhs: ExprNode):

        """
        Initializes a GeExpr.

        Args:
            lhs (ExprNode): The left-hand side expression.
            rhs (ExprNode): The right-hand side expression.
        """

        self.lhs = lhs
        self.rhs = rhs
    
    def __str__(self) -> str:

        """
        Returns a string representation of the GeExpr.

        Returns:
            str: A string representation of the GeExpr.
        """

        return f'(>= {self.lhs} {self.rhs})'
        
class GtExpr(ExprNode):
    id = 'gt_expr'

    def __init__(self, lhs: ExprNode, rhs: ExprNode):

        """
        Initializes a GtExpr.

        Args:
            lhs (ExprNode): The left-hand side expression.
            rhs (ExprNode): The right-hand side expression.
        """

        self.lhs = lhs
        self.rhs = rhs
    
    def __str__(self) -> str:

        """
        Returns a string representation of the GtExpr.

        Returns:
            str: A string representation of the GtExpr.
        """

        return f'(> {self.lhs} {self.rhs})'

class StringLiteral(ExprNode):
    id = 'str_lit'
    value: str

    def __init__(self, token: Token):

        """
        Initializes a StringLiteral.

        Args:
            token (Token): The token representing the string literal.
        """

        self.token = token
        self.value = token.val
        self.raw = token.val[1:-1]
    
    def __str__(self) -> str:

        """
        Returns a string representation of the StringLiteral.

        Returns:
            str: A string representation of the StringLiteral.
        """

        return self.value

class IntLiteral(ExprNode):
    id = 'int_lit'
    value: int

    def __init__(self, token: Token):

        """
        Initializes an IntLiteral.

        Args:
            token (Token): The token representing the integer literal.
        """

        self.token = token
        self.value = int(token.val)
    
    def __str__(self) -> str:

        """
        Returns a string representation of the IntLiteral.

        Returns:
            str: A string representation of the IntLiteral.
        """

        return str(self.value)

class RealLiteral(ExprNode):
    id = 'real_lit'
    value: float

    def __init__(self, token: Token):

        """
        Initializes a RealLiteral.

        Args:
            token (Token): The token representing the real (floating-point) literal.
        """

        self.token = token
        self.value = float(token.val)
    
    def __str__(self) -> str:

        """
        Returns a string representation of the RealLiteral.

        Returns:
            str: A string representation of the RealLiteral.
        """

        return str(self.value)

class TypeNode(AstNode):
    id = 'type'

    def __init__(self, value: Token):

        """
        Initializes a TypeNode.

        Args:
            value (Token): The token representing the type.
        """        

        self.value = value
    
    def __str__(self) -> str:

        """
        Returns a string representation of the TypeNode.

        Returns:
            str: A string representation of the TypeNode.
        """

        return f'(type {self.value.val})'

class ArrayTypeNode(TypeNode):
    id = 'array'

    def __init__(self, start: Token, subtype: TypeNode, from_num: IntLiteral, to_num: IntLiteral):
        
        """
        Initializes an ArrayTypeNode.

        Args:
            start (Token): A token representing the start of the array type.
            subtype (TypeNode): The subtype (element type) of the array.
            from_num (IntLiteral): The lower bound of the array.
            to_num (IntLiteral): The upper bound of the array.
        """
        
        super().__init__(start)

        self.subtype = subtype
        self.from_num = from_num
        self.to_num = to_num

    def __str__(self) -> str:

        """
        Returns a string representation of the ArrayTypeNode.

        Returns:
            str: A string representation of the ArrayTypeNode.
        """

        return f'(type (array {self.subtype} {self.from_num} {self.to_num}))'
