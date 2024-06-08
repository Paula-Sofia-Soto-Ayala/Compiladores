from __future__ import annotations
from collections import deque
import sys

from Visitor import TreeVisitor
from Scanner import Lexer
from AstNodes import Token
from AstNodes import *

# Represents a basic parser, takes a deque of  Token as input during initialization
class Parser:
    # Initializes the Parser with a deque of Token objects
    def __init__(self, tokens: deque[Token]) -> None:
        self.tokens = tokens

    # Returns the first token in the queue without removing it
    def peek_token(self) -> Token | None:
        return self.tokens[0] if self.tokens else None

    # Removes and returns the first token in the queue
    def next_token(self) -> Token | None:
        return self.tokens.popleft() if self.tokens else None

    # Checks if the queue is empty
    def empty_queue(self) -> bool:
        return not self.tokens or len(self.tokens) == 0

    # Checks if there are tokens in the queue
    def have_tokens(self) -> bool:
        return not self.empty_queue()

    # Expects the next token to have a specific type
    def expect_type(self, expected_type: str, error_message: str):
        if self.empty_queue():
            self.error(error_message)

        token = self.next_token()
        if token.type != expected_type:
            self.error(f"Expected {expected_type} but got: {token}")
        return token

    # Expects the next token to have a specific value
    def expect_value(self, expected_val: str, error_message: str):
        if self.empty_queue():
            self.error(error_message)

        token = self.next_token()
        if token.val != expected_val:
            self.error(f"Expected {expected_val} but got: {token}")
        return token

    # Raises a SyntaxError with the given message
    def error(self, message: str):
        raise SyntaxError(message)

    # start → program ID ; vars_block functions_block procedures_block begin statement_list end .
    def start(self) -> SourceNode:
        # Parses the program header
        prog = self.parse_program_head()

        # Parse variable declarations
        vars = self.parse_variables()

        # Parse functions and procedures
        subprogs = self.parse_subprogs()

        # Parse program body
        code = self.compound_statement(ignoreSemicolon=True)
        self.expect_value(".", "Missing end '.'")

        print("Finished parsing program")

        # Return the generated AST Nodes
        return SourceNode(prog, vars, subprogs, code)

    # Parses the program head
    def parse_program_head(self) -> ProgramNode:
        self.expect_value("program", "Empty program")

        # Parse the program identifier
        ident = self.expect_type("identifier", "No program identifier")
        self.expect_type("semicolon", "Missing semicolon")

        print("Parsed program head correctly")

        # Return the header AST Node
        return ProgramNode(ident)

    # Parses a variable declaration block inside a program/function/procedure
    # vars_block → var ID var_list’ : type_specifier ; var_declaration’ | ε
    def parse_variables(self) -> VariablesNode:
        start = None
        vars: list[VarNode] = []

        # Expect a declaration list if we see the 'VAR' keyword
        if self.peek_token().val == "var":
            start = self.expect_value("var", "Expected variable block start")

            while self.peek_token().type == "identifier":
                # Parse a variable list separated by commas
                vars.extend(self.parse_var_list())
                self.expect_value(";", "Missing semi-colon")

            print("Parsed variables block correctly")
            return VariablesNode(start, vars)
        
        # Else just return an empty VAR section Node
        print("No variable declarations")
        return VariablesNode()

    # Parses a variable list separated by commas
    # var_declaration’ → var_list : type_specifier ; var_declaration’ | ε
    # var_list’ → , ID var_list’ | ε
    def parse_var_list(self) -> list[VarNode]:
        output: list[Token] = []
        while self.have_tokens():
            # Expect identifiers until we see a colon char ':'
            output.append(self.expect_type("identifier", "Expected variable identifier"))
            
            # Expect a type definition
            if self.peek_token().val == ":":
                break
            # Else expect another variable
            else:
                self.expect_value(",", "Missing comma in variable declaration")

        # Parse the declaration list type
        self.expect_value(":", "Missing colon")
        type = self.parse_type()

        # Return a list of identifiers with their types
        return [ VarNode(name, type) for name in output ]

    # Parses the program's subprograms (function|procedure)
    # functions_block’ → function ID( params ) : type_specifier ; local_declarations begin statement_list end ; functions_block’ | ε
    # procedures_block’ → procedure ID( params ) ; local_declarations begin statement_list end ; procedures_block’ | ε
    def parse_subprogs(self) -> list[SubprogNode]:
        progs = []
        while (token := self.peek_token().val) in ("function", "procedure"):
            progs.append(self.parse_subprog(token))

        return progs

    # Parses a subprogram (function|procedure) with their vars, code, and start
    def parse_subprog(self, token: str) -> SubprogNode:
        # Parse function header
        node = self.parse_subprog_header(token, FunctionNode if token == "function" else ProcedureNode)

        # Parse local declarations
        # local_declarations → vars_block | ε
        node.vars = self.parse_variables()

        # Parse function body
        node.code = self.compound_statement()

        return node

    # Parses the function or procedure header, including: identifier, args, and return type
    def parse_subprog_header(self, token: str, node: type[SubprogNode]) -> SubprogNode:
        start = self.expect_value(token, f"Missing {token} keyword")
        print(f"Parsing {token}: {self.peek_token().val}")
        
        # Parses the program identifier name
        name = self.expect_type("identifier", "Missing function identifier")
        self.expect_value("(", "Missing open paren")

        # Parse function parameters
        params = []
        while self.peek_token().type == "identifier":
            # Parse a list of arguments separated by semicolons
            # params → ID var_list’ : type_specifier ; param_list’ | ε
            params.extend(self.parse_var_list())
            # Reached end of function params
            if self.peek_token().val == ")":
                break
            # Separate params with a ';'
            else:
                self.expect_value(";", "Missing semicolon")

        self.expect_value(")", "Missing closing paren")
        
        # Parse a return type if parsing a function
        if token == "function": 
            self.expect_value(":", "Missing colon")
            ret_type = self.parse_type()
        # Else ignore if parsing a procedure
        else: ret_type = None

        self.expect_value(";", "Missing semi-colon")

        # Return the subprogram AST Node
        return node(start, name, params, ret_type)

    # Parse a statement AST Node
    # statement_list → statement ; statement_list’
    def parse_statement(self, ignoreSemicolon = False) -> StmtNode:
        # If we see a begin then parse a compound statement
        # statement_list’ → statement ; statement_list’ | ε
        if self.peek_token().val == "begin":
            return self.compound_statement(ignoreSemicolon)
        # Else parse a simple statement (no semicolon needed)
        else:
            return self.simple_statement()

    # Parses a simple statement, which doesn't need a semicolon
    # statement → ID statement′
    #   | begin statement_list end
    #   | if ( logic_expression ) then statement selection_stmt′
    #   | for ID := int_number to int_number do statement
    #   | repeat statement_list until ( logic_expression )
    #   | readln ( ID var_list′ );
    #   | writeln ( output output_list′) ;
    def simple_statement(self) -> StmtNode:
        match self.peek_token():
            case Token(val="if"):
                # Parses an if-then-else block
                return self.parse_if_block()
            case Token(val="for"):
                # Parses a for-loop block
                return self.parse_for_loop()
            case Token(val="repeat"):
                # Parses a repeat-until block
                return self.parse_repeat()
            case Token(val="readLn"):
                # Parses a readLn statement
                return self.parse_readln()
            case Token(val="writeLn"):
                # Parses a writeLn statement
                return self.parse_writeln()
            case Token(type="identifier"):
                # Parses either a function call
                if self.tokens[1].val == "(":
                    return self.parse_call()
                # Or an assignment statement
                else:
                    # assignment_stmt’ → arithmetic_expression | STRING
                    return self.parse_assignment()
            case _:
                # Errors when failing to match any previous statement
                self.error("Expected a simple statement")

    # Parses a compound statement, which requires a begin-else
    # And a list of semicolon separated simple statements.
    def compound_statement(self, ignoreSemicolon = False) -> StmtNode:
        start = self.expect_value("begin", "Missing begin keyword")
        stmts = []

        # Parse the nested statements inside the compound statement
        while self.peek_token().val != "end":
            stmts.append(self.parse_statement(ignoreSemicolon))
            self.expect_value(";", "Missing semi-colon")

        # Stop once we find an end keyword
        self.expect_value("end", "Missing end keyword")

        # Some constructs have optional semicolons at the end
        # Ignore if `ignoreSemicolon = True`
        if not ignoreSemicolon:
            self.expect_value(";", "Missing semi-colon")
        
        # Return a compound statement AST Node with its statements list
        return CompoundStmtNode(start, stmts)

    # Parse a variable assignment statement
    # var′ → [ arithmetic_expression ] | ε
    def parse_assignment(self) -> StmtNode:

        # Expect an identifier to assign to (lhs)
        name = self.parse_identifier()

        # If the left side is an array expect an index
        if self.peek_token().val == "[":
            self.expect_value("[", "Missing indexing open bracket")

            # Arrays accept expressions on their indexers, i.e: array[expr]
            index = self.parse_expression()
            # Create an indexer expression AST Node to report
            name = IndexExpr(name, index)
            self.expect_value("]", "Missing indexing closing bracket")

        # Assignment operator and right side expression
        self.expect_value(":=", "Expected assignment operator")
        
        # Check right side of the assignment
        expr = self.parse_expression()
        
        # Return an assignment statement AST Node
        return AssignStmtNode(name, expr)

    # Parse a writeLn statement with its argument list
    def parse_writeln(self) -> WriteLnNode:
        start = self.expect_value("writeLn", "Expected WRITELN keyword")
        self.expect_value("(", "Missing open parenthesis")

        # Parse the arguments to be written to IO
        # output_list’ → output output_list’ | ε
        # output → arithmetic_expression | STRING | ε
        args = self.parse_arguments(optional=True)
        self.expect_value(")", "Missing closing parenthesis")

        # Return an AST Node with the statement start and args
        return WriteLnNode(start, args)

    # Parse an argument list for subprogram calls / writeLn statements
    # args → arithmetic_expression arg_list’ | ε
    def parse_arguments(self, optional: bool) -> list[ExprNode]:
        # If arguments are optional no error on empty arguments
        if self.peek_token().val == ")":
            if optional:
                print("No function or procedure arguments")
                return []
            else:
                self.error("Empty arguments on call")

        exprs: list[ExprNode] = []
        while self.have_tokens():
            # arg_list’ → arithmetic_expression arg_list’ | ε
            # Parse the argument expressions individually
            exprs.append(self.parse_expression())
            
            # If we reach a closing paren then exit
            if self.peek_token().val == ")":
                break
            # Otherwise expect a comma and more expressions
            else:
                self.expect_value(",", "Missing comma in output list")

        # Return the arguments list as an ExprNode list
        return exprs

    # Parse a readLn statement
    def parse_readln(self) -> ReadLnNode:
        start = self.expect_value("readLn", "Expected READLN keyword")
        self.expect_value("(", "Missing open parenthesis")

        # Expect an identifier that will receive the value of the IO read
        id = self.expect_type("identifier", "Missing target identifier for readLn")
        self.expect_value(")", "Missing closing parenthesis")

        # Return an AST Node with the ReadLn statement
        return ReadLnNode(start, id)

    # Parse an if-then-else statement
    def parse_if_block(self) -> IfNode:

        # Save the if branch start
        start = self.expect_value("if", "Expected IF keyword")
        else_branch = None
        
        self.expect_value("(", "Missing open parenthesis")

        # Parse the condition expr evaluated for the branch
        condition = self.parse_condition()
        self.expect_value(")", "Missing closing parenthesis")
        self.expect_value("then", "Missing THEN keyword")

        # Parse the IF-THEN branch with NO semicolon
        then_branch = self.parse_statement(ignoreSemicolon=True)

        # Optionally parse the ELSE branch with NO semicolon
        # selection_stmt’ → else statement | ε
        if self.peek_token().val == "else":
            self.expect_value("else", "Missing ELSE keyword")
            # Save the else branch statement
            else_branch = self.parse_statement(ignoreSemicolon=True)

        # Return an IfBlock AST Node with its start, condition, and branches
        return IfNode(start, condition, then_branch, else_branch)

    # Parse a for-loop statement with its range and code block
    def parse_for_loop(self) -> ForNode:

        # Save the start node for the loop
        start = self.expect_value("for", "Expected FOR keyword")

        # Parse the loop index variable
        name = self.parse_identifier()
        self.expect_value(":=", "Expected assignment operator")

        # Parse the for-loop index range
        to_num = self.parse_int_lit()
        self.expect_value("to", "Expected TO keyword")
        from_num = self.parse_int_lit()

        self.expect_value("do", "Expected DO keyword")
        
        # Parse its statement list and save them
        stmts = [self.parse_statement(ignoreSemicolon=True)]

        # Return a ForLoop AST Node with its start, index var, range, and statements
        return ForNode(start, name, to_num, from_num, stmts)
        
    # Parse a repeat-until code block with its condition
    def parse_repeat(self) -> RepeatNode:
        # Save the repeat block start
        start = self.expect_value("repeat", "Expected REPEAT keyword")

        stmts = []
        while self.peek_token().val != "until":
            # Parse each statement individually
            stmts.append(self.parse_statement(ignoreSemicolon=True))
            self.expect_value(";", "Missing semicolon")

        # Stop once we find the UNTIL keyword
        self.expect_value("until", "Expected UNTIL keyword")
        self.expect_value("(", "Missing open parenthesis")

        # Parse the UNTIL loop condition
        condition = self.parse_condition()
        self.expect_value(")", "Missing closing parenthesis")

        # Return a RepeatUntil AST Node with its start, condition, and statements
        return RepeatNode(start, condition, stmts)

    # Parse a subprogram call with its arguments
    def parse_call(self) -> StmtNode:

        # Parse the function identifier
        name = self.parse_identifier()
        self.expect_value("(", "Missing open parenthesis")

        # Parse its argument list
        args = self.parse_arguments(optional=True)
        self.expect_value(")", "Missing closing parenthesis")

        # Return a CallStmtNode with the subprogram name and args
        return CallStmtNode(name, args)

    # Parse a condition expression
    def parse_condition(self) -> ExprNode:
        return self.parse_equality_expr()

    # Parse an expression Node
    # i.e: func call, identifiers, literals, conditions, array index, etc...
    # arithmetic_expression → term   arithmetic_expression’
    def parse_expression(self) -> ExprNode:
        return self.parse_equality_expr()

    # Lowest precedence expr: equality < relational
    # relop → <= | < | > | >= | = | <>
    def parse_equality_expr(self) -> ExprNode:
        lhs = self.parse_relational_expr()
        
        token = self.peek_token()
        match token.val:
            case "=": node = EqExpr
            case "<>": node = NeExpr
            case _: return lhs

        # Found the expr operator
        if node:
            self.next_token()
        
        rhs = self.parse_expression()
        return node(lhs, rhs)

    # Higher precedence than equality: relational < addition
    # relop → <= | < | > | >= | = | <>
    def parse_relational_expr(self) -> ExprNode:
        lhs = self.parse_add_expr()
        
        token = self.peek_token()
        match token.val:
            case "<": node = LeExpr
            case "<=": node = LtExpr
            case ">": node = GtExpr
            case ">=": node = GeExpr
            case _: return lhs

        # Found the expr operator
        if node:
            self.next_token()
        
        rhs = self.parse_expression()
        return node(lhs, rhs)

    # Higher precedence than relational expr: addition < multiplication
    # arithmetic_expression’ → + term arithmetic_expression’| - term arithmetic_expression’| ε
    def parse_add_expr(self) -> ExprNode:
        lhs = self.parse_mult_expr()
        
        token = self.peek_token()
        match token.val:
            case "+": node = AddExpr
            case "-": node = SubExpr
            case _: return lhs

        # Found the expr operator
        if node:
            self.next_token()
        
        rhs = self.parse_expression()
        return node(lhs, rhs)

    # Higher precedence than additive expr: multiplication < simple_expr
    # term’ → * factor term’ | / factor  term’ | ε
    def parse_mult_expr(self) -> ExprNode:
        lhs = self.parse_simple_expr()
        
        token = self.peek_token()
        match token.val:
            case "*": node = MultExpr
            case "/": node = DivExpr
            case _: return lhs

        # Found the expr operator
        if node:
            self.next_token()
        
        rhs = self.parse_expression()
        return node(lhs, rhs)

    # Highest precedence expression
    # Includes: literals, calls, indexing, and nested expressions
    # Factor → ID factor′ | int_number | real_number | ( arithmetic_operator )
    def parse_simple_expr(self) -> ExprNode:
        token = self.peek_token()

        # We've reached a leaf, so we expect a Factor
        if token.type in ["integer", "real", "string"]:
            return self.parse_literal()
        # factor′ → ( args ) | var′
        elif token.type == "identifier":
            if self.tokens[1].val == "(":
                # function call
                return self.parse_call()
            if self.tokens[1].val == "[":
                # indexing expression
                return self.parse_array_indexing()
            else:
                # identifier expression
                return self.parse_identifier()
        # Otherwise parse an expr inside parenthesis
        # factor′ → ( args ) | var′
        elif token.val == "(":
            self.next_token()  # consume '('
            output = self.parse_expression()
            self.expect_value(")", "Expected closing parenthesis")
            return output
        # If we fail to match an expression then throw an error
        else:
            self.error("Expected simple expression")

    # Parse an array indexing operation
    def parse_array_indexing(self) -> IndexExpr:
        # Parse the array name
        name = self.parse_identifier()
        self.expect_value("[", "Missing open bracket")

        # Parse the indexing expression inside brackets
        index = self.parse_expression()
        self.expect_value("]", "Missing closing bracket")

        # Return an IndexExpr AST Node with the array and indexer
        return IndexExpr(name, index)

    # Parse a variable indentifier
    # factor′ → ( args ) | var′
    def parse_identifier(self) -> IdentNode:
        name = self.expect_type("identifier", "Expected an identifier")

        # Return an indentifier AST Node with the variablenam
        return IdentNode(name)

    # Parse a literal: string | integer | real
    # Examples: "hello", 4, 3.1416
    def parse_literal(self) -> ExprNode:
        match self.peek_token().type:
            case "string": return self.parse_string_lit()
            case "integer": return self.parse_int_lit()
            case "real": return self.parse_real_lit()

    # Parse a string literal AST Node, i.e: "world"
    def parse_string_lit(self) -> StringLiteral:
        token = self.expect_type("string", "Expected a string")
        return StringLiteral(token)
        
    # Returns a real literal AST Node, i.e: 12.34
    def parse_real_lit(self) -> RealLiteral:
        token = self.expect_type("real", "Expected a real")
        return RealLiteral(token)
    
    # Returns an integer literal AST Node, i.e: 25
    def parse_int_lit(self) -> IntLiteral:
        token = self.expect_type("integer", "Expected an integer")
        return IntLiteral(token)

    # Parse a type declaration
    # type_specifier → integer | real | string | array [ int_number .. int_number ] of basic_type
    def parse_type(self) -> TypeNode:
        token = self.peek_token()

        match token.val:
            # Match either a simple type
            case "integer" | "real" | "string":
                return self.parse_simple_type()
            # Or an array type
            case "array":
                return self.parse_array_type()
            # Or throw an error on a failing match
            case _:
                self.error(f"Expected type declaration but got: {token}")
            
    # Parses a simple type declaration
    # basic_type → integer | real | string
    def parse_simple_type(self) -> TypeNode:
        token = self.next_token()
        match token.val:
            # Returns a Type AST Node with the simple type
            case "integer" | "real" | "string":
                return TypeNode(token)
            # Th
            case _:
                self.error(f"Expected type but got: {token}")
                return None

    # Parses an array type with its range + simple type
    def parse_array_type(self) -> TypeNode:
        # Parse the type start node
        start = self.expect_value("array", "Missing array keyword")
        self.expect_value("[", "Missing open bracket")

        # Parse the array length / range
        from_num = self.parse_int_lit()
        self.expect_value("..", "Missing range operator")
        to_num = self.parse_int_lit()

        self.expect_value("]", "Missing close bracket")
        self.expect_value("of", "Missing 'of' keyword")
        
        # Parse the array subtype (a simple type, i.e: real, int, string)
        subtype = self.parse_simple_type()

        # Return an ArrayType AST Node with its start, subtype, and range
        return ArrayTypeNode(start, subtype, from_num, to_num)


def main():
    args = sys.argv

    # Validate we got a file name arg
    if len(args) > 1:
        # Run the tokenization step
        print("\n--Starting lexical anaylisis--\n\n")
        lexer = Lexer()
        lexer.start(args[1] or "Test5.txt")

        # Print the token stream
        # lexer.print_tokens()

        print("\n--Finished lexical anaylisis--\n")

        print("--Started syntax anaylisis--\n")
        # Create an instance of the parser
        parser = Parser(deque(lexer.tokens))
        tree = parser.start()
        print("\n--Finished syntax anaylisis--\n")

        # Print the generated AST Nodes
        print(f"AST: {tree}")

        print("\n--Started semantical analysis--\n")
        # Visit AST and update symbol table
        visitor = TreeVisitor(symbols=lexer.symbols)
        visitor.visit(tree)

        # Print the updated symbol table
        lexer.print_symbols()
        # lexer.print_tokens()
    else:
        print("No file provided for parsing")

if __name__ == "__main__":
    main()

""" 3.1.- Gramática final 
1.   start → program ID ; vars_block functions_block procedures_block begin statement_list end •
2.   vars_block → VAR var_list' : type_specifier ; var_declaration' | ε
3.   var_declaration' → var_list : type_specifier ; var_declaration' | ε
4.   var_list' → , ID var_list' | ε
5.   type_specifier → integer | real | string | array [ int_number • • int_number ] of basic_type
6.   basic_type → integer | real | string
7.   functions_block → functions_block'
8.   functions_block' → function ID( params ) : type_specifier ; local_declarations begin statement_list end ; functions_block' | ε
9.   procedures_block → procedures_block'
10.  procedures_block' → procedure ID( params ) ; local_declarations begin statement_list end ; procedures_block' | ε
11.  params → ID var_list' : type_specifier ; param_list' | ε
12.  param_list' → var_list : type_specifier ; param_list' | ε
13.  local_declarations → vars_block | ε
14.  statement_list → statement ; statement_list'
15.  statement_list' → statement ; statement_list' | ε
16.  statement → ID statement' | begin statement_list end | if ( logic_expression ) then statement selection_stmt' | for ID := int_number to int_number do statement | repeat statement_list until ( logic_expression ) | readln ( ID var_list' ); | writeln ( output    output_list') ;
17.  statement' → var' :=  assignment_stmt' | ( args )
18.  assignment_stmt' → arithmetic_expression | STRING
19.  selection_stmt' → else statement | ε
20.  output_list' → , output output_list' | ε
21.  output → arithmetic_expression | STRING | ε
22.  var' → [ arithmetic_expression ] | ε
23.  logic_expression → arithmetic_expression relop arithmetic_expression
24.  relop → <= | < | > | >= | = | <> 
25.  arithmetic_expression → term   arithmetic_expression'
26.  arithmetic_expression' → + term arithmetic_expression'| - term arithmetic_expression'| ε
27.  term → factor term'
28.  term' → * factor   term' | / factor  term' | ε
29.  factor → ID factor' | int_number | real_number  |  (arithmetic_operator)
30.  factor' → ( args ) | var'
31.  args → arithmetic_expression arg_list' | ε
32.  arg_list' → , arithmetic_expression arg_list' | ε """
