import os 
import sys
from typing import List

# Symbol table entry class
class Symbol:
    def __init__(self, id: int, name: str, type: str):
        self.id = id
        self.name = name
        self.type = type
    def __hash__(self) -> int:
        return hash((self.name, self.type))
    def __eq__(self, other) -> bool:
        if not isinstance(other, Symbol):
            return False
        return (self.name, self.type) == (other.name, other.type)
    def __str__(self) -> str:
        return f"{self.id}: {self.name} - {self.type}"
    
# Keywords and their corresponding token names.       
keywords = {
    'array': 'array',
    'begin': 'begin',
    'do': 'do',
    'else': 'else',
    'end': 'end',
    'for': 'for',
    'function': 'function',
    'if': 'if',
    'integer': 'integer',
    'of': 'of',
    'procedure': 'procedure',
    'program': 'program',
    'readLn': 'readLn',
    'real': 'real',
    'repeat': 'repeat',
    'string': 'string',
    'then': 'then',
    'to': 'to',
    'until': 'until',
    'var': 'var',
    'writeLn': 'writeLn',
}

# Operators and their corresponding token names.
operators = {
    '-': 'minus',
    ':': 'type_assign',
    ':=': 'val_assign',
    '*': 'mult',
    '/': 'div',
    '+': 'plus',
    '<': 'less_than',
    '<=': 'less_eq_than',
    '<>': 'not_eq',
    '=': 'eq',
    '>': 'more_than',
    '>=': 'more_eq_than',
}

# Punctuation characters and their corresponding token names.
punctuation = {
    ',': 'comma',
    ';': 'semi-colon',
    '.': 'dot',
    '{': 'open_curly',
    '}': 'close_curly',
    '(': 'open_paren',
    ')': 'close_paren',
    '[': 'open_brackets',
    ']': 'close_brackets',
    "'": 'single_quote',
}

# Transition table --> (Current State, Input Character Class): Next State.
transition_table = {
    (0, 'letter'): 1,
    (0, 'digit'): 4,
    (0, '_'): 2,
    (0, '.'): 34,
    (0, '('): 7,
    (0, '*'): 28,
    (0, ')'): 35,
    (0, '<'): 10,
    (0, '='): 30,
    (0, '>'): 11,
    (0, ':'): 12,
    (0, '+'): 26,
    (0, '-'): 27,
    (0, '/'): 29,
    (0, ';'): 31,
    (0, ','): 32,
    (0, "'"): 13,
    (0, '.'): 34,
    (0, '['): 36,
    (0, ']'): 37,
    (0, '{'): 14,
    (0, '}'): 42,
    (0, 'whitespace'): 0,
    (0, 'eof'): 0,
    (0, 'other'): 43,

    (1, 'letter'): 1,
    (1, 'digit'): 1,
    (1, '_'): 1,
    (1, '.'): 15,
    (1, '('): 15,
    (1, '*'): 15,
    (1, ')'): 15,
    (1, '<'): 15,
    (1, '='): 15,
    (1, '>'): 15,
    (1, ':'): 15,
    (1, '+'): 15,
    (1, '-'): 15,
    (1, '/'): 15,
    (1, ';'): 15,
    (1, ','): 15,
    (1, "'"): 15,
    (1, '.'): 15,
    (1, '['): 15,
    (1, ']'): 15,
    (1, '{'): 15,
    (1, '}'): 15,
    (1, 'whitespace'): 15,
    (1, 'eof'): 15,
    (1, 'other'): 43,
 
    (2, 'letter'): 3,
    (2, 'digit'): 3,
    (2, '_'): 43,
    (2, '.'): 15,
    (2, '('): 15,
    (2, '*'): 15,
    (2, ')'): 15,
    (2, '<'): 15,
    (2, '='): 15,
    (2, '>'): 15,
    (2, ':'): 15,
    (2, '+'): 15,
    (2, '-'): 15,
    (2, '/'): 15,
    (2, ';'): 15,
    (2, ','): 15,
    (2, "'"): 15,
    (2, '.'): 15,
    (2, '['): 15,
    (2, ']'): 15,
    (2, '{'): 15,
    (2, '}'): 15,
    (2, 'whitespace'): 15,
    (2, 'eof'): 15,
    (2, 'other'): 43,

    (3, 'letter'): 1,
    (3, 'digit'): 1,
    (3, '_'): 1,
    (3, '.'): 15,
    (3, '('): 15,
    (3, '*'): 15,
    (3, ')'): 15,
    (3, '<'): 15,
    (3, '='): 15,
    (3, '>'): 15,
    (3, ':'): 15,
    (3, '+'): 15,
    (3, '-'): 15,
    (3, '/'): 15,
    (3, ';'): 15,
    (3, ','): 15,
    (3, "'"): 15,
    (3, '.'): 15,
    (3, '['): 15,
    (3, ']'): 15,
    (3, '{'): 15,
    (3, '}'): 15,
    (3, 'whitespace'): 15,
    (3, 'eof'): 15,
    (3, 'other'): 43,

    (4, 'letter'): 16,
    (4, 'digit'): 4,
    (4, '_'): 16,
    (4, '.'): 16,
    (4, '('): 16,
    (4, '*'): 16,
    (4, ')'): 16,
    (4, '<'): 16,
    (4, '='): 16,
    (4, '>'): 16,
    (4, ':'): 16,
    (4, '+'): 16,
    (4, '-'): 16,
    (4, '/'): 16,
    (4, ';'): 16,
    (4, ','): 16,
    (4, "'"): 16,
    (4, '.'): 5,
    (4, '['): 16,
    (4, ']'): 16,
    (4, '{'): 16,
    (4, '}'): 16,
    (4, 'whitespace'): 16,
    (4, 'eof'): 16,
    (4, 'other'): 43,

    (5, 'letter'): 17,
    (5, 'digit'): 6,
    (5, '_'): 17,
    (5, '.'): 17,
    (5, '('): 17,
    (5, '*'): 17,
    (5, ')'): 17,
    (5, '<'): 17,
    (5, '='): 17,
    (5, '>'): 17,
    (5, ':'): 17,
    (5, '+'): 17,
    (5, '-'): 17,
    (5, '/'): 17,
    (5, ';'): 17,
    (5, ','): 17,
    (5, "'"): 17,
    (5, '.'): 17,
    (5, '['): 17,
    (5, ']'): 17,
    (5, '{'): 17,
    (5, '}'): 17,
    (5, 'whitespace'): 17,
    (5, 'eof'): 17,
    (5, 'other'): 43,

    (6, 'letter'): 17,
    (6, 'digit'): 6,
    (6, '_'): 17,
    (6, '.'): 17,
    (6, '('): 17,
    (6, '*'): 17,
    (6, ')'): 17,
    (6, '<'): 17,
    (6, '='): 17,
    (6, '>'): 17,
    (6, ':'): 17,
    (6, '+'): 17,
    (6, '-'): 17,
    (6, '/'): 17,
    (6, ';'): 17,
    (6, ','): 17,
    (6, "'"): 17,
    (6, '.'): 17,
    (6, '['): 17,
    (6, ']'): 17,
    (6, '{'): 17,
    (6, '}'): 17,
    (6, 'whitespace'): 17,
    (6, 'eof'): 17,
    (6, 'other'): 43,

    (7, 'letter'): 18,
    (7, 'digit'): 18,
    (7, '_'): 18,
    (7, '.'): 18,
    (7, '('): 18,
    (7, '*'): 8,
    (7, ')'): 18,
    (7, '<'): 18,
    (7, '='): 18,
    (7, '>'): 18,
    (7, ':'): 18,
    (7, '+'): 18,
    (7, '-'): 18,
    (7, '/'): 18,
    (7, ';'): 18,
    (7, ','): 18,
    (7, "'"): 18,
    (7, '.'): 18,
    (7, '['): 18,
    (7, ']'): 18,
    (7, '{'): 18,
    (7, '}'): 18,
    (7, 'whitespace'): 18,
    (7, 'eof'): 18,
    (7, 'other'): 43,

    (8, 'letter'): 8,
    (8, 'digit'): 8,
    (8, '_'): 8,
    (8, '.'): 8,
    (8, '('): 8,
    (8, '*'): 9,
    (8, ')'): 8,
    (8, '<'): 8,
    (8, '='): 8,
    (8, '>'): 8,
    (8, ':'): 8,
    (8, '+'): 8,
    (8, '-'): 8,
    (8, '/'): 8,
    (8, ';'): 8,
    (8, ','): 8,
    (8, "'"): 8,
    (8, '.'): 8,
    (8, '['): 8,
    (8, ']'): 8,
    (8, '{'): 8,
    (8, '}'): 8,
    (8, 'whitespace'): 8,
    (8, 'eof'): 40,
    (8, 'other'): 8,

    (9, 'letter'): 8,
    (9, 'digit'): 8,
    (9, '_'): 8,
    (9, '.'): 8,
    (9, '('): 8,
    (9, '*'): 9,
    (9, ')'): 19,
    (9, '<'): 8,
    (9, '='): 8,
    (9, '>'): 8,
    (9, ':'): 8,
    (9, '+'): 8,
    (9, '-'): 8,
    (9, '/'): 8,
    (9, ';'): 8,
    (9, ','): 8,
    (9, "'"): 8,
    (9, '.'): 8,
    (9, '['): 8,
    (9, ']'): 8,
    (9, '{'): 8,
    (9, '}'): 8,
    (9, 'whitespace'): 8,
    (9, 'eof'): 40,
    (9, 'other'): 8,

    (10, 'letter'): 20,
    (10, 'digit'): 20,
    (10, '_'): 20,
    (10, '.'): 20,
    (10, '('): 20,
    (10, '*'): 20,
    (10, ')'): 20,
    (10, '<'): 20,
    (10, '='): 21,
    (10, '>'): 22,
    (10, ':'): 20,
    (10, '+'): 20,
    (10, '-'): 20,
    (10, '/'): 20,
    (10, ';'): 20,
    (10, ','): 20,
    (10, "'"): 20,
    (10, '.'): 20,
    (10, '['): 20,
    (10, ']'): 20,
    (10, '{'): 20,
    (10, '}'): 20,
    (10, 'whitespace'): 20,
    (10, 'eof'): 20,
    (10, 'other'): 43,

    (11, 'letter'): 23,
    (11, 'digit'): 23,
    (11, '_'): 23,
    (11, '.'): 23,
    (11, '('): 23,
    (11, '*'): 23,
    (11, ')'): 23,
    (11, '<'): 23,
    (11, '='): 24,
    (11, '>'): 23,
    (11, ':'): 23,
    (11, '+'): 23,
    (11, '-'): 23,
    (11, '/'): 23,
    (11, ';'): 23,
    (11, ','): 23,
    (11, "'"): 23,
    (11, '.'): 23,
    (11, '['): 23,
    (11, ']'): 23,
    (11, '{'): 23,
    (11, '}'): 23,
    (11, 'whitespace'): 23,
    (11, 'eof'): 23,
    (11, 'other'): 43,
 
    (12, 'letter'): 39,
    (12, 'digit'): 39,
    (12, '_'): 39,
    (12, '.'): 39,
    (12, '('): 39,
    (12, '*'): 39,
    (12, ')'): 39,
    (12, '<'): 39,
    (12, '='): 25,
    (12, '>'): 39,
    (12, ':'): 39,
    (12, '+'): 39,
    (12, '-'): 39,
    (12, '/'): 39,
    (12, ';'): 39,
    (12, ','): 39,
    (12, "'"): 39,
    (12, '.'): 39,
    (12, '['): 39,
    (12, ']'): 39,
    (12, '{'): 39,
    (12, '}'): 39,
    (12, 'whitespace'): 39,
    (12, 'eof'): 39,
    (12, 'other'): 43,
 
    (13, 'letter'): 13,
    (13, 'digit'): 13,
    (13, '_'): 13,
    (13, '.'): 13,
    (13, '('): 13,
    (13, '*'): 13,
    (13, ')'): 13,
    (13, '<'): 13,
    (13, '='): 13,
    (13, '>'): 13,
    (13, ':'): 13,
    (13, '+'): 13,
    (13, '-'): 13,
    (13, '/'): 13,
    (13, ';'): 13,
    (13, ','): 13,
    (13, "'"): 33,
    (13, '.'): 13,
    (13, '['): 13,
    (13, ']'): 13,
    (13, '{'): 13,
    (13, '}'): 13,
    (13, 'whitespace'): 13,
    (13, 'eof'): 13,
    (13, 'other'): 13,
 
    (14, 'letter'): 14,
    (14, 'digit'): 14,
    (14, '_'): 14,
    (14, '.'): 14,
    (14, '('): 14,
    (14, '*'): 14,
    (14, ')'): 14,
    (14, '<'): 14,
    (14, '='): 14,
    (14, '>'): 14,
    (14, ':'): 14,
    (14, '+'): 14,
    (14, '-'): 14,
    (14, '/'): 14,
    (14, ';'): 14,
    (14, ','): 14,
    (14, "'"): 34,
    (14, '.'): 14,
    (14, '['): 14,
    (14, ']'): 14,
    (14, '{'): 41,
    (14, '}'): 38,
    (14, 'whitespace'): 14,
    (14, 'eof'): 41,
    (14, 'other'): 14,
}

# Final states of transition table --> State: Token Type.
final_states = {
    15: 'identifier',
    16: 'integer',
    17: 'real',
    18: 'open_parens',
    19: 'multiline_comment',
    20: 'less_than',
    21: 'less_eq_than', 
    22: 'not_eq',
    23: 'more_than',
    24: 'more_eq_than',
    25: 'val_assign',
    26: 'plus',
    27: 'minus',
    28: 'mult',
    29: 'div',
    30: 'eq',
    31: 'semicolon',
    32: 'comma',
    33: 'string',
    34: 'point',
    35: 'closed_parens',
    36: 'open_sqre_bracket',
    37: 'closed_sqre_bracket',
    38: 'single_line_comment',
    39: 'type_assignment',
}

# Error states of the transition tables --> State: Error type.
error_states = {
    40: 'eof',
    41: 'single_line_comment_error',
    42: 'open_curly_brace_error',
    43: 'invalid_char_error',
}

# Function to check if a word is a keyword.
def is_keyword(word: str):
    return word in keywords.keys()

# Function to check if a word is an operator.
def is_operator(word: str):
    return word in operators.keys()

# Function to check if a word is a punctuation character.
def is_punctuation(word: str):
    return word in punctuation.keys()

# Function to check if a word is a digit.
def is_digit(word: str):
    return str.isnumeric(word)

# Function to check if a word is whitespace.
def is_whitespace(word: str):
    return word.isspace()

# Function to clean the source file from comments and excessive whitespace.
# (Removes comments and replace multiple whitespaces with a single space)
def clean_file(source: str) -> str:
    # Remove comments
    comment_starters = ['{', '(*']
    comment_enders = ['}', '*)']
    for start, end in zip(comment_starters, comment_enders):
        while start in source:
            start_index = source.find(start)
            end_index = source.find(end, start_index)
            if end_index == -1:  # If the end of the comment is not found
                source = source[:start_index]  # Remove from start to the end
            else:
                source = source[:start_index] + source[end_index + len(end):]

    # Replace multiple whitespaces with a single space
    result = []
    in_whitespace = False
    for char in source:
        if char.isspace():
            if not in_whitespace:
                result.append(' ')
                in_whitespace = True
        else:
            in_whitespace = False
            result.append(char)

    return ''.join(result)

# Function to determine the type of character (whitespace, letter, operator, etc.).
def get_char_type(char: str):
    if is_whitespace(char):
        return "whitespace"
    if str.isalpha(char):
        return "letter"
    if is_operator(char):
        return char
    if is_punctuation(char):
        return char
    if is_digit(char):
        return "digit"
    
    return "other"

# Function to read the contents of a file.
def read_file(file_path):
    with open(file_path, 'r') as file:
        return file.read()
  
# Function to create a token based on the lexeme and state.
def create_token(lexeme: str, state: int):
    # Ignores comment final states
    if state in [38, 19]:
        return None
    if is_whitespace(lexeme):
        return None
    if is_keyword(lexeme):
        return (lexeme, 'keyword')
    if state in final_states:
        return (lexeme, final_states[state])
    else:
        return None

# Class to hold the result of the tokenization process.
class TokenizerResult:
    def __init__(self, tokens: list[tuple[str, str]], symbols: dict[str, List[Symbol] | set[Symbol]]):
        self.tokens = tokens
        self.symbols = symbols

# Function to tokenize the source code into tokens and symbol table entries.
def tokenize(source: str) -> TokenizerResult:
    # Initialization of symbol table, state, lexeme, tokens, and index.
    symbol_table: dict[str, List[Symbol] | set[Symbol]] = {
        'identifier': set([]),
        'real': [],
        'integer': [],
        'string': []
    }

    current_state = 0
    lexeme = ''
    tokens = []

    index = 0

    # Processing each character in the source code
    while (index < len(source)):
        char = source[index]
        char_type = get_char_type(char)
        transition = (current_state, char_type)
        next_state = transition_table[transition]

        # If in an error state print error
        if next_state in error_states:
            print(f"Invalid state found: {transition}")
            print(f"Error: {error_states[next_state]}")
            exit(1)

        if next_state in final_states:
            # hack that closes quotes, reason: missing delimiter state
            if lexeme and char == "'":
                lexeme += char
                index += 1

            # two char operators, i.e: >= or :=
            if is_operator(lexeme) and is_operator(char):
                lexeme += char
                index += 1

            token = create_token(lexeme or char, next_state)
            if token:
                (name, type) = token

                symbol_list = symbol_table.get(type)
                if symbol_list is not None:
                    id = len(symbol_list) + 1
                    symbol = Symbol(id, name, type)
                    token = (name, type, id)

                    if isinstance(symbol_list, set):
                        symbol_list.add(symbol)
                    else:
                        symbol_list.append(symbol)

                tokens.append(token)

            # if lexeme was delimiter move to the next char
            # needed for operators and punctuation
            # if we just closed a comment move up as well
            if not lexeme:
                index += 1
            if next_state in [38, 19]:
                print("Comment: " + lexeme + char)
                index += 1

            lexeme = ''
            current_state = 0
            continue

        if reset_lexeme(current_state, next_state):
            lexeme = char
        else:
            lexeme += char

        current_state = next_state
        index += 1

    return TokenizerResult(tokens, symbol_table)

# Function to determine whether the current lexeme should be reset when transitioning from one state to another in the state machine.
def reset_lexeme(current_state: int, next_state: int):
    # Moving from ( to * inside a comment
    if current_state == 7 and next_state == 8:
        return False
    # Moving from comment to *
    if current_state == 8 and next_state == 9:
        return False
    # Moving from * to ) in a comment
    if current_state == 9 and next_state == 19:
        return False
    # Moving digit to '.'
    if current_state == 4 and next_state == 5:
        return False
    # Moving from '.' to digit
    if current_state == 5 and next_state == 6:
        return False
    
    return current_state != next_state

# Opens file, tokenizes it and returns result
def lexer(filename) -> TokenizerResult:
    dir_path = os.path.dirname(os.path.realpath(__file__))
    file_path = os.path.join(dir_path, filename)
    source = read_file(file_path)
    result = tokenize(source)

    return result

def main():
    args = sys.argv
    if len(args) > 1:
        result = lexer(args[1])

        # Print the tokens in the token stream
        print("\nTOKEN STREAM")
        for token in result.tokens:
            print(token)

        # Print the contents of the symbol table
        print("\nSYMBOL TABLE")
        for type, symbols in result.symbols.items():
            print(f"\n{type.upper()}(s)")
            print("\nID | CONTENT | TYPE")

            for symbol in symbols:
                print(symbol)
    else:
        print("No file provided for lexing.")

if __name__ == "__main__":
    main()
