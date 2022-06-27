# frozen_string_literal: true

module Lox
  # This class is responsible for converting a stream of tokens from the input
  # string into an abstract syntax tree.
  class Parser
    # This module contains a list of constants that represent each of the types
    # of precedence for the lox language.
    module Precedence
      NONE = 0
      ASSIGNMENT = 1
      OR = 2
      AND = 3
      EQUALITY = 4
      COMPARISON = 5
      TERM = 6
      FACTOR = 7
      UNARY = 8
      CALL = 9
      PRIMARY = 10

      # This returns the precedence of the type of the given token when it is
      # found in an infix position.
      def self.infix_for(token)
        case token.type
        in :EQUAL then ASSIGNMENT
        in :OR then OR
        in :AND then AND
        in :BANG_EQUAL | :EQUAL_EQUAL then EQUALITY
        in :GREATER | :GREATER_EQUAL | :LESS | :LESS_EQUAL then COMPARISON
        in :MINUS | :PLUS then TERM
        in :SLASH | :STAR then FACTOR
        else NONE
        end
      end
    end

    # This parses a source string and returns the correspond syntax tree.
    def parse(source)
      parse_program(Lexer.new(source))
    end

    # This parses an expression at or above the current level of precedence. It is
    # an implementation of Pratt parsing.
    def parse_expression(tokens, precedence = Precedence::NONE + 1)
      node =
        case tokens.next
        in { type: :BANG | :MINUS => type, location: slocation } => token
          expr = parse_expression(tokens, Precedence::UNARY)
          AST::Unary.new(operator: token, node: expr, location: slocation.to(expr.location))
        in { type: :LEFT_PAREN, location: slocation }
          expr = parse_expression(tokens)
          tokens.next => { type: :RIGHT_PAREN, location: elocation }
          AST::Group.new(node: expr, location: slocation.to(elocation))
        in { type: :TRUE, location: }
          AST::Literal.new(value: Type::True.instance, location: location)
        in { type: :FALSE, location: }
          AST::Literal.new(value: Type::False.instance, location: location)
        in { type: :NIL, location: }
          AST::Literal.new(value: Type::Nil.instance, location: location)
        in { type: :NUMBER, value:, location: }
          AST::Literal.new(value: Type::Number.new(value: value), location: location)
        in { type: :STRING, value:, location: }
          AST::Literal.new(value: Type::String.new(value: value), location: location)
        in { type: :IDENTIFIER, value:, location: }
          AST::Variable.new(name: value, location: location)
        end

      while (infix = Precedence.infix_for(tokens.peek)) && (precedence <= infix)
        token = tokens.next
        node =
          if token.type == :EQUAL
            case node
            in AST::Variable
              value = parse_expression(tokens, infix)
              AST::Assignment.new(variable: node, value: value, location: node.location.to(value.location))
            in AST::Literal[value: Type::True | Type::False | Type::Nil => value]
              raise Error::SyntaxError.new("Error at '#{value.to_lox}': Expect variable name.", node.location)
            else
              raise Error::SyntaxError.new("Error at '#{token.value}': Invalid assignment target.", token.location)
            end
          else
            right = parse_expression(tokens, infix + 1)
            AST::Binary.new(left: node, operator: token, right: right, location: node.location.to(right.location))
          end
      end

      node
    end

    private

    def parse_program(tokens)
      statements = []
      statements << parse_declaration(tokens) until tokens.peek in { type: :EOF }

      AST::Program.new(
        statements: statements,
        location: AST::Location.new(start: 0, finish: tokens.peek.location.finish)
      )
    end

    def parse_declaration(tokens)
      case tokens.peek
      in { type: :VAR }
        parse_var_declaration(tokens)
      else
        parse_statement(tokens)
      end
    end

    def parse_var_declaration(tokens)
      keyword = tokens.consume(:VAR)
      identifier = tokens.consume(:IDENTIFIER)

      initializer = parse_expression(tokens) if tokens.match(:EQUAL)
      semicolon = tokens.consume(:SEMICOLON)
    
      AST::VariableDeclaration.new(
        name: identifier.value,
        initializer: initializer,
        location: keyword.location.to(semicolon.location)
      )
    end

    def parse_statement(tokens)
      case tokens.peek
      in { type: :PRINT }
        parse_print_statement(tokens)
      in { type: :LEFT_BRACE }
        parse_block_statement(tokens)
      else
        parse_expression_statement(tokens)
      end
    end

    def parse_print_statement(tokens)
      keyword = tokens.consume(:PRINT)
      value = parse_expression(tokens)
      semicolon = tokens.consume(:SEMICOLON)

      AST::PrintStatement.new(
        value: value,
        location: keyword.location.to(semicolon.location)
      )
    end

    def parse_block_statement(tokens)
      lbrace = tokens.consume(:LEFT_BRACE)

      statements = []
      statements << parse_declaration(tokens) until tokens.peek in { type: :EOF | :RIGHT_BRACE }
      rbrace = tokens.consume(:RIGHT_BRACE)

      AST::BlockStatement.new(
        statements: statements,
        location: lbrace.location.to(rbrace.location)
      )
    end

    def parse_expression_statement(tokens)
      value = parse_expression(tokens)
      semicolon = tokens.consume(:SEMICOLON)

      AST::Expression.new(
        value: value,
        location: value.location.to(semicolon.location)
      )
    end
  end
end
