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
        in :LEFT_PAREN | :DOT then CALL
        else NONE
        end
      end
    end

    MAXIMUM_ARGUMENTS = 255

    attr_reader :errors

    def initialize
      @errors = []
    end

    # This parses a source string and returns the correspond syntax tree.
    def parse(source)
      parse_program(Lexer.new(source, self))
    end

    # This parses an expression at or above the current level of precedence. It is
    # an implementation of Pratt parsing.
    def parse_expression(tokens, precedence = Precedence::NONE + 1)
      node =
        case tokens.peek
        in { type: :BANG | :MINUS => type, location: slocation } => token
          tokens.next
          expr = parse_expression(tokens, Precedence::UNARY)
          AST::Unary.new(operator: token, node: expr, location: slocation.to(expr.location))
        in { type: :LEFT_PAREN, location: slocation }
          tokens.next
          expr = parse_expression(tokens)
          rparen = consume(tokens, :RIGHT_PAREN, "Expected ')' after expression.")
          AST::Group.new(node: expr, location: slocation.to(rparen.location))
        in { type: :TRUE, location: }
          tokens.next
          AST::Literal.new(value: Type::True.instance, location: location)
        in { type: :FALSE, location: }
          tokens.next
          AST::Literal.new(value: Type::False.instance, location: location)
        in { type: :NIL, location: }
          tokens.next
          AST::Literal.new(value: Type::Nil.instance, location: location)
        in { type: :NUMBER, value:, location: }
          tokens.next
          AST::Literal.new(value: Type::Number.new(value: value), location: location)
        in { type: :STRING, value:, location: }
          tokens.next
          AST::Literal.new(value: Type::String.new(value: value), location: location)
        in { type: :THIS, location: }
          tokens.next
          AST::ThisExpression.new(location: location)
        in { type: :IDENTIFIER, value:, location: }
          tokens.next
          AST::Variable.new(name: value, location: location)
        in token
          errors << Error::SyntaxError.new("Error at #{token.to_value_s}: Expect expression.", token.location)
          tokens.next unless token in { type: :EOF }
          synchronize(tokens)
          return AST::Missing.new(location: token.location)
        end

      while (infix = Precedence.infix_for(tokens.peek)) && (precedence <= infix)
        token = tokens.next
        node =
          case token
          in { type: :EQUAL }
            value = parse_expression(tokens, infix)

            case node
            in AST::Variable
              AST::Assignment.new(variable: node, value: value, location: node.location.to(value.location))
            in AST::GetExpression
              AST::SetExpression.new(object: node.object, name: node.name, value: value, location: node.location.to(value.location))
            in AST::Literal[value: Type::True | Type::False | Type::Nil => value]
              errors << Error::SyntaxError.new("Error at '#{value.to_lox}': Expect variable name.", node.location)
              AST::Assignment.new(variable: node, value: value, location: node.location.to(value.location))
            else
              errors << Error::SyntaxError.new("Error at #{token.to_value_s}: Invalid assignment target.", token.location)
              AST::Assignment.new(variable: node, value: value, location: node.location.to(value.location))
            end
          in { type: :LEFT_PAREN, location: arguments_location }
            arguments = []

            if !(tokens.peek in { type: :RIGHT_PAREN })
              exceeding = nil

              loop do
                exceeding ||= tokens.peek if arguments.length >= MAXIMUM_ARGUMENTS
                arguments << parse_expression(tokens)
                break unless tokens.peek in { type: :COMMA }
                tokens.next
              end

              if exceeding
                errors << Error::SyntaxError.new("Error at #{exceeding.to_value_s}: Can't have more than 255 arguments.", exceeding.location)
              end
            end

            rparen = consume(tokens, :RIGHT_PAREN, "Expect ')' after arguments.")
            synchronize(tokens) if rparen in { type: :MISSING }
            AST::Call.new(callee: node, arguments: arguments, arguments_location: arguments_location, location: node.location.to(rparen.location))
          in { type: :DOT }
            name = consume(tokens, :IDENTIFIER, "Expect property name after '.'.")
            AST::GetExpression.new(object: node, name: name, location: node.location.to(name.location))
          else
            right = parse_expression(tokens, infix + 1)
            AST::Binary.new(left: node, operator: token, right: right, location: node.location.to(right.location))
          end
      end

      node
    end

    private

    def consume(tokens, type, message)
      peeked = tokens.peek

      if peeked in { type: ^type }
        tokens.next
      else
        errors << Error::SyntaxError.new("Error at #{peeked.to_value_s}: #{message}", peeked.location)
        AST::Token.new(type: :MISSING, location: peeked.location, value: nil)
      end
    end

    # A small state machine that accepts an item separated by delimiters until
    # it hits the end.
    # 
    #       +------+
    #   delimiter  |  +-- delimiter --+
    #       | V----+  |               |
    #     +------+ <--+        +---------+
    # --> | item | -- item --> | delimit |
    #     +------+ <-- item -- +---------+
    #        |                  |
    #      ending --------------+
    #        |
    #        V
    #     +-------+
    #     ||final||
    #     +-------+
    #
    def parse_list(tokens, item_type, delimiter_type, ending_type, message)
      state = :item

      loop do
        case [state, tokens.peek]
        in [:item, { type: ^item_type }]
          # Here we were expecting an item and we got an item. We'll yield out
          # the item to the caller, advance the tokens, and move to the delimit
          # state.
          yield tokens.next
          state = :delimit
        in [:item, { type: ^delimiter_type }]
          # Here we were expecting an item and we got a delimiter. We'll yield
          # out a missing token to the caller (which will be returned by the
          # consume method), advance the tokens past the delimiter, and keep the
          # state in the item state.
          yield consume(tokens, item_type, message)
          tokens.next
        in [:item, { type: ^ending_type }]
          # Here we were expecting an item and we got the ending. We'll yield
          # out a missing token to the caller to indicate we were missing an
          # item after the last delimiter, and then return from this method.
          yield consume(tokens, item_type, message)
          return
        in [:delimit, { type: ^item_type } => token]
          # Here we were expecting a delimiter and we got an item type. In that
          # case we'll throw in a missing token for the delimiter and switch
          # back to the item state.
          consume(tokens, delimiter_type, "expected #{delimiter_type}.")
          state = :item
        in [:delimit, { type: ^delimiter_type }]
          # Here we were expecting a delimiter and we got a delimiter. We'll
          # advance past the delimiter and switch to the item state.
          tokens.next
          state = :item
        in [:delimit, { type: ^ending_type }]
          # Here we were expecting a delimiter and we got the ending. That's
          # fine, we can return directly from this method now.
          return
        in [_, token]
          # Here we got something that we don't even handle. In this case we'll
          # add an error, enter panic mode, and attempt to synchronize.
          errors << Error::SyntaxError.new("Error at #{token.to_value_s}: #{message}", token.location)
          synchronize(tokens)
          return
        end
      end
    end

    # This is the synchronization mechanism. If we've found something that we
    # don't explicitly handle, skip forward until we find something that looks
    # right.
    def synchronize(tokens)
      loop do
        if tokens.peek in { type: :EOF | :CLASS | :FOR | :FUN | :IF | :PRINT | :RETURN | :VAR | :WHILE }
          return
        elsif tokens.previous in { type: :SEMICOLON }
          return
        else
          tokens.next
        end
      end
    end

    def parse_program(tokens)
      statements = []
      
      until tokens.peek in { type: :EOF }
        statements << parse_declaration(tokens)
      end

      AST::Program.new(
        statements: statements,
        location: AST::Location.new(start: 0, finish: tokens.peek.location.finish)
      )
    end

    def parse_declaration(tokens)
      case tokens.peek
      in { type: :CLASS }
        parse_class_declaration(tokens)
      in { type: :FUN }
        parse_function(tokens, :function, tokens.next.location)
      in { type: :VAR }
        parse_var_declaration(tokens)
      else
        parse_statement(tokens)
      end
    end

    def parse_class_declaration(tokens)
      keyword = consume(tokens, :CLASS, "Expect 'class'.")
      name = consume(tokens, :IDENTIFIER, "Expect class name.")

      superclass =
        if tokens.peek in { type: :LESS }
          tokens.next
          supername = consume(tokens, :IDENTIFIER, "Expect superclass name.")
          AST::Variable.new(name: supername.value, location: supername.location)
        end

      consume(tokens, :LEFT_BRACE, "Expect '{' before class body.")

      methods = []
      while !(tokens.peek in { type: :RIGHT_BRACE | :EOF })
        methods << parse_function(tokens, :method)
      end

      rbrace = consume(tokens, :RIGHT_BRACE, "Expect '}' after class body.")
      AST::ClassStatement.new(
        name: name,
        superclass: superclass,
        methods: methods,
        location: keyword.location.to(rbrace.location)
      )
    end

    def parse_function(tokens, kind, start_location = nil)
      name = consume(tokens, :IDENTIFIER, "Expected #{kind} name.")
      consume(tokens, :LEFT_PAREN, "Expect '(' after #{kind} name.")

      parameters = []
      if !(tokens.peek in { type: :RIGHT_PAREN })
        exceeding = nil

        parse_list(tokens, :IDENTIFIER, :COMMA, :RIGHT_PAREN, "Expected parameter name.") do |token|
          exceeding ||= tokens.peek if parameters.length >= MAXIMUM_ARGUMENTS
          parameters << token
        end

        if exceeding
          errors << Error::SyntaxError.new("Error at #{exceeding.to_value_s}: Can't have more than 255 parameters.", exceeding.location)
        end
      end

      consume(tokens, :RIGHT_PAREN, "Expected ')' after parameters.")
      body = parse_block_statement(tokens, "Expect '{' before function body.")

      AST::Function.new(
        name: name.value,
        parameters: parameters,
        statements: body.statements,
        location: (start_location || name.location).to(body.location)
      )
    end

    def parse_var_declaration(tokens)
      keyword = consume(tokens, :VAR, "Expect 'var'.")

      if tokens.peek in { type: :FALSE | :NIL | :THIS | :TRUE, location: } => token
        errors << Error::SyntaxError.new("Error at #{token.to_value_s}: Expect variable name.", location)
        synchronize(tokens)
        nil
      else
        identifier = consume(tokens, :IDENTIFIER, "Expect identifier after 'var'.")

        if tokens.peek in { type: :EQUAL }
          tokens.next
          initializer = parse_expression(tokens)
        end

        semicolon = consume(tokens, :SEMICOLON, "Expect ';' after variable declaration.")
      
        AST::VariableDeclaration.new(
          name: identifier.value,
          initializer: initializer,
          location: keyword.location.to(semicolon.location)
        )
      end
    end

    def parse_statement(tokens)
      case tokens.peek
      in { type: :FOR }
        parse_for_statement(tokens)
      in { type: :IF }
        parse_if_statement(tokens)
      in { type: :PRINT }
        parse_print_statement(tokens)
      in { type: :RETURN }
        parse_return_statement(tokens)
      in { type: :WHILE }
        parse_while_statement(tokens)
      in { type: :LEFT_BRACE }
        parse_block_statement(tokens, "Expect '{'.")
      else
        parse_expression_statement(tokens)
      end
    end

    def parse_for_statement(tokens)
      keyword = consume(tokens, :FOR, "Expect 'for'.")
      consume(tokens, :LEFT_PAREN, "Expect '(' after 'for'.")

      initializer =
        case tokens.peek
        in { type: :SEMICOLON }
          tokens.next
          nil
        in { type: :VAR }
          parse_var_declaration(tokens)
        else
          parse_expression_statement(tokens)
        end

      condition = parse_expression(tokens) unless (tokens.peek in { type: :SEMICOLON })
      consume(tokens, :SEMICOLON, "Expect ';' after loop condition.") unless (condition in AST::Missing)

      increment = parse_expression(tokens) unless (tokens.peek in { type: :RIGHT_PAREN })
      body =
        if increment in AST::Missing
          nil
        else
          consume(tokens, :RIGHT_PAREN, "Expect ')' after for clauses.")
          parse_statement(tokens)
        end

      AST::ForStatement.new(
        initializer: initializer,
        condition: condition,
        increment: increment,
        body: body,
        location: keyword.location.to((body || increment || condition || initializer || keyword).location)
      )
    end

    def parse_if_statement(tokens)
      keyword = consume(tokens, :IF, "Expect 'if'.")
      consume(tokens, :LEFT_PAREN, "Expect '(' after 'if'.")

      condition = parse_expression(tokens)
      consume(tokens, :RIGHT_PAREN, "Expect ')' after if condition.")

      then_branch = parse_statement(tokens)
      else_branch =
        if tokens.peek in { type: :ELSE }
          tokens.next
          parse_statement(tokens)
        end

      AST::IfStatement.new(
        condition: condition,
        then_branch: then_branch,
        else_branch: else_branch,
        location: keyword.location.to((else_branch || then_branch).location)
      )
    end

    def parse_print_statement(tokens)
      keyword = consume(tokens, :PRINT, "Expect 'print'.")
      value = parse_expression(tokens)

      semicolon =
        unless value in AST::Missing
          consume(tokens, :SEMICOLON, "Expect ';' after value.")
        end

      AST::PrintStatement.new(
        value: value,
        location: keyword.location.to((semicolon || value).location)
      )
    end

    def parse_return_statement(tokens)
      keyword = consume(tokens, :RETURN, "Expect 'return'.")
      value =
        unless tokens.peek in { type: :SEMICOLON }
          parse_expression(tokens)
        end

      semicolon = consume(tokens, :SEMICOLON, "Expect ';' after return value.")

      AST::ReturnStatement.new(
        value: value,
        location: keyword.location.to(semicolon.location)
      )
    end

    def parse_while_statement(tokens)
      keyword = consume(tokens, :WHILE, "Expect 'while'.")
      consume(tokens, :LEFT_PAREN, "Expect '(' after 'while'.")

      condition = parse_expression(tokens)
      body =
        unless condition in AST::Missing
          consume(tokens, :RIGHT_PAREN, "Expect ')' after condition.")
          body = parse_statement(tokens)
        end

      AST::WhileStatement.new(
        condition: condition,
        body: body,
        location: keyword.location.to(body.location)
      )
    end

    def parse_block_statement(tokens, message)
      lbrace = consume(tokens, :LEFT_BRACE, message)

      statements = []
      statements << parse_declaration(tokens) until tokens.peek in { type: :EOF | :RIGHT_BRACE }
      rbrace = consume(tokens, :RIGHT_BRACE, "Expect '}' after block.")

      AST::BlockStatement.new(
        statements: statements,
        location: lbrace.location.to(rbrace.location)
      )
    end

    def parse_expression_statement(tokens)
      value = parse_expression(tokens)
      semicolon =
        if tokens.previous in { type: :SEMICOLON }
          tokens.previous
        elsif !(value in AST::Missing)
          consume(tokens, :SEMICOLON, "Expect ';' after expression.")
        end

      AST::Expression.new(
        value: value,
        location: value.location.to((semicolon || value).location)
      )
    end
  end
end
