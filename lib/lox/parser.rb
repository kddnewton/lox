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

    attr_reader :builder, :errors

    def initialize(builder)
      @builder = builder
      @errors = []
    end

    # This parses a source string and returns the correspond syntax tree.
    def parse(source)
      parse_program(Lexer.new(source, self)).node
    end

    # This parses an expression at or above the current level of precedence. It is
    # an implementation of Pratt parsing.
    def parse_expression(tokens, precedence = Precedence::NONE + 1)
      node =
        case tokens.peek
        in { type: :BANG | :MINUS => type, location: slocation } => token
          tokens.next
          expr = parse_expression(tokens, Precedence::UNARY)
          dispatch_unary_expression(operator: token, node: expr.node, location: slocation.to(expr.location))
        in { type: :LEFT_PAREN, location: slocation }
          tokens.next
          expr = parse_expression(tokens)
          elocation =
            if expr.node in { type: :missing }
              expr.location
            else
              consume(tokens, :RIGHT_PAREN, "Expected ')' after expression.").location
            end

          dispatch_group(node: expr.node, location: slocation.to(elocation))
        in { type: :TRUE, location: }
          tokens.next
          dispatch_true(location:)
        in { type: :FALSE, location: }
          tokens.next
          dispatch_false(location:)
        in { type: :NIL, location: }
          tokens.next
          dispatch_nil(location:)
        in { type: :NUMBER, value:, location: }
          tokens.next
          dispatch_number(value:, location:)
        in { type: :STRING, value:, location: }
          tokens.next
          dispatch_string(value:, location:)
        in { type: :THIS, location: }
          tokens.next
          dispatch_this_expression(location:)
        in { type: :IDENTIFIER, value: name, location: }
          tokens.next
          dispatch_variable(name:, next_token: tokens.peek, location:)
        in { type: :SUPER, location: }
          tokens.next
          if consume(tokens, :DOT, "Expect '.' after 'super'.") in { type: :MISSING }
            synchronize(tokens)
            return dispatch_missing(location: location)
          end

          method = consume(tokens, :IDENTIFIER, "Expect superclass method name.")
          dispatch_super_expression(method: method.value, location: location.to(method.location))
        in token
          errors << Error::SyntaxError.new("Error at #{token.to_value_s}: Expect expression.", token.location)
          tokens.next unless token in { type: :EOF }
          synchronize(tokens)
          return dispatch_missing(location: token.location)
        end

      while (infix = Precedence.infix_for(tokens.peek)) && (precedence <= infix)
        token = tokens.next
        node =
          case token
          in { type: :EQUAL }
            value = parse_expression(tokens, infix)

            case node.type
            in :get_expression
              dispatch_set_expression(object: node.node.object, name: node.node.name, value: value.node, location: node.location.to(value.location))
            in :variable
              dispatch_assignment(variable: node.node, value: value.node, location: node.location.to(value.location))
            in :true | :false | :nil
              errors << Error::SyntaxError.new("Error at '#{node.value.to_lox}': Expect variable name.", node.location)
              dispatch_assignment(variable: node.node, value: value.node, location: node.location.to(value.location))
            else
              errors << Error::SyntaxError.new("Error at #{token.to_value_s}: Invalid assignment target.", token.location)
              dispatch_assignment(variable: node.node, value: value.node, location: node.location.to(value.location))
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
            dispatch_call(callee: node.node, arguments: arguments.map(&:node), arguments_location:, location: node.location.to(rparen.location))
          in { type: :DOT }
            name = consume(tokens, :IDENTIFIER, "Expect property name after '.'.")
            dispatch_get_expression(object: node.node, name: name, location: node.location.to(name.location))
          else
            right = parse_expression(tokens, infix + 1)
            dispatch_binary(left: node.node, operator: token, right: right.node, location: node.location.to(right.location))
          end
      end

      node
    end

    private

    #---------------------------------------------------------------------------
    # Parsing helpers
    #---------------------------------------------------------------------------

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
    def synchronize(tokens, break_on = nil)
      loop do
        if tokens.peek in { type: :EOF | :CLASS | :FOR | :FUN | :IF | :PRINT | :RETURN | :VAR | :WHILE }
          return
        elsif break_on && tokens.peek in { type: ^break_on }
          return
        elsif tokens.previous in { type: :SEMICOLON }
          return
        else
          tokens.next
        end
      end
    end

    #---------------------------------------------------------------------------
    # Parse node types and call dispatch methods
    #---------------------------------------------------------------------------

    def parse_program(tokens)
      statements = []
      
      until tokens.peek in { type: :EOF }
        statements << parse_declaration(tokens)
      end

      dispatch_program(
        statements: statements.compact.map(&:node),
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

    # class Name {}
    def parse_class_declaration(tokens)
      keyword = consume(tokens, :CLASS, "Expect 'class'.")
      name = consume(tokens, :IDENTIFIER, "Expect class name.")

      superclass =
        if tokens.peek in { type: :LESS }
          tokens.next
          supername = consume(tokens, :IDENTIFIER, "Expect superclass name.")

          if supername in { type: :MISSING }
            synchronize(tokens, :LEFT_BRACE)
            nil
          else
            dispatch_variable(name: supername.value, next_token: tokens.peek, location: supername.location)
          end
        end

      consume(tokens, :LEFT_BRACE, "Expect '{' before class body.")

      methods = []
      while !(tokens.peek in { type: :RIGHT_BRACE | :EOF })
        methods << parse_function(tokens, :method)
      end

      rbrace = consume(tokens, :RIGHT_BRACE, "Expect '}' after class body.")
      dispatch_class_statement(
        name: name,
        superclass: superclass&.node,
        methods: methods.map(&:node),
        location: keyword.location.to(rbrace.location)
      )
    end

    # name(param1, param2) {
    #   body
    # }
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

      dispatch_function(
        name: name.value,
        parameters: parameters,
        statements: body.node.statements,
        location: (start_location || name.location).to(body.location)
      )
    end

    # var a = 1;
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
        dispatch_variable_declaration(
          name: identifier.value,
          initializer: initializer&.node,
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

    # for (initializer; condition; increment) {
    #   body
    # }
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
      consume(tokens, :SEMICOLON, "Expect ';' after loop condition.") unless (condition in { type: :missing })

      increment = parse_expression(tokens) unless (tokens.peek in { type: :RIGHT_PAREN })
      body =
        unless increment in { type: :missing }
          consume(tokens, :RIGHT_PAREN, "Expect ')' after for clauses.")
          parse_statement(tokens)
        end

      dispatch_for_statement(
        initializer: initializer&.node,
        condition: condition&.node,
        increment: increment&.node,
        body: body&.node,
        location: keyword.location.to((body || increment || condition || initializer || keyword).location)
      )
    end

    # if condition
    #   then_branch
    # else
    #   else_branch
    # end
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

      dispatch_if_statement(
        condition: condition.node,
        then_branch: then_branch.node,
        else_branch: else_branch&.node,
        location: keyword.location.to((else_branch || then_branch).location)
      )
    end

    # print value;
    def parse_print_statement(tokens)
      keyword = consume(tokens, :PRINT, "Expect 'print'.")
      value = parse_expression(tokens)

      semicolon =
        unless value in { type: :missing }
          consume(tokens, :SEMICOLON, "Expect ';' after value.")
        end

      dispatch_print_statement(
        value: value.node,
        location: keyword.location.to((semicolon || value).location)
      )
    end

    # return value;
    def parse_return_statement(tokens)
      keyword = consume(tokens, :RETURN, "Expect 'return'.")
      value =
        unless tokens.peek in { type: :SEMICOLON }
          parse_expression(tokens)
        end

      semicolon = consume(tokens, :SEMICOLON, "Expect ';' after return value.")

      dispatch_return_statement(
        value: value&.node,
        location: keyword.location.to(semicolon.location)
      )
    end

    # while condition
    #   statement
    # end
    def parse_while_statement(tokens)
      keyword = consume(tokens, :WHILE, "Expect 'while'.")
      consume(tokens, :LEFT_PAREN, "Expect '(' after 'while'.")

      condition = parse_expression(tokens)
      body =
        unless condition in { type: :missing }
          consume(tokens, :RIGHT_PAREN, "Expect ')' after condition.")
          parse_statement(tokens)
        end

      dispatch_while_statement(
        condition: condition.node,
        body: body&.node,
        location: keyword.location.to((body || condition).location)
      )
    end

    # {}
    def parse_block_statement(tokens, message)
      lbrace = consume(tokens, :LEFT_BRACE, message)

      statements = []
      statements << parse_declaration(tokens) until tokens.peek in { type: :EOF | :RIGHT_BRACE }
      rbrace = consume(tokens, :RIGHT_BRACE, "Expect '}' after block.")

      dispatch_block_statement(
        statements: statements.map(&:node),
        location: lbrace.location.to(rbrace.location)
      )
    end

    # 1 + 1;
    def parse_expression_statement(tokens)
      value = parse_expression(tokens)
      semicolon =
        if tokens.previous in { type: :SEMICOLON }
          tokens.previous
        elsif !(value in { type: :missing })
          consume(tokens, :SEMICOLON, "Expect ';' after expression.")
        end

      dispatch_expression_statement(
        value: value.node,
        location: value.location.to((semicolon || value).location)
      )
    end

    #---------------------------------------------------------------------------
    # Dispatch nodes to builder, return values we can work with up the tree.
    #---------------------------------------------------------------------------

    class DispatchResult
      attr_reader :type, :node, :location

      def initialize(type:, node:, location:)
        @type = type
        @node = node
        @location = location
      end

      def deconstruct_keys(keys)
        { type:, node:, location: }
      end
    end

    def dispatch_assignment(variable:, value:, location:)
      node = builder.on_assignment(variable:, value:, location:)
      DispatchResult.new(type: :assignment, node:, location:)
    end

    def dispatch_binary(left:, operator:, right:, location:)
      node = builder.on_binary(left:, operator:, right:, location:)
      DispatchResult.new(type: :binary, node:, location:)
    end

    def dispatch_block_statement(statements:, location:)
      node = builder.on_block_statement(statements:, location:)
      DispatchResult.new(type: :block_statement, node:, location:)
    end

    def dispatch_call(callee:, arguments:, arguments_location:, location:)
      node = builder.on_call(callee:, arguments:, arguments_location:, location:)
      DispatchResult.new(type: :call, node:, location:)
    end

    def dispatch_class_statement(name:, superclass:, methods:, location:)
      node = builder.on_class_statement(name:, superclass:, methods:, location:)
      DispatchResult.new(type: :class_statement, node:, location:)
    end

    def dispatch_expression_statement(value:, location:)
      node = builder.on_expression_statement(value:, location:)
      DispatchResult.new(type: :expression_statement, node:, location:)
    end

    def dispatch_function(name:, parameters:, statements:, location:)
      node = builder.on_function(name:, parameters:, statements:, location:)
      DispatchResult.new(type: :function, node:, location:)
    end

    def dispatch_number(value:, location:)
      node = builder.on_number(value:, location:)
      DispatchResult.new(type: :number, node:, location:)
    end

    def dispatch_false(location:)
      node = builder.on_false(location:)
      DispatchResult.new(type: :false, node:, location:)
    end

    def dispatch_for_statement(initializer:, condition:, increment:, body:, location:)
      node = builder.on_for_statement(initializer:, condition:, increment:, body:, location:)
      DispatchResult.new(type: :for_statement, node:, location:)
    end

    def dispatch_get_expression(object:, name:, location:)
      node = builder.on_get_expression(object:, name:, location:)
      DispatchResult.new(type: :get_expression, node:, location:)
    end

    def dispatch_group(node:, location:)
      node = builder.on_group(node:, location:)
      DispatchResult.new(type: :group, node:, location:)
    end

    def dispatch_if_statement(condition:, then_branch:, else_branch:, location:)
      node = builder.on_if_statement(condition:, then_branch:, else_branch:, location:)
      DispatchResult.new(type: :if_statement, node:, location:)
    end

    def dispatch_missing(location:)
      node = builder.on_missing(location:)
      DispatchResult.new(type: :missing, node:, location:)
    end

    def dispatch_nil(location:)
      node = builder.on_nil(location:)
      DispatchResult.new(type: :nil, node:, location:)
    end

    def dispatch_print_statement(value:, location:)
      node = builder.on_print_statement(value:, location:)
      DispatchResult.new(type: :print_statement, node:, location:)
    end

    def dispatch_program(statements:, location:)
      node = builder.on_program(statements:, location:)
      DispatchResult.new(type: :program, node:, location:)
    end

    def dispatch_return_statement(value:, location:)
      node = builder.on_return_statement(value:, location:)
      DispatchResult.new(type: :return_statement, node:, location:)
    end

    def dispatch_set_expression(object:, name:, value:, location:)
      node = builder.on_set_expression(object:, name:, value:, location:)
      DispatchResult.new(type: :set_expression, node:, location:)
    end

    def dispatch_string(value:, location:)
      node = builder.on_string(value:, location:)
      DispatchResult.new(type: :string, node:, location:)
    end

    def dispatch_super_expression(method:, location:)
      node = builder.on_super_expression(method:, location:)
      DispatchResult.new(type: :super_expression, node:, location:)
    end

    def dispatch_this_expression(location:)
      node = builder.on_this_expression(location:)
      DispatchResult.new(type: :this_expression, node:, location:)
    end

    def dispatch_true(location:)
      node = builder.on_true(location:)
      DispatchResult.new(type: :true, node:, location:)
    end

    def dispatch_unary_expression(operator:, node:, location:)
      node = builder.on_unary_expression(operator:, node:, location:)
      DispatchResult.new(type: :unary_expression, node:, location:)
    end

    def dispatch_variable(name:, next_token:, location:)
      node = builder.on_variable(name:, next_token:, location:)
      DispatchResult.new(type: :variable, node:, location:)
    end

    def dispatch_variable_declaration(name:, initializer:, location:)
      node = builder.on_variable_declaration(name:, initializer:, location:)
      DispatchResult.new(type: :variable_declaration, node:, location:)
    end

    def dispatch_while_statement(condition:, body:, location:)
      node = builder.on_while_statement(condition:, body:, location:)
      DispatchResult.new(type: :while_statement, node:, location:)
    end
  end
end
