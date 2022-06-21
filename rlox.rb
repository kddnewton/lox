#!/usr/bin/env ruby
# frozen_string_literal: true

# The top-level class that provides all of the functionality of the language.
class Lox
  # A list of strings that represent all of the keywords in the language.
  KEYWORDS = %w[and class else false fun for if nil or print return super this true var while]

  # A list of the operators in the language keyed by their string representation
  # where the value is the name of the token that represents the operator.
  OPERATORS = {
    "(" => :LEFT_PAREN, ")" => :RIGHT_PAREN, "{" => :LEFT_BRACE, "}" => :RIGHT_BRACE, "," => :COMMA,
    "." => :DOT, "-" => :MINUS, "+" => :PLUS, ";" => :SEMICOLON, "/" => :SLASH, "*" => :STAR,
    "!" => :BANG, "!=" => :BANG_EQUAL, "=" => :EQUAL, "==" => :EQUAL_EQUAL, ">" => :GREATER,
    ">=" => :GREATER_EQUAL, "<" => :LESS, "<=" => :LESS_EQUAL
  }

  # In debug methods, it's necessary to go from the operator to the value that
  # it represents in source.
  OPERATORS_NAMES = OPERATORS.invert

  # This module contains the definitions of any of the various errors that can
  # occur when parsing and evaluating a lox program.
  module Error
    # This is the parent class of any kind of error in our program.
    class Error < StandardError
    end

    # This error occurs during parsing when a token is not recognized or not
    # properly formed.
    class SyntaxError < Error
    end

    # This is an error that occurs when a lox error is raised.
    class RuntimeError < Error
      attr_reader :location

      def initialize(message, location = nil)
        super(message)
        @location = location
      end
    end
  end

  # This module contains the various types that are used in lox. In general we
  # avoid using native Ruby types because we can end up calling methods on them
  # that we don't want to support.
  module Type
    # The way lox works according to the book, you can call all of the operators
    # on any kind of value. So here we're going to prepare base implementations
    # that just raise.
    class Object
      def unwrap_number(message = "Operands must be numbers.")
        raise Error::RuntimeError, message unless (self in Number)
        value
      end

      def unwrap_string(message = "Operands must be two numbers or two strings.")
        raise Error::RuntimeError, message unless (self in String)
        value
      end

      def -@() = raise Error::RuntimeError, "Operand must be a number."
      def +(other) = raise Error::RuntimeError, "Operands must be two numbers or two strings."
      def -(other) = raise Error::RuntimeError, "Operands must be numbers."
      def *(other) = raise Error::RuntimeError, "Operands must be numbers."
      def /(other) = raise Error::RuntimeError, "Operands must be numbers."
      def <(other) = raise Error::RuntimeError, "Operands must be numbers."
      def <=(other) = raise Error::RuntimeError, "Operands must be numbers."
      def >(other) = raise Error::RuntimeError, "Operands must be numbers."
      def >=(other) = raise Error::RuntimeError, "Operands must be numbers."

      def !=(other)
        !(self == other)
      end
    end

    # This represents the false value.
    class False < Object
      def self.instance
        @instance ||= new
      end

      def to_s
        "false"
      end

      #-------------------------------------------------------------------------
      # Runtime methods
      #-------------------------------------------------------------------------

      def !@
        True.instance
      end

      def ==(other)
        Type.boolean((other in False))
      end
    end

    # This represents the nil value.
    class Nil < Object
      def self.instance
        @instance ||= new
      end

      def to_s
        "nil"
      end

      #-------------------------------------------------------------------------
      # Runtime methods
      #-------------------------------------------------------------------------

      def !@
        True.instance
      end

      def ==(other)
        Type.boolean((other in Nil))
      end
    end

    # This represents a number. Lox makes no delineation between integers/
    # floats/bignums/etc.
    class Number < Object
      attr_reader :value

      def initialize(value:)
        @value = value
      end

      def deconstruct_keys(keys)
        { value: value }
      end

      def to_s
        value % 1 == 0 ? value.to_i.to_s : value.to_s
      end

      #-------------------------------------------------------------------------
      # Runtime methods
      #-------------------------------------------------------------------------

      def -@
        Number.new(value: -value)
      end

      def +(other) = Number.new(value: value + other.unwrap_number("Operands must be two numbers or two strings."))
      def -(other) = Number.new(value: value - other.unwrap_number)
      def *(other) = Number.new(value: value * other.unwrap_number)
      def /(other) = Number.new(value: value / other.unwrap_number)

      def <(other) = Type.boolean(value < other.unwrap_number)
      def <=(other) = Type.boolean(value <= other.unwrap_number)
      def >(other) = Type.boolean(value > other.unwrap_number)
      def >=(other) = Type.boolean(value >= other.unwrap_number)

      def ==(other) = Type.boolean((other in Number[value: ^(value)]))
    end

    # This represents a string.
    class String < Object
      attr_reader :value

      def initialize(value:)
        @value = value
      end

      def deconstruct_keys(keys)
        { value: value }
      end

      def to_s
        value
      end

      #-------------------------------------------------------------------------
      # Runtime methods
      #-------------------------------------------------------------------------

      def +(other)
        String.new(value: value + other.unwrap_string)
      end

      def ==(other)
        Type.boolean((other in String[value: ^(value)]))
      end
    end

    # This represents the true value.
    class True < Object
      def self.instance
        @instance ||= new
      end

      def to_s
        "true"
      end

      #-------------------------------------------------------------------------
      # Runtime methods
      #-------------------------------------------------------------------------

      def !@
        False.instance
      end

      def ==(other)
        Type.boolean((other in True))
      end
    end

    # A convenient shortcut to turn a Ruby boolean value into a lox boolean
    # value.
    def self.boolean(value)
      value ? True.instance : False.instance
    end
  end

  # This module knows how to walk through the syntax tree.
  module Visitor
    # A class that knows how to walk down the syntax tree.
    class BaseVisitor
      def visit(node)
        node&.accept(self)
      end

      def visit_all(nodes)
        nodes.map { |node| visit(node) }
      end

      def visit_child_nodes(node)
        visit_all(node.child_nodes)
      end

      # Visit an Assignment node.
      alias visit_assignment visit_child_nodes

      # Visit a Binary node.
      alias visit_binary visit_child_nodes

      # Visit a BlockStatement node.
      alias visit_block_statement visit_child_nodes

      # Visit an Expression node.
      alias visit_expression visit_child_nodes

      # Visit a Group node.
      alias visit_group visit_child_nodes

      # Visit a Literal node.
      alias visit_literal visit_child_nodes

      # Visit a Print node.
      alias visit_print_statement visit_child_nodes

      # Visit a Program node.
      alias visit_program visit_child_nodes

      # Visit a Unary node.
      alias visit_unary visit_child_nodes

      # Visit a Variable node.
      alias visit_variable visit_child_nodes

      # Visit a VariableDeclaration node.
      alias visit_variable_declaration visit_child_nodes
    end

    # This is a visitor that will print the tree to a set of s-expressions that
    # match what the book's test suite expects.
    class DebugVisitor < BaseVisitor
      # Visit a Binary node.
      def visit_binary(node)
        "(#{OPERATORS_NAMES[node.operator.type]} #{visit(node.left)} #{visit(node.right)})"
      end

      # Visit a Group node.
      def visit_group(node)
        "(group #{visit(node.node)})"
      end

      # Visit a Literal node.
      def visit_literal(node)
        case node.value
        in Type::True then "true"
        in Type::False then "false"
        in Type::Nil then "nil"
        in Type::Number[value:] then value.inspect
        in Type::String[value:] then value.inspect
        end
      end

      # Visit a Unary node.
      def visit_unary(node)
        "(#{OPERATORS_NAMES[node.operator.type]} #{visit(node.node)})"
      end
    end

    # This is a visitor that will walk the tree and evaluate it.
    class EvaluateVisitor < BaseVisitor
      class Environment
        attr_reader :parent, :variables

        def initialize(parent)
          @parent = parent
          @variables = {}
        end

        def declare(node, value)
          variables[node.name] = value
        end

        def fetch(node)
          if variables.key?(node.name)
            variables[node.name]
          elsif parent
            parent.fetch(node)
          else
            raise Error::RuntimeError.new("Undefined variable '#{node.name}'.", node.location)
          end
        end

        def assign(node, value)
          if variables.key?(node.name)
            variables[node.name] = value
          elsif parent
            parent.assign(node, value)
          else
            raise Error::RuntimeError.new("Undefined variable '#{node.name}'.", node.location)
          end
        end
      end

      attr_reader :environment

      def initialize
        @environment = Environment.new(nil)
      end

      # Visit an Assignment node.
      def visit_assignment(node)
        environment.assign(node.variable, visit(node.value))
      end

      # Visit a Binary node.
      def visit_binary(node)
        case node.operator.type
        in :BANG_EQUAL then visit(node.left) != visit(node.right)
        in :EQUAL_EQUAL then visit(node.left) == visit(node.right)
        in :GREATER then visit(node.left) > visit(node.right)
        in :GREATER_EQUAL then visit(node.left) >= visit(node.right)
        in :LESS then visit(node.left) < visit(node.right)
        in :LESS_EQUAL then visit(node.left) <= visit(node.right)
        in :MINUS then visit(node.left) - visit(node.right)
        in :PLUS then visit(node.left) + visit(node.right)
        in :SLASH then visit(node.left) / visit(node.right)
        in :STAR then visit(node.left) * visit(node.right)
        end
      rescue Error::RuntimeError => error
        raise Error::RuntimeError.new(error.message, node.operator.location)
      end

      # Visit a BlockStatement node.
      def visit_block_statement(node)
        parent = environment
        @environment = Environment.new(parent)
        
        begin
          visit_all(node.statements)
        ensure
          @environment = parent
        end
      end

      # Visit an Expression node.
      def visit_expression(node)
        visit(node.value)
      end

      # Visit a Group node.
      def visit_group(node)
        visit(node.node)
      end

      # Visit a Literal node.
      def visit_literal(node)
        node.value
      end

      # Visit a Print node.
      def visit_print_statement(node)
        puts visit(node.value)
      end

      # Visit a Program node.
      def visit_program(node)
        visit_all(node.statements)
        nil
      end

      # Visit a Unary node.
      def visit_unary(node)
        case node.operator.type
        in :BANG then !visit(node.node)
        in :MINUS then -visit(node.node)
        end
      rescue Error::RuntimeError => error
        raise Error::RuntimeError.new(error.message, node.operator.location)
      end

      # Visit a Variable node.
      def visit_variable(node)
        environment.fetch(node)
      end

      # Visit a VariableDeclaration node.
      def visit_variable_declaration(node)
        environment.declare(node, node.initializer ? visit(node.initializer) : nil)
      end
    end
  end

  # This module contains the definitions of the nodes in the syntax tree, as
  # well as the methods to walk over them.
  module AST
    # This represents a location in the source string. It is used to tell nodes
    # where they are so we can have accurate error reporting.
    class Location
      attr_reader :start, :finish

      def initialize(start:, finish:)
        @start = start
        @finish = finish
      end

      def to(other)
        Location.new(start: start, finish: other.finish)
      end

      def range
        start...finish
      end
    end

    # A token is a representation of a single lexical unit in the source code.
    # It is consumed and discarded after parsing.
    class Token
      attr_reader :type, :value, :location

      def initialize(type:, value:, index:, length:)
        @type = type
        @value = value
        @location = Location.new(start: index, finish: index + length)
      end

      def deconstruct_keys(keys)
        { type: type, value: value, location: location }
      end
    end

    # This represents assigning to a variable.
    class Assignment
      attr_reader :variable, :value, :location

      def initialize(variable:, value:, location:)
        @variable = variable
        @value = value
        @location = location
      end

      def accept(visitor)
        visitor.visit_assignment(self)
      end

      def child_nodes
        [variable, value]
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { variable: variable, value: value, location: location }
      end
    end

    # A binary node is a node that represents calling a binary operator between
    # two other nodes in the source. It contains an operator and both child
    # nodes.
    class Binary
      attr_reader :left, :operator, :right, :location

      def initialize(left:, operator:, right:, location:)
        @left = left
        @operator = operator
        @right = right
        @location = location
      end

      def accept(visitor)
        visitor.visit_binary(self)
      end

      def child_nodes
        [left, right]
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { left: left, operator: operator, right: right, location: location }
      end
    end

    # This represents a block wrapped by braces.
    class BlockStatement
      attr_reader :statements, :location

      def initialize(statements:, location:)
        @statements = statements
        @location = location
      end

      def accept(visitor)
        visitor.visit_block_statement(self)
      end

      def child_nodes
        statements
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { statements: statements, location: location }
      end
    end

    # This represents an expression to be executed.
    class Expression
      attr_reader :value, :location

      def initialize(value:, location:)
        @value = value
        @location = location
      end

      def accept(visitor)
        visitor.visit_expression(self)
      end

      def child_nodes
        [value]
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { value: value, location: location }
      end
    end

    # A group is a node that holds another node. It is used to represent the use
    # of parentheses in the source code.
    class Group
      attr_reader :node, :location

      def initialize(node:, location:)
        @node = node
        @location = location
      end

      def accept(visitor)
        visitor.visit_group(self)
      end

      def child_nodes
        [node]
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { node: node, location: location }
      end
    end

    # A literal is a node that holds a value from the Type module.
    class Literal
      attr_reader :value, :location

      def initialize(value:, location:)
        @value = value
        @location = location
      end

      def accept(visitor)
        visitor.visit_literal(self)
      end

      def child_nodes
        []
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { value: value, location: location }
      end
    end

    # This represents a print statement.
    class PrintStatement
      attr_reader :value, :location

      def initialize(value:, location:)
        @value = value
        @location = location
      end

      def accept(visitor)
        visitor.visit_print_statement(self)
      end

      def child_nodes
        [value]
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { value: value, location: location }
      end
    end

    # This is the top-level node of the whole tree.
    class Program
      attr_reader :statements, :location

      def initialize(statements:, location:)
        @statements = statements
        @location = location
      end

      def accept(visitor)
        visitor.visit_program(self)
      end

      def child_nodes
        statements
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { statements: statements, location: location }
      end
    end

    # A unary node is a node that represents calling a unary operator on another
    # node. It contains an operator and the child node.
    class Unary
      attr_reader :operator, :node, :location

      def initialize(operator:, node:, location:)
        @operator = operator
        @node = node
        @location = location
      end

      def accept(visitor)
        visitor.visit_unary(self)
      end

      def child_nodes
        [node]
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { operator: operator, node: node, location: location }
      end
    end

    # This represents the use of a variable.
    class Variable
      attr_reader :name, :location

      def initialize(name:, location:)
        @name = name
        @location = location
      end

      def accept(visitor)
        visitor.visit_variable(self)
      end

      def child_nodes
        []
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { name: name, location: location }
      end
    end

    # This represents a variable declaration.
    class VariableDeclaration
      attr_reader :name, :initializer, :location

      def initialize(name:, initializer:, location:)
        @name = name
        @initializer = initializer
        @location = location
      end

      def accept(visitor)
        visitor.visit_variable_declaration(self)
      end

      def child_nodes
        [initializer]
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { name: name, initializer: initializer, location: location }
      end
    end
  end

  # This module contains a list of constants that represent each of the types of
  # precedence for the lox language.
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

  # This takes a source string and converts it into an enumerator that will
  # yield out one token at a time.
  def tokens(source)
    Enumerator.new do |enum|
      index = 0

      while index < source.length
        case source[index..]
        in /\A(\/\/[^\n]*|\s+)/
          # skip whitespace and comments
        in /\A([(){},\.\-+;*\/]|[!=><]=?)/
          type = OPERATORS[$&]
          enum << AST::Token.new(type: type, index: index, length: $&.length, value: nil)
        in /\A"([^"]*)"/
          enum << AST::Token.new(type: :STRING, index: index, length: $&.length, value: $&[1...-1])
        in /\A\d+(\.\d+)?/
          enum << AST::Token.new(type: :NUMBER, index: index, length: $&.length, value: $&.to_f)
        in /\A[a-z_][A-Za-z0-9_]*/
          type = KEYWORDS.include?($&) ? $&.upcase.to_sym : :IDENTIFIER
          value = type == :IDENTIFIER ? $& : nil
          enum << AST::Token.new(type: type, index: index, length: $&.length, value: value)
        end

        index += $&.length
      end

      enum << AST::Token.new(type: :EOF, index: index, length: 0, value: nil)
    end
  end

  # This parses a source string and returns the correspond syntax tree.
  def parse(source)
    parse_program(tokens(source))
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
        if token.type != :EQUAL
          right = parse_expression(tokens, infix + 1)
          AST::Binary.new(left: node, operator: token, right: right, location: node.location.to(right.location))
        elsif !(node in AST::Variable)
          raise Error::SyntaxError.new("Invalid assignment target.", token.location)
        else
          value = parse_expression(tokens, infix)
          AST::Assignment.new(variable: node, value: value, location: node.location.to(value.location))
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
    tokens.next => { type: :VAR }
    tokens.next => { type: :IDENTIFIER, value: name, location: slocation }

    if tokens.peek in { type: :EQUAL }
      tokens.next
      initializer = parse_expression(tokens)
    end

    tokens.next => { type: :SEMICOLON, location: elocation }
  
    AST::VariableDeclaration.new(name: name, initializer: initializer, location: slocation.to(elocation))
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
    tokens.next => { type: :PRINT, location: slocation }
    value = parse_expression(tokens)
    tokens.next => { type: :SEMICOLON, location: elocation }

    AST::PrintStatement.new(value: value, location: slocation.to(elocation))
  end

  def parse_block_statement(tokens)
    tokens.next => { type: :LEFT_BRACE, location: slocation }

    statements = []
    statements << parse_declaration(tokens) until tokens.peek in { type: :EOF | :RIGHT_BRACE }
    tokens.next => { type: :RIGHT_BRACE, location: elocation }

    AST::BlockStatement.new(statements: statements, location: slocation.to(elocation))
  end

  def parse_expression_statement(tokens)
    value = parse_expression(tokens)
    tokens.next => { type: :SEMICOLON, location: elocation }

    AST::Expression.new(value: value, location: value.location.to(elocation))
  end
end

source = File.read(ARGV.first)
suite = ENV.fetch("SUITE", "chap08_statements")

case suite
in "chap04_scanning"
  Lox.new.tokens(source).each do |token|
    value =
      case token
      in { value: nil } | { type: :IDENTIFIER }
        "null"
      in { value: }
        value
      end

    puts "#{token.type} #{source[token.location.range]} #{value}"
  end
in "chap06_parsing"
  tokens = Lox.new.tokens(source)
  puts Lox.new.parse_expression(tokens).accept(Lox::Visitor::DebugVisitor.new)
in "chap07_evaluating"
  tokens = Lox.new.tokens(source)
  puts Lox.new.parse_expression(tokens).accept(Lox::Visitor::EvaluateVisitor.new)
in "chap08_statements"
  begin
    Lox.new.parse(source).accept(Lox::Visitor::EvaluateVisitor.new)
  rescue Lox::Error::SyntaxError => error
    warn(error.message)
    warn("[line #{source[0..error.location.start].count("\n") + 1}]")
    exit 65
  rescue Lox::Error::RuntimeError => error
    warn(error.message)
    warn("[line #{source[0..error.location.start].count("\n") + 1}]")
    exit 70
  end
end
