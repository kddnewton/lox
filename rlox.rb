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

  # A token is a representation of a single lexical unit in the source code.
  class Token
    attr_reader :type, :index, :length, :value

    def initialize(type:, value:, index:, length:)
      @type = type
      @value = value
      @index = index
      @length = length
    end

    def debug(source)
      "#{type} #{source[index...(index + length)]} #{value.nil? ? "null" : value}"
    end

    def deconstruct_keys(keys)
      { type: type, value: value, index: index, length: length }
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
      in :BANG_EQUAL | :EQUAL_EQUAL then EQUALITY
      in :GREATER | :GREATER_EQUAL | :LESS | :LESS_EQUAL then COMPARISON
      in :MINUS | :PLUS then TERM
      in :SLASH | :STAR then FACTOR
      else NONE
      end
    end
  end

  # This module contains the various types that are used in lox. In general we
  # avoid using native Ruby types because we can end up calling methods on them
  # that we don't want to support.
  module Type
    # This represents the false value.
    class False
      def to_lox
        "false"
      end
    end

    # This represents the null value.
    class Null
      def to_lox
        "null"
      end
    end

    # This represents a number. Lox makes no delineation between integers/
    # floats/bignums/etc.
    class Number
      include Comparable

      attr_reader :value

      def initialize(value:)
        @value = value
      end

      def deconstruct_keys(keys)
        { value: value }
      end

      def to_lox
        value % 1 == 0 ? value.to_i.to_s : value.to_s
      end

      #-------------------------------------------------------------------------
      # Runtime methods
      #-------------------------------------------------------------------------

      def -@
        Number.new(value: -value)
      end

      def +(other) = Number.new(value: value + number_value(other))
      def -(other) = Number.new(value: value - number_value(other))
      def *(other) = Number.new(value: value * number_value(other))
      def /(other) = Number.new(value: value / number_value(other))

      def <(other) = value < number_value(other) ? True.new : False.new
      def <=(other) = value <= number_value(other) ? True.new : False.new
      def >(other) = value > number_value(other) ? True.new : False.new
      def >=(other) = value >= number_value(other) ? True.new : False.new
      def ==(other) = value == number_value(other) ? True.new : False.new

      private

      def number_value(other)
        raise TypeError, "#{other.class} is not a Number" unless other in Number
        other.value
      end
    end

    # This represents a string.
    class String
      attr_reader :value

      def initialize(value:)
        @value = value
      end

      def deconstruct_keys(keys)
        { value: value }
      end

      def to_lox
        value.inspect
      end
    end

    # This represents the true value.
    class True
      def to_lox
        "true"
      end
    end
  end

  # This module contains the definitions of the nodes in the syntax tree, as
  # well as the methods to walk over them.
  module AST
    # A class that knows how to walk down the syntax tree.
    class Visitor
      def visit(node)
        node&.accept(self)
      end

      def visit_all(nodes)
        nodes.map { |node| visit(node) }
      end

      def visit_child_nodes(node)
        visit_all(node.child_nodes)
      end

      # Visit a Binary node.
      alias visit_binary visit_child_nodes

      # Visit a Group node.
      alias visit_group visit_child_nodes

      # Visit a Literal node.
      alias visit_literal visit_child_nodes

      # Visit a Unary node.
      alias visit_unary visit_child_nodes
    end

    # This is a visitor that will print the tree to a set of s-expressions that
    # match what the book's test suite expects.
    class DebugVisitor < Visitor
      # Visit a Binary node.
      def visit_binary(node)
        "(#{OPERATORS_NAMES[node.type]} #{visit(node.left)} #{visit(node.right)})"
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
        in Type::Null then "null"
        in Type::Number[value:] then value.inspect
        in Type::String[value:] then value.inspect
        end
      end

      # Visit a Unary node.
      def visit_unary(node)
        "(#{OPERATORS_NAMES[node.type]} #{visit(node.node)})"
      end
    end

    # This is a visitor that will walk the tree and evaluate it.
    class EvaluateVisitor < Visitor
      # Visit a Binary node.
      def visit_binary(node)
        case node.type
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
      end

      # Visit a Group node.
      def visit_group(node)
        visit(node.node)
      end

      # Visit a Literal node.
      def visit_literal(node)
        node.value
      end

      # Visit a Unary node.
      def visit_unary(node)
        case node.type
        in :BANG then !visit(node.node)
        in :MINUS then -visit(node.node)
        end
      end
    end

    # A binary node is a node that represents calling a binary operator between
    # two other nodes in the source. It contains an operator and both child
    # nodes.
    class Binary
      attr_reader :type, :left, :right

      def initialize(type:, left:, right:)
        @type = type
        @left = left
        @right = right
      end

      def accept(visitor)
        visitor.visit_binary(self)
      end

      def child_nodes
        [left, right]
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { type: type, left: left, right: right }
      end
    end

    # A group is a node that holds another node. It is used to represent the use
    # of parentheses in the source code.
    class Group
      attr_reader :node

      def initialize(node:)
        @node = node
      end

      def accept(visitor)
        visitor.visit_group(self)
      end

      def child_nodes
        [node]
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { node: node }
      end
    end

    # A literal is a node that holds a value from the Type module.
    class Literal
      attr_reader :value

      def initialize(value:)
        @value = value
      end

      def accept(visitor)
        visitor.visit_literal(self)
      end

      def child_nodes
        []
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { value: value }
      end
    end

    # A unary node is a node that represents calling a unary operator on another
    # node. It contains an operator and the child node.
    class Unary
      attr_reader :type, :node

      def initialize(type:, node:)
        @type = type
        @node = node
      end

      def accept(visitor)
        visitor.visit_unary(self)
      end

      def child_nodes
        [node]
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { type: type, node: node }
      end
    end
  end

  def lex(source) = tokens(source).map { _1.debug(source) }
  def parse(source) = parse_precedence(tokens(source), Precedence::ASSIGNMENT)

  private

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
          enum << Token.new(type: type, index: index, length: $&.length, value: nil)
        in /\A"([^"]*)"/
          enum << Token.new(type: :STRING, index: index, length: $&.length, value: $&[1...-1])
        in /\A\d+(\.\d+)?/
          enum << Token.new(type: :NUMBER, index: index, length: $&.length, value: $&.to_f)
        in /\A[a-z_][A-Za-z0-9_]*/
          type = KEYWORDS.include?($&) ? $&.upcase.to_sym : :IDENTIFIER
          enum << Token.new(type: type, index: index, length: $&.length, value: nil)
        end

        index += $&.length
      end

      enum << Token.new(type: :EOF, index: index, length: 0, value: nil)
    end
  end

  # This parses an expression at or above the current level of precedence. It is
  # an implementation of Pratt parsing.
  def parse_precedence(tokens, precedence)
    node =
      case tokens.next
      in { type: :BANG | :MINUS => type }
        AST::Unary.new(type: type, node: parse_precedence(tokens, Precedence::UNARY))
      in { type: :LEFT_PAREN }
        AST::Group.new(node: parse_precedence(tokens, Precedence::ASSIGNMENT)).tap do
          tokens.next => { type: :RIGHT_PAREN }
        end
      in { type: :TRUE }
        AST::Literal.new(value: Type::True.new)
      in { type: :FALSE }
        AST::Literal.new(value: Type::False.new)
      in { type: :NULL }
        AST::Literal.new(value: Type::Null.new)
      in { type: :NUMBER, value: }
        AST::Literal.new(value: Type::Number.new(value: value))
      in { type: :STRING, value: }
        AST::Literal.new(value: Type::String.new(value: value))
      end

    while (infix = Precedence.infix_for(tokens.peek)) && (precedence <= infix)
      token = tokens.next
      node = AST::Binary.new(type: token.type, left: node, right: parse_precedence(tokens, infix + 1))
    end

    node
  end
end

case ENV["SUITE"]
in "chap04_scanning" then puts Lox.new.lex(File.read(ARGV.first))
in "chap06_parsing" then puts Lox.new.parse(File.read(ARGV.first)).accept(Lox::AST::DebugVisitor.new)
in "chap07_evaluating" then puts Lox.new.parse(File.read(ARGV.first)).accept(Lox::AST::EvaluateVisitor.new).to_lox
end
