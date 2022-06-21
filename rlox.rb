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
  class Token < Struct.new(:type, :index, :length, :value, keyword_init: true)
    def debug(source) = "#{type} #{source[index...(index + length)]} #{value.nil? ? "null" : value}"
  end

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
      case node.type
      in :TRUE | :FALSE | :NULL then node.type.downcase
      in :STRING | :NUMBER then node.value.inspect
      end
    end

    # Visit a Unary node.
    def visit_unary(node)
      "(#{OPERATORS_NAMES[node.type]} #{visit(node.node)})"
    end
  end

  # This is the parent node of all of the nodes in the syntax tree. It provides
  # common functionality like pretty printing.
  class Node
    def debug
      DebugVisitor.new.visit(self)
    end
  end

  # A binary node is a node that represents calling a binary operator between
  # two other nodes in the source. It contains an operator and both child nodes.
  class Binary < Node
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
  class Group < Node
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

  # A literal is a node that uses the running language's (Ruby) type system to
  # represent a value in the lox language. Currently this means it holds
  # booleans, nulls, strings, and numbers.
  class Literal < Node
    attr_reader :type, :value

    def initialize(type:, value:)
      @type = type
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
      { type: type, value: value }
    end
  end

  # A unary node is a node that represents calling a unary operator on another
  # node. It contains an operator and the child node.
  class Unary < Node
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

  def lex(source) = tokens(source).map { _1.debug(source) }
  def parse(source) = expression(tokens(source)).debug

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
          enum << Token.new(type:, index:, length: $&.length)
        in /\A"([^"]*)"/
          enum << Token.new(type: :STRING, index:, length: $&.length, value: $&[1...-1])
        in /\A\d+(\.\d+)?/
          enum << Token.new(type: :NUMBER, index:, length: $&.length, value: $&.to_f)
        in /\A[a-z_][A-Za-z0-9_]*/
          type = KEYWORDS.include?($&) ? $&.upcase.to_sym : :IDENTIFIER
          enum << Token.new(type:, index:, length: $&.length)
        end

        index += $&.length
      end

      enum << Token.new(type: :EOF, index:, length: 0)
    end
  end

  def binary(enum, types, &parse)
    node = parse.call(enum)
    while types.include?(enum.peek.type)
      node = Binary.new(type: enum.next.type, left: node, right: parse.call(enum))
    end
  
    node
  end

  def expression(enum) = equality(enum)
  def equality(enum) = binary(enum, %i[BANG_EQUAL EQUAL_EQUAL], &method(:comparison))
  def comparison(enum) = binary(enum, %i[GREATER GREATER_EQUAL LESS LESS_EQUAL], &method(:term))
  def term(enum) = binary(enum, %i[MINUS PLUS], &method(:factor))
  def factor(enum) = binary(enum, %i[SLASH STAR], &method(:unary))

  def unary(enum)
    if enum.peek in { type: :BANG | :MINUS => type }
      Unary.new(type:, node: unary(enum.tap(&:next)))
    else
      primary(enum)
    end
  end

  def primary(enum)
    case enum.next
    in { type: :TRUE | :FALSE => type, value: } then Literal.new(type:, value: type == :TRUE)
    in { type: :NULL | :STRING | :NUMBER => type, value: } then Literal.new(type:, value:)
    in { type: :LEFT_PAREN }
      Group.new(node: expression(enum).tap { enum.next => { type: :RIGHT_PAREN } })
    end
  end
end

case ENV["SUITE"]
when "chap04_scanning" then puts Lox.new.lex(File.read(ARGV.first))
when "chap06_parsing" then puts Lox.new.parse(File.read(ARGV.first))
end
