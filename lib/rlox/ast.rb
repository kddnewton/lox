# frozen_string_literal: true

module Lox
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
      attr_reader :type, :location, :value

      def initialize(type:, location:, value:)
        @type = type
        @location = location
        @value = value
      end

      def deconstruct_keys(keys)
        { type: type, location: location, value: value }
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

    # This is a very special node in the tree that represents when a node was
    # expected but was not found.
    class Missing
      attr_reader :location

      def initialize(location:)
        @location = location
      end

      def accept(visitor)
        visitor.visit_missing(self)
      end

      def child_nodes
        []
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { location: location }
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
end
