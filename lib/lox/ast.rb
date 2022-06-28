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

      def to_value_s
        case type
        in :NUMBER
          "'#{Lox::Type::Number.new(value: value).to_lox}'"
        in :EOF
          "end"
        else
          "'#{value}'"
        end
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

    # This represents a function call.
    class Call
      attr_reader :callee, :arguments, :arguments_location, :location

      def initialize(callee:, arguments:, arguments_location:, location:)
        @callee = callee
        @arguments = arguments
        @arguments_location = arguments_location
        @location = location
      end

      def accept(visitor)
        visitor.visit_call(self)
      end

      def child_nodes
        [callee, *arguments]
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { callee: callee, arguments: arguments, arguments_location: arguments_location, location: location }
      end
    end

    # This represents a class declaration.
    class ClassStatement
      attr_reader :name, :methods, :location

      def initialize(name:, methods:, location:)
        @name = name
        @methods = methods
        @location = location
      end

      def accept(visitor)
        visitor.visit_class_statement(self)
      end

      def child_nodes
        methods
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { name: name.value, methods: methods, location: location }
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

    # This represents a for loop.
    class ForStatement
      attr_reader :initializer, :condition, :increment, :body, :location

      def initialize(initializer:, condition:, increment:, body:, location:)
        @initializer = initializer
        @condition = condition
        @increment = increment
        @body = body
        @location = location
      end

      def accept(visitor)
        visitor.visit_for_statement(self)
      end

      def child_nodes
        [initializer, condition, increment, body]
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { initializer: initializer, condition: condition, increment: increment, body: body, location: location }
      end

      def desugar
        statements = body

        if increment
          statements =
            AST::BlockStatement.new(
              statements: [body, increment],
              location: body.location.to(increment.location)
            )
        end

        desugared =
          AST::WhileStatement.new(
            condition: condition || AST::Literal.new(value: Type::True.instance, location: location),
            body: statements,
            location: location
          )

        if initializer
          desugared =
            AST::BlockStatement.new(
              statements: [initializer, desugared],
              location: initializer.location.to(desugared.location)
            )
        end

        desugared
      end
    end

    # This represents a function declaration.
    class Function
      attr_reader :name, :parameters, :statements, :location

      def initialize(name:, parameters:, statements:, location:)
        @name = name
        @parameters = parameters
        @statements = statements
        @location = location
      end

      def accept(visitor)
        visitor.visit_function(self)
      end

      def child_nodes
        [*parameters, *statements]
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { name: name, parameters: parameters.map(&:name), statements: statements, location: location }
      end
    end

    # This represents accessing a member of an object.
    class GetExpression
      attr_reader :object, :name, :location

      def initialize(object:, name:, location:)
        @object = object
        @name = name
        @location = location
      end

      def accept(visitor)
        visitor.visit_get_expression(self)
      end

      def child_nodes
        [object]
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { object: object, name: name.value, location: location }
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

    # An if statement with an optional else clause.
    class IfStatement
      attr_reader :condition, :then_branch, :else_branch, :location

      def initialize(condition:, then_branch:, else_branch:, location:)
        @condition = condition
        @then_branch = then_branch
        @else_branch = else_branch
        @location = location
      end

      def accept(visitor)
        visitor.visit_if_statement(self)
      end

      def child_nodes
        [condition, then_branch, else_branch]
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { condition: condition, then_branch: then_branch, else_branch: else_branch, location: location }
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

    # This represents a return statement.
    class ReturnStatement
      attr_reader :value, :location

      def initialize(value:, location:)
        @value = value
        @location = location
      end

      def accept(visitor)
        visitor.visit_return_statement(self)
      end

      def child_nodes
        [value]
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { value: value, location: location }
      end
    end

    # This represents setting a member of an object.
    class SetExpression
      attr_reader :object, :name, :value, :location

      def initialize(object:, name:, value:, location:)
        @object = object
        @name = name
        @value = value
        @location = location
      end

      def accept(visitor)
        visitor.visit_set_expression(self)
      end

      def child_nodes
        [value, object]
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { object: object, name: name.value, value: value, location: location }
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

    # This represents a while statement.
    class WhileStatement
      attr_reader :condition, :body, :location

      def initialize(condition:, body:, location:)
        @condition = condition
        @body = body
        @location = location
      end

      def accept(visitor)
        visitor.visit_while_statement(self)
      end

      def child_nodes
        [condition, body]
      end

      alias deconstruct child_nodes

      def deconstruct_keys(keys)
        { condition: condition, body: body, location: location }
      end
    end
  end
end
