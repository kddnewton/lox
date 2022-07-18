# frozen_string_literal: true

module Lox
  module Visitor
    # This visitor resolves all variable declarations and usages.
    class Resolver < BaseVisitor
      attr_reader :interpreter, :scopes, :class_type, :function_type

      def initialize(interpreter)
        @interpreter = interpreter
        @scopes = []

        @class_type = :none
        @function_type = :none
      end

      # Visit an Assignment node.
      def visit_assignment(node)
        visit(node.value)
        resolve_local(node.variable, node.variable.name)
      end

      # Visit a BlockStatement node.
      def visit_block_statement(node)
        push_scope { visit_all(node.statements) }
      end

      # Visit a ClassStatement node.
      def visit_class_statement(node)
        with_class_type(:class) do
          declare(node.name.value, node.location)
          define(node.name.value)

          if node.superclass && node.name.value == node.superclass.name
            interpreter.errors << Error::SyntaxError.new("Error at '#{node.name.value}': A class can't inherit from itself.", node.location)
          end

          push_scope do |scope|
            scope[:this] = true

            node.methods.each do |method|
              resolve_function(method, method.name == "init" ? :initializer : :method)
            end
          end
        end
      end

      # Visit a ForStatement node.
      def visit_for_statement(node)
        visit(node.desugar)
      end

      # Visit a Function node.
      def visit_function(node)
        declare(node.name, node.location)
        define(node.name)
        resolve_function(node, :function)
      end

      # Visit a ReturnStatement node.
      def visit_return_statement(node)
        if function_type == :none
          interpreter.errors << Error::SyntaxError.new("Error at 'return': Can't return from top-level code.", node.location)
        end

        if node.value && function_type == :initializer
          interpreter.errors << Error::SyntaxError.new("Error at 'return': Can't return a value from an initializer.", node.location)
        end

        super
      end

      # Visit a ThisExpression node.
      def visit_this_expression(node)
        if class_type == :none
          interpreter.errors << Error::SyntaxError.new("Error at 'this': Can't use 'this' outside of a class.", node.location)
        else
          resolve_local(node, :this)
        end
      end

      # Visit a Variable node.
      def visit_variable(node)
        if scopes.any? && (scopes.last[node.name] == false)
          interpreter.errors << Error::SyntaxError.new("Error at '#{node.name}': Can't read local variable in its own initializer.", node.location)
        end

        resolve_local(node, node.name)
      end

      # Visit a VariableDeclaration node.
      def visit_variable_declaration(node)
        declare(node.name, node.location)
        visit(node.initializer) if node.initializer
        define(node.name)
      end

      private

      def declare(name, location)
        return if scopes.empty?

        scope = scopes.last
        if scope.key?(name)
          interpreter.errors << Error::SyntaxError.new("Error at '#{name}': Already a variable with this name in this scope.", location)
        end

        scope[name] = false
      end

      def define(name)
        scopes.last[name] = true if scopes.any?
      end

      def resolve_function(node, function_type)
        with_function_type(function_type) do
          push_scope do
            node.parameters.each do |param|
              declare(param.value, param.location)
              define(param.value)
            end

            visit_all(node.statements)
          end
        end
      end

      def resolve_local(node, name)
        scopes.reverse_each.each_with_index do |scope, index|
          if scope.key?(name)
            interpreter.resolve(node, index)
            return
          end
        end
      end

      def push_scope
        scope = {}
        scopes << scope
        yield scope
        scopes.pop
      end

      def with_class_type(class_type)
        current = class_type
        @class_type = class_type

        yield
      ensure
        @class_type = current
      end

      def with_function_type(function_type)
        current = function_type
        @function_type = function_type

        yield
      ensure
        @function_type = current
      end
    end
  end
end
