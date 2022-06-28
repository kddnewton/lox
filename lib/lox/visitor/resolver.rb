# frozen_string_literal: true

module Lox
  module Visitor
    # This visitor resolves all variable declarations and usages.
    class Resolver < BaseVisitor
      attr_reader :interpreter, :scopes, :function_type

      def initialize(interpreter)
        @interpreter = interpreter
        @scopes = []
        @function_type = :none
      end

      # Visit an Assignment node.
      def visit_assignment(node)
        visit(node.value)
        resolve_local(node.variable)
      end

      # Visit a BlockStatement node.
      def visit_block_statement(node)
        push_scope { visit_all(node.statements) }
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

        super
      end

      # Visit a Variable node.
      def visit_variable(node)
        if scopes.any? && (scopes.last[node.name] == false)
          interpreter.errors << Error::SyntaxError.new("Error at '#{node.name}': Can't read local variable in its own initializer.", node.location)
        end

        resolve_local(node)
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

      def resolve_local(node)
        scopes.reverse_each.each_with_index do |scope, index|
          if scope.key?(node.name)
            interpreter.resolve(node, index)
            return
          end
        end
      end

      def push_scope
        scopes << {}
        yield
        scopes.pop
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