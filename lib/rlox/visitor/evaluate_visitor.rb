# frozen_string_literal: true

module Lox
  module Visitor
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
          Type::Nil.instance
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

      # Visit a Missing node.
      def visit_missing(node)
        raise
      end

      # Visit a Print node.
      def visit_print_statement(node)
        puts visit(node.value).to_lox
        Type::Nil.instance
      end

      # Visit a Program node.
      def visit_program(node)
        visit_all(node.statements)
        Type::Nil.instance
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
        environment.declare(node, node.initializer ? visit(node.initializer) : Type::Nil.instance)
      end
    end
  end
end
