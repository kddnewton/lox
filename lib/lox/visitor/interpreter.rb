# frozen_string_literal: true

module Lox
  module Visitor
    # This is a visitor that will walk the tree and evaluate it.
    class Interpreter < BaseVisitor
      class Environment
        attr_reader :parent, :variables, :functions

        def initialize(parent: nil, functions: {})
          @parent = parent
          @variables = {}
          @functions = functions
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

      class Function
        attr_reader :arity, :callable

        def initialize(arity:, &callable)
          @arity = arity
          @callable = callable
        end

        def call(*arguments)
          callable.call(*arguments)
        end
      end

      attr_reader :environment

      def initialize
        functions = {
          clock: Function.new(arity: 0) { Type::Number.new(value: Time.now.to_i / 1000) }
        }

        @environment = Environment.new(functions: functions)
      end

      # Visit an Assignment node.
      def visit_assignment(node)
        environment.assign(node.variable, visit(node.value))
      end

      # Visit a Binary node.
      def visit_binary(node)
        case node.operator.type
        in :AND
          left = visit(node.left)
          left.truthy? ? visit(node.right) : left
        in :BANG_EQUAL
          visit(node.left) != visit(node.right)
        in :EQUAL_EQUAL
          visit(node.left) == visit(node.right)
        in :GREATER
          visit(node.left) > visit(node.right)
        in :GREATER_EQUAL
          visit(node.left) >= visit(node.right)
        in :LESS
          visit(node.left) < visit(node.right)
        in :LESS_EQUAL
          visit(node.left) <= visit(node.right)
        in :MINUS
          visit(node.left) - visit(node.right)
        in :OR
          left = visit(node.left)
          left.truthy? ? left : visit(node.right)
        in :PLUS
          visit(node.left) + visit(node.right)
        in :SLASH
          visit(node.left) / visit(node.right)
        in :STAR
          visit(node.left) * visit(node.right)
        end
      rescue Error::RuntimeError => error
        raise Error::RuntimeError.new(error.message, node.operator.location)
      end

      # Visit a BlockStatement node.
      def visit_block_statement(node)
        parent = environment
        @environment = Environment.new(parent: parent)

        begin
          visit_all(node.statements)
          Type::Nil.instance
        ensure
          @environment = parent
        end
      end

      # Visit a Call node.
      def visit_call(node)
        callee = visit(node.callee)
        arguments = node.arguments.map { |argument| visit(argument) }

        if !callee.callable?
          raise Error::RuntimeError.new("Can only call functions and classes.", node.location)
        elsif callee.arity != arguments.size
          raise Error::RuntimeError.new("Expected #{callee.arity} arguments but got #{arguments.size}.", node.arguments_location)
        else
          callee.call(self, arguments)
        end
      end

      # Visit an Expression node.
      def visit_expression(node)
        visit(node.value)
      end

      # Visit a ForStatement node.
      def visit_for_statement(node)
        body = node.body

        if node.increment
          body =
            AST::BlockStatement.new(
              statements: [node.body, node.increment],
              location: node.body.location.to(node.increment.location)
            )
        end

        desugared =
          AST::WhileStatement.new(
            condition: node.condition || AST::Literal.new(value: Type::True.instance),
            body: body,
            location: node.location
          )

        if node.initializer
          desugared =
            AST::BlockStatement.new(
              statements: [node.initializer, desugared],
              location: node.initializer.location.to(desugared.location)
            )
        end

        visit(desugared)
      end

      # Visit a Group node.
      def visit_group(node)
        visit(node.node)
      end

      # Visit an IfStatement node.
      def visit_if_statement(node)
        if visit(node.condition).truthy?
          visit(node.then_branch)
        elsif node.else_branch
          visit(node.else_branch)
        end
        Type::Nil.instance
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

      # Visit a WhileStatement node.
      def visit_while_statement(node)
        while visit(node.condition).truthy?
          visit(node.body)
        end
        Type::Nil.instance
      end
    end
  end
end
