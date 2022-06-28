# frozen_string_literal: true

module Lox
  module Visitor
    # This is a visitor that will walk the tree and evaluate it.
    class Interpreter < BaseVisitor
      class Environment
        attr_reader :parent, :variables

        def initialize(parent: nil, variables: {})
          @parent = parent
          @variables = variables
        end

        def declare(name, value)
          variables[name] = value
        end

        def fetch(name, location)
          if variables.key?(name)
            variables[name]
          # elsif parent
            # parent.fetch(name, location)
          else
            raise Error::RuntimeError.new("Undefined variable '#{name}'.", location)
          end
        end

        def assign(name, value, location)
          if variables.key?(name)
            variables[name] = value
          # elsif parent
            # parent.assign(name, value, location)
          else
            raise Error::RuntimeError.new("Undefined variable '#{name}'.", location)
          end
        end
      end

      class LongJump < StandardError
        attr_reader :value

        def initialize(value)
          @value = value
          super("long-jump")
        end
      end

      attr_reader :globals, :environment, :locals, :errors

      def initialize
        variables = {
          "clock" => Type::Function.new(descriptor: "<native fn>", arity: 0, closure: environment) {
            Type::Number.new(value: Time.now.to_i / 1000)
          }
        }

        @globals = @environment = Environment.new(variables: variables)
        @locals = {}
        @errors = []
      end

      # This is a callback from the resolver.
      def resolve(node, depth)
        locals[node] = depth
      end

      # Visit an Assignment node.
      def visit_assignment(node)
        origin =
          if locals.key?(node.variable)
            current = environment
            locals[node.variable].times { current = current.parent }
            current
          else
            globals
          end

        origin.assign(node.variable.name, visit(node.value), node.location)
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
        push_frame do
          visit_all(node.statements)
          Type::Nil.instance
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
          callee.call(*arguments)
        end
      end

      # Visit an Expression node.
      def visit_expression(node)
        visit(node.value)
      end

      # Visit a ForStatement node.
      def visit_for_statement(node)
        visit(node.desugar)
      end

      # Visit a Function node.
      def visit_function(node)
        closure = environment
        closure.declare(
          node.name,
          Type::Function.new(descriptor: "<fn #{node.name}>", arity: node.parameters.size, closure: closure) do |*arguments|
            push_frame(closure) do |environment|
              node.parameters.zip(arguments).each do |(parameter, argument)|
                environment.declare(parameter.value, argument)
              end

              begin
                visit_all(node.statements)
                Type::Nil.instance
              rescue LongJump => jump
                jump.value
              end
            end
          end
        )

        Type::Nil.instance
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

      # Visit a ReturnStatement node.
      def visit_return_statement(node)
        raise LongJump, node.value ? visit(node.value) : Type::Nil.instance
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
        origin =
          if locals.key?(node)
            current = environment
            locals[node].times { current = current.parent }
            current
          else
            globals
          end

        origin.fetch(node.name, node.location)
      end

      # Visit a VariableDeclaration node.
      def visit_variable_declaration(node)
        environment.declare(node.name, node.initializer ? visit(node.initializer) : Type::Nil.instance)
      end

      # Visit a WhileStatement node.
      def visit_while_statement(node)
        while visit(node.condition).truthy?
          visit(node.body)
        end
        Type::Nil.instance
      end

      private

      def push_frame(parent = environment)
        current = @environment
        @environment = Environment.new(parent: parent)
        yield @environment
      ensure
        @environment = current
      end
    end
  end
end
