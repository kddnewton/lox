# frozen_string_literal: true

module Lox
  module Visitor
    # This is a visitor that will print the tree to a set of s-expressions that
    # match what the book's test suite expects.
    class DebugVisitor < BaseVisitor
      # In debug methods, it's necessary to go from the operator to the value
      # that it represents in source.
      OPERATORS_NAMES = Lexer::OPERATORS.invert

      attr_reader :q

      def initialize(q)
        @q = q
      end

      # Visit a Binary node.
      def visit_binary(node)
        group(OPERATORS_NAMES[node.operator.type]) do
          q.breakable
          visit(node.left)
          q.breakable
          visit(node.right)
        end
      end

      # Visit a Call node.
      def visit_call(node)
        group("call") do
          q.breakable
          visit(node.callee)

          if node.arguments.any?
            q.breakable
            q.seplist(node.arguments) { |argument| visit(argument) }
          end
        end
      end

      # Visit a ForStatement node.
      def visit_for_statement(node)
        group("for") do
          q.breakable
          visit(node.initializer)

          q.breakable
          visit(node.condition)

          q.breakable
          visit(node.increment)

          q.breakable
          visit(node.body)
        end
      end

      # Visit a Group node.
      def visit_group(node)
        group("group") do
          q.breakable
          visit(node.node)
        end
      end

      # Visit an IfStatement node.
      def visit_if_statement(node)
        group("if-statement") do
          q.breakable
          visit(node.condition)
          q.breakable
          visit(node.then_branch)

          if node.else_branch
            q.breakable
            visit(node.else_branch)
          end
        end
      end

      # Visit a Literal node.
      def visit_literal(node)
        if node.value in Type::Number
          lox = node.value.to_lox
          q.text(lox.include?(".") ? lox : "#{lox}.0")
        else
          q.text(node.value.to_lox)
        end
      end

      # Visit a Missing node.
      def visit_missing(node)
        q.text("(missing)")
      end

      # Visit a Unary node.
      def visit_unary(node)
        group(OPERATORS_NAMES[node.operator.type]) do
          q.breakable
          visit(node.node)
        end
      end

      # Visit a WhileStatement node.
      def visit_while_statement(node)
        group("while-statement") do
          q.breakable
          visit(node.condition)
          q.breakable
          visit(node.body)
        end
      end

      private

      def group(name)
        q.group do
          q.text("(#{name}")
          q.nest(2) do
            yield
          end
          q.breakable("")
          q.text(")")
        end
      end
    end
  end
end
